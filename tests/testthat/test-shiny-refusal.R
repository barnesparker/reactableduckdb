# What refusal actually looks like through Shiny's HTTP layer, observed
# end-to-end: a real app (callr), a real browser session (chromote) so the
# genuine JS client issues requests, then crafted POSTs to the session's
# dataobj URL (httr2). Observed behaviour is recorded in
# design/reactable-server-contract.md section 12.

start_refusal_app <- function(env = parent.frame()) {
  port <- httpuv::randomPort()
  stderr_file <- withr::local_tempfile(.local_envir = env)
  proc <- callr::r_bg(
    function(port, pkg_dir) {
      tryCatch(
        library(reactableduckdb),
        error = function(e) pkgload::load_all(pkg_dir, quiet = TRUE)
      )
      library(shiny)
      con1 <- DBI::dbConnect(duckdb::duckdb())
      con2 <- DBI::dbConnect(duckdb::duckdb())
      for (con in list(con1, con2)) {
        DBI::dbExecute(con, "
          CREATE TABLE t AS
          SELECT i AS id, 'name_' || (i % 97) AS name,
                 (i % 100)::INTEGER AS qty
          FROM range(2000) r(i)")
      }
      b1 <- reactable_duckdb_backend(dplyr::tbl(con1, "t"), key = "id")
      b2 <- reactable_duckdb_backend(dplyr::tbl(con2, "t"), key = "id")
      ui <- fluidPage(
        reactable::reactableOutput("tbl"),
        reactable::reactableOutput("tbl2"),
        actionButton("kill", "kill")
      )
      server <- function(input, output, session) {
        output$tbl <- reactable::renderReactable(
          reactable_duckdb(b1, filterable = TRUE, defaultPageSize = 5)
        )
        output$tbl2 <- reactable::renderReactable(
          reactable_duckdb(b2, filterable = TRUE, defaultPageSize = 5)
        )
        observeEvent(input$kill, DBI::dbDisconnect(con2, shutdown = TRUE))
      }
      runApp(shinyApp(ui, server), port = port, launch.browser = FALSE)
    },
    args = list(port = port, pkg_dir = normalizePath(testthat::test_path("..", ".."))),
    stderr = stderr_file
  )
  withr::defer(proc$kill(), envir = env)
  base_url <- sprintf("http://127.0.0.1:%d", port)
  for (i in 1:100) {
    up <- tryCatch(
      httr2::resp_status(httr2::req_perform(httr2::request(base_url))) == 200,
      error = function(e) FALSE
    )
    if (up) break
    if (!proc$is_alive()) stop("app process died: ", paste(readLines(stderr_file, warn = FALSE), collapse = "\n"))
    Sys.sleep(0.2)
  }
  if (!up) stop("app did not come up")
  list(proc = proc, base_url = base_url, stderr_file = stderr_file)
}

post_json <- function(url, body) {
  resp <- httr2::request(url) |>
    httr2::req_body_raw(body, type = "application/json") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()
  list(
    status = httr2::resp_status(resp),
    type = httr2::resp_content_type(resp),
    body = httr2::resp_body_string(resp)
  )
}

test_that("refusal through Shiny is HTTP 500 with server-side logging", {
  skip_on_cran()
  skip_if_no_backend_api()
  for (pkg in c("shiny", "callr", "httr2", "chromote", "pkgload")) {
    skip_if_not_installed(pkg)
  }
  browser <- tryCatch(chromote::ChromoteSession$new(), error = function(e) NULL)
  if (is.null(browser)) skip("chromote could not start a Chrome session")
  withr::defer(browser$close())

  app <- start_refusal_app()

  captured <- list()
  browser$Network$enable()
  browser$Network$requestWillBeSent(function(msg) {
    if (grepl("dataobj", msg$request$url, fixed = TRUE)) {
      captured[[length(captured) + 1]] <<- msg$request
    }
  })
  browser$Page$navigate(app$base_url)
  rendered <- FALSE
  for (i in 1:100) {
    n <- browser$Runtime$evaluate("document.querySelectorAll('.rt-tr-group').length")$result$value
    if (!is.null(n) && n > 0) {
      rendered <- TRUE
      break
    }
    Sys.sleep(0.2)
  }
  expect_true(rendered)

  # A real user interaction: the genuine JS client sends the next-page
  # request. This is the empirical record of the client request shape.
  browser$Runtime$evaluate("document.querySelectorAll('#tbl .rt-next-button')[0].click()")
  for (i in 1:50) {
    urls <- vapply(captured, function(r) r$url, character(1))
    if (any(grepl("dataobj/tbl?", urls, fixed = TRUE))) break
    Sys.sleep(0.2)
  }
  tbl_requests <- captured[grepl("dataobj/tbl?", vapply(captured, function(r) r$url, character(1)), fixed = TRUE)]
  expect_gt(length(tbl_requests), 0)
  client_body <- jsonlite::fromJSON(tbl_requests[[length(tbl_requests)]]$postData, simplifyVector = FALSE)
  # Observed client shape: pageIndex/pageSize numbers; sortBy/filters/groupBy
  # arrays; expanded/selectedRowIds objects.
  expect_contains(
    names(client_body),
    c("pageIndex", "pageSize", "sortBy", "filters", "groupBy")
  )
  expect_type(client_body$pageIndex, "integer")

  tbl1_url <- paste0(
    app$base_url,
    sub("^.*(/session/)", "\\1", tbl_requests[[length(tbl_requests)]]$url)
  )
  tbl2_url <- sub("dataobj/tbl?", "dataobj/tbl2?", tbl1_url, fixed = TRUE)

  # Control: a valid crafted request answers 200 with resolvedData JSON.
  ok <- post_json(tbl1_url, '{"pageIndex":1,"pageSize":5}')
  expect_identical(ok$status, 200L)
  expect_match(ok$type, "application/json")
  expect_contains(names(jsonlite::fromJSON(ok$body)), c("data", "rowCount"))

  # Unknown-column filter: HTTP 500, an HTML error page. The widget in the
  # browser keeps its previous rows — refusal is silent for the end user,
  # which is why the server-side log matters.
  refused <- post_json(
    tbl1_url,
    '{"pageIndex":0,"pageSize":5,"filters":[{"id":"nope","value":"xsecretx"}]}'
  )
  expect_identical(refused$status, 500L)
  expect_match(refused$type, "text/html")
  expect_match(refused$body, "error has occurred", ignore.case = TRUE)

  # Oversized page request: same surface.
  oversized <- post_json(tbl1_url, '{"pageIndex":0,"pageSize":9999}')
  expect_identical(oversized$status, 500L)

  # Invalidated connection: kill con2 in the live app, then a previously
  # valid request 500s.
  browser$Runtime$evaluate("document.getElementById('kill').click()")
  Sys.sleep(1)
  dead <- post_json(tbl2_url, '{"pageIndex":0,"pageSize":5}')
  expect_identical(dead$status, 500L)

  # The log-and-rethrow wrapper put the story in the app's stderr: our
  # structured lines with the schema hash and ids-only request summary.
  Sys.sleep(0.5)
  logs <- paste(readLines(app$stderr_file, warn = FALSE), collapse = "\n")
  expect_match(logs, "reactableduckdb request failed")
  expect_match(logs, "Unknown filter column", fixed = TRUE)
  expect_match(logs, "filter_ids=[nope]", fixed = TRUE)
  expect_match(logs, "no longer valid")
  # Filter values never reach the logs.
  expect_no_match(logs, "xsecretx")
})
