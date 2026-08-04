# Row identity and row selection.
#
# The browser test at the bottom is load-bearing, not a nicety. Row identity
# depends on `__state`, an undocumented extension point in reactable's
# JavaScript client. If it is renamed or dropped upstream, every R-level test
# here still passes while the client silently reverts to identifying rows by
# their position in the delivered page — selection then marks rows the user
# never chose, with no error anywhere. Only a real browser catches that.
# Observed behaviour is recorded in design/selection-contract.md.

# Key round-tripping ----------------------------------------------------------

test_that("row ids round-trip exactly for every supported key kind", {
  expect_identical(selection_key_kind(c("a", "b")), "character")
  expect_identical(selection_key_kind(1L), "integer")
  expect_identical(selection_key_kind(1.5), "double")
  expect_identical(selection_key_kind(Sys.Date()), NA_character_)
  expect_identical(selection_key_kind(list(1)), NA_character_)

  chr <- c("a", "b b", "c'd")
  expect_identical(
    parse_row_ids(format_row_ids(chr, "character"), "character"),
    chr
  )

  int <- c(1L, -5L, 2147483647L)
  expect_identical(
    parse_row_ids(format_row_ids(int, "integer"), "integer"),
    int
  )

  # DuckDB BIGINT reaches R as a double, so whole-number doubles are the
  # ordinary integer-key case and must be exact right up to 2^53.
  dbl <- c(0, 1, -7, 2^53 - 1, -(2^53 - 1))
  expect_identical(parse_row_ids(format_row_ids(dbl, "double"), "double"), dbl)
  # Plain digits, never scientific notation: a row id has to be a stable
  # string, not whatever R's print heuristics choose for the magnitude.
  expect_match(format_row_ids(dbl, "double"), "^-?[0-9]+$")
})

test_that("a double key that is not whole-number-valued is refused", {
  # No decimal float format round-trips every double through R's as.numeric(),
  # so fractional keys are refused rather than silently mismatched.
  expect_snapshot(error = TRUE, format_row_ids(c(1, 2.5), "double"))
  expect_snapshot(error = TRUE, format_row_ids(2^53 + 2, "double"))
  expect_snapshot(error = TRUE, format_row_ids(c(1, Inf), "double"))
  # NAs pass through as NA ids; attach_row_state() is what refuses them.
  expect_identical(format_row_ids(c(1, NA_real_), "double"), c("1", NA))
})

test_that("unreadable row ids are refused, never dropped", {
  expect_snapshot(error = TRUE, parse_row_ids("abc", "double"))
  expect_snapshot(error = TRUE, parse_row_ids(c("1", "x"), "integer"))
  expect_identical(parse_row_ids(character(0), "character"), character(0))
})

# Row state on the page -------------------------------------------------------

test_that("a keyed page carries row identity and a global index", {
  skip_if_no_backend_api()
  b <- local_backend(n = 200, key = "id")
  page <- source_page(b, b$source, NULL, 3, 10)
  expect_contains(names(page), "__state")
  state <- page[["__state"]]
  expect_identical(names(state), c("id", "index"))
  # The index is the row's position in the whole filtered result, not in the
  # page: that is what getReactableState() reports back to the app.
  expect_identical(state$index, 30 + seq_len(10) - 1)
  expect_identical(state$id, format_row_ids(page$id, b$key_kind))
  expect_identical(anyDuplicated(state$id), 0L)
})

test_that("a keyless backend gets no row state at all", {
  skip_if_no_backend_api()
  b <- local_backend(n = 50)
  # A `__state` column without an `id` would leave getRowId() undefined --
  # worse than the page-index fallback, so nothing is attached.
  expect_no_match(
    names(source_page(b, b$source, NULL, 0, 5)),
    "__state",
    fixed = TRUE
  )
})

test_that("a source column named __state is refused", {
  skip_if_no_backend_api()
  con <- local_duckdb_con()
  DBI::dbExecute(con, 'CREATE TABLE clash AS SELECT 1 AS id, 2 AS "__state"')
  expect_snapshot(
    error = TRUE,
    reactable_duckdb_backend(dplyr::tbl(con, "clash"), key = "id")
  )
})

# Constructor refusals --------------------------------------------------------

test_that("selection requires a key of a round-trippable type", {
  skip_if_no_backend_api()
  expect_snapshot(
    error = TRUE,
    reactable_duckdb(local_backend(n = 50), selection = "multiple")
  )
  con <- local_duckdb_con()
  DBI::dbExecute(
    con,
    "CREATE TABLE dated AS SELECT DATE '2020-01-01' + i::INTEGER AS d FROM range(5) r(i)"
  )
  b <- reactable_duckdb_backend(dplyr::tbl(con, "dated"), key = "d")
  # A Date is a fine sort tie-breaker, so the backend builds...
  expect_identical(b$key, "d")
  # ...but it cannot be recovered exactly from a string row id.
  expect_snapshot(error = TRUE, reactable_duckdb(b, selection = "single"))
})

test_that("index-based selection arguments are refused", {
  skip_if_no_backend_api()
  b <- local_backend(n = 100, key = "id")
  # Both address rows by index. Our ids are key values, so an index would
  # silently mean "the row whose key is N" -- a different row, no error.
  expect_snapshot(
    error = TRUE,
    reactable_duckdb(b, selection = "multiple", defaultSelected = c(1, 2))
  )
  expect_snapshot(
    error = TRUE,
    reactable_duckdb(b, selection = "multiple", selectionId = "sel")
  )
  # Without `selection` we do not intercept them; reactable applies its own
  # rules, which is what should happen for arguments that are not ours.
  expect_snapshot(error = TRUE, reactable_duckdb(b, defaultSelected = c(1, 2)))
})

test_that("the select-all header is neutralized for multiple selection only", {
  skip_if_no_backend_api()
  b <- local_backend(n = 100, key = "id")
  sel_col <- function(w) {
    found <- Filter(
      function(c) identical(c$id, ".selection"),
      w$x$tag$attribs$columns
    )
    if (length(found) == 0) NULL else found[[1]]
  }
  # "multiple" renders a select-all checkbox that can only ever cover the
  # delivered page, so it is hidden and made unclickable.
  multi <- sel_col(reactable_duckdb(b, selection = "multiple"))
  expect_identical(
    multi$headerStyle,
    list(visibility = "hidden", pointerEvents = "none")
  )
  # "single" renders radio buttons and no select-all, so nothing to suppress.
  expect_null(sel_col(reactable_duckdb(b, selection = "single"))$headerStyle)
  # A caller who understands the semantics can put it back.
  restored <- sel_col(reactable_duckdb(
    b,
    selection = "multiple",
    columns = list(
      .selection = reactable::colDef(headerStyle = list(color = "red"))
    )
  ))
  expect_identical(restored$headerStyle, list(color = "red"))
})

# Selection -> rows -----------------------------------------------------------

test_that("selected keys resolve to a lazy query, never a materialization", {
  skip_if_no_backend_api()
  b <- local_backend(n = 200, key = "id")
  reactableduckdb:::enable_query_log(b)

  q <- selection_subset(b, c("3", "7", "11"))
  expect_s3_class(q, "tbl_lazy")
  # Building the query runs nothing.
  expect_length(b$state$query_log, 0)
  expect_identical(sort(dplyr::collect(q)$id), c(3, 7, 11))

  # No selection matches no rows -- and still without materializing.
  empty <- selection_subset(b, character(0))
  expect_s3_class(empty, "tbl_lazy")
  expect_identical(nrow(dplyr::collect(empty)), 0L)
})

test_that("the selection registry is per session and replaces, not merges", {
  skip_if_no_backend_api()
  skip_if_not_installed("shiny")
  b <- local_backend(n = 50, key = "id")
  # record_selection() is a no-op with no session, so a bare data request
  # cannot leak one session's selection into a shared backend.
  record_selection(b, list(`1` = TRUE))
  expect_length(b$state$selection, 0)

  shiny::withReactiveDomain(shiny::MockShinySession$new(), {
    record_selection(b, list(`3` = TRUE, `9` = TRUE))
    token <- shiny::getDefaultReactiveDomain()$token
    expect_identical(b$state$selection[[token]]$keys, c("3", "9"))
    # The client re-sends the whole selection every time, so a later request
    # replaces the set rather than adding to it.
    record_selection(b, list(`9` = TRUE))
    expect_identical(b$state$selection[[token]]$keys, "9")
    record_selection(b, NULL)
    expect_identical(b$state$selection[[token]]$keys, character(0))
  })
})

test_that("selection accessors refuse outside a session or without a key", {
  skip_if_no_backend_api()
  skip_if_not_installed("shiny")
  b <- local_backend(n = 50, key = "id")
  expect_snapshot(error = TRUE, reactable_duckdb_selected_keys(b))
  keyless <- local_backend(n = 50)
  shiny::withReactiveDomain(shiny::MockShinySession$new(), {
    expect_snapshot(error = TRUE, reactable_duckdb_selected_keys(keyless))
  })
})

# End-to-end in a real browser ------------------------------------------------

test_that("selection follows rows across pages in a real browser", {
  skip_on_cran()
  skip_if_no_backend_api()
  for (pkg in c("shiny", "callr", "httr2", "chromote", "pkgload")) {
    skip_if_not_installed(pkg)
  }
  pkg_dir <- skip_without_source_tree()
  browser <- tryCatch(chromote::ChromoteSession$new(), error = function(e) NULL)
  if (is.null(browser)) skip("chromote could not start a Chrome session")
  withr::defer(browser$close())

  port <- httpuv::randomPort()
  stderr_file <- withr::local_tempfile()
  proc <- callr::r_bg(
    function(port, pkg_dir) {
      # A fresh R session, so it loads the package itself -- unconditionally
      # from the source tree, never from the library. The assertion turns a
      # stale copy into a loud failure rather than a wrong result.
      pkgload::load_all(pkg_dir, quiet = TRUE)
      stopifnot(pkgload::is_dev_package("reactableduckdb"))
      library(shiny)
      con <- DBI::dbConnect(duckdb::duckdb())
      DBI::dbExecute(
        con,
        "
        CREATE TABLE t AS
        SELECT i AS id, 'name_' || i AS name FROM range(2000) r(i)"
      )
      b <- reactable_duckdb_backend(dplyr::tbl(con, "t"), key = "id")
      ui <- fluidPage(
        reactable::reactableOutput("tbl"),
        verbatimTextOutput("keys"),
        verbatimTextOutput("rows")
      )
      server <- function(input, output, session) {
        output$tbl <- reactable::renderReactable(
          reactable_duckdb(b, selection = "multiple", defaultPageSize = 5)
        )
        selected_keys <- reactable_duckdb_selected_keys(b)
        selected_rows <- reactable_duckdb_selected(b)
        output$keys <- renderText(paste0(
          "KEYS=[",
          paste(selected_keys(), collapse = ","),
          "]"
        ))
        output$rows <- renderText({
          paste0(
            "NAMES=[",
            paste(
              dplyr::pull(dplyr::collect(selected_rows()), name),
              collapse = ","
            ),
            "]"
          )
        })
      }
      runApp(shinyApp(ui, server), port = port, launch.browser = FALSE)
    },
    args = list(port = port, pkg_dir = pkg_dir),
    stderr = stderr_file
  )
  withr::defer(proc$kill())

  base_url <- sprintf("http://127.0.0.1:%d", port)
  up <- FALSE
  for (i in 1:150) {
    up <- tryCatch(
      httr2::resp_status(httr2::req_perform(httr2::request(base_url))) == 200,
      error = function(e) FALSE
    )
    if (up) break
    if (!proc$is_alive()) {
      stop(
        "app died: ",
        paste(readLines(stderr_file, warn = FALSE), collapse = "\n")
      )
    }
    Sys.sleep(0.2)
  }
  expect_identical(up, TRUE)

  browser$Page$navigate(base_url)
  for (i in 1:150) {
    n <- browser$Runtime$evaluate(
      "document.querySelectorAll('.rt-tr-group').length"
    )$result$value
    if (!is.null(n) && n > 0) break
    Sys.sleep(0.2)
  }
  ev <- function(js) browser$Runtime$evaluate(js)$result$value
  settle <- function() Sys.sleep(2)
  click_row <- function(i) {
    ev(sprintf(
      "document.querySelectorAll('#tbl .rt-tbody .rt-tr')[%d].querySelector('input[type=checkbox]').click()",
      i
    ))
    settle()
  }
  # The id column as rendered, and which rows show a checked box.
  shown <- function() {
    jsonlite::fromJSON(ev(
      "
      JSON.stringify([...document.querySelectorAll('#tbl .rt-tbody .rt-tr')].map(r => ({
        id: r.querySelectorAll('.rt-td')[1].textContent,
        checked: r.querySelector('input[type=checkbox]').checked
      })))
    "
    ))
  }
  keys <- function() ev("document.querySelector('#keys').textContent")
  names_out <- function() ev("document.querySelector('#rows').textContent")

  expect_identical(shown()$id, as.character(0:4))

  click_row(2) # id = 2
  page1 <- shown()
  expect_identical(page1$checked, c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_identical(keys(), "KEYS=[2]")

  # Page 2. The row in the same *position* must not appear selected: that is
  # the page-index fallback this whole mechanism exists to prevent.
  ev("document.querySelectorAll('#tbl .rt-next-button')[0].click()")
  settle()
  page2 <- shown()
  expect_identical(page2$id, as.character(5:9))
  expect_identical(page2$checked, rep(FALSE, 5))
  # ...and the selection made on page 1 is still reported while off screen.
  expect_identical(keys(), "KEYS=[2]")

  click_row(0) # id = 5
  expect_identical(keys(), "KEYS=[2,5]")
  # The lazy query resolves both, including the row that is not on screen.
  expect_identical(names_out(), "NAMES=[name_2,name_5]")

  # Returning to page 1 shows the original selection, by row and not position.
  ev("document.querySelectorAll('#tbl .rt-prev-button')[0].click()")
  settle()
  back <- shown()
  expect_identical(back$id, as.character(0:4))
  expect_identical(back$checked, c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_identical(keys(), "KEYS=[2,5]")

  # Deselecting removes it from the set.
  click_row(2)
  expect_identical(keys(), "KEYS=[5]")

  # The select-all header is present but neutralized: clicking where it sits
  # must not select the page.
  header_click <- ev(
    "
    (() => {
      const c = document.querySelector('#tbl .rt-thead .rt-td-select');
      const r = c.getBoundingClientRect();
      const el = document.elementFromPoint(r.left + r.width/2, r.top + r.height/2);
      el.click();
      return el.className;
    })()
  "
  )
  settle()
  expect_no_match(header_click, "rt-select-input")
  expect_identical(keys(), "KEYS=[5]")
})
