# Primary no-materialization evidence (plan section 14, amended per D1):
# the deterministic query log plus an absolute allocation ceiling. Stingy
# sources cannot catch an accidental full collect, so these can.

test_that("query log shows only scalar counts and LIMITed page queries", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000, key = "id")
  reactableduckdb:::enable_query_log(b)
  server_data(b, pageIndex = 0, pageSize = 50)
  server_data(
    b,
    pageIndex = 2,
    pageSize = 50,
    filters = list(list(id = "qty", value = ">=10"))
  )
  server_data(
    b,
    pageIndex = 0,
    pageSize = 50,
    sortBy = list(list(id = "amount", desc = TRUE))
  )

  log <- query_log(b)
  expect_gt(length(log), 0)
  for (sql in log) {
    is_count <- grepl("reactableduckdb_count", sql, fixed = TRUE)
    is_page <- grepl("reactableduckdb_base", sql, fixed = TRUE)
    expect_true(is_count || is_page)
    if (is_count) {
      expect_match(sql, '^SELECT COUNT\\(\\*\\) AS "n" FROM \\(')
    } else {
      expect_match(sql, "LIMIT \\d+ OFFSET \\d+$")
    }
  }
})

test_that("page allocation stays under an absolute ceiling at any source size", {
  skip_if_no_backend_api()
  skip_if_not_installed("bench")
  # Under covr every traced expression allocates, so mem_alloc would measure
  # the instrumentation rather than the package -- a false failure at best and
  # a meaningless pass at worst. This is also the most expensive test in the
  # suite to run instrumented (two 250,000-row tables).
  skip_on_covr()
  measure_page <- function(n) {
    tbl <- local_duckdb_tbl(n = n)
    b <- reactable_duckdb_backend(tbl, key = "id")
    server_data(b, pageSize = 50) # warm-up: dispatch, translation caches
    result <- bench::mark(
      server_data(b, pageIndex = 1, pageSize = 50),
      iterations = 1,
      check = FALSE,
      # bench::mark() drops iterations that garbage collected, and warns
      # ("Some expressions had a GC in every iteration") when that leaves it
      # nothing. Whether it fires depends on GC timing, so it made the suite's
      # warning count nondeterministic. There is one iteration and only
      # mem_alloc is read, so filtering has nothing to contribute here.
      filter_gc = FALSE
    )
    as.numeric(result$mem_alloc)
  }
  small <- measure_page(10000)
  large <- measure_page(250000)
  ceiling_bytes <- 8 * 1024^2
  expect_lt(small, ceiling_bytes)
  expect_lt(large, ceiling_bytes)
})

test_that("DuckDB's plan for a page query is a streaming LIMIT", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000, key = "id")
  reactableduckdb:::enable_query_log(b)
  server_data(b, pageIndex = 1, pageSize = 25)
  plan <- DBI::dbGetQuery(b$con, paste("EXPLAIN", last_page_sql(b)))
  plan_text <- paste(unlist(plan), collapse = "\n")
  # With an ORDER BY, DuckDB plans the bounded page as a streaming TOP_N
  # operator (Top: <limit>, Offset: <offset>); without one it is LIMIT.
  expect_match(plan_text, "LIMIT|TOP_N", ignore.case = TRUE)
  expect_match(plan_text, "Top: 25")
})

test_that("a user-owned dbplyr lazy pipeline works end to end", {
  skip_if_no_backend_api()
  tbl <- local_duckdb_tbl(n = 2000)
  lazy <- tbl |>
    dplyr::filter(qty > 10) |>
    dplyr::mutate(revenue = amount * qty) |>
    dplyr::select(id, name, grp, qty, revenue)
  b <- reactable_duckdb_backend(lazy, key = "id")
  rd <- server_data(b, filters = list(list(id = "revenue", value = ">=1000")))
  # `__state` is the row-identity column appended to every page of a keyed
  # backend (R/selection.R); the source's own columns follow the pipeline.
  expect_identical(
    names(rd$data),
    c("id", "name", "grp", "qty", "revenue", "__state")
  )
  reference <- collect_reference(b)
  expect_equal(rd$rowCount, sum(reference$revenue >= 1000))
})

test_that("the backend never disconnects a connection it does not own", {
  skip_if_no_backend_api()
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, paste("CREATE TABLE test_data AS", test_table_sql(100)))
  b <- reactable_duckdb_backend(dplyr::tbl(con, "test_data"))
  server_data(b)
  rm(b)
  gc()
  expect_true(DBI::dbIsValid(con))
  expect_identical(nrow(DBI::dbGetQuery(con, "SELECT 1")), 1L)
})
