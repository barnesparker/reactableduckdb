# Backend construction and the object itself: the source adapter's refusals,
# `key` validation, and the print method. Query behaviour through the backend
# lives in test-pagination.R / test-sorting.R / test-filters.R; the count cache
# lives in test-cache.R.

# Source adapter refusals -----------------------------------------------------

test_that("a real non-DuckDB DBI source is refused, not translated", {
  skip_if_no_backend_api()
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "t", data.frame(id = 1:10, x = letters[1:10]))
  # A perfectly good lazy dbplyr pipeline -- just not on DuckDB. The package
  # generates DuckDB-specific SQL (NULLS LAST, strpos), so refusing is the
  # only honest answer; translating it would produce quietly wrong queries.
  expect_error(
    reactable_duckdb_backend(dplyr::tbl(con, "t")),
    class = "reactableduckdb_source_error"
  )
})

test_that("a prototype with no columns is refused", {
  # Tested at the unit level on purpose: the public constructor renders the
  # query (backend.R) before building the prototype, and dbplyr itself aborts
  # on a zero-column query first ("Query contains no columns"). This guard is
  # therefore defensive, and this is the only way to reach it.
  expect_error(
    reactableduckdb:::source_prototype(data.frame()),
    class = "reactableduckdb_schema_error"
  )
})

# key validation --------------------------------------------------------------

test_that("key must be a single column name", {
  skip_if_no_backend_api()
  tbl <- local_duckdb_tbl(n = 50)
  for (bad in list(c("id", "qty"), 1L, NA_character_, character(0))) {
    expect_error(
      reactable_duckdb_backend(tbl, key = bad),
      class = "reactableduckdb_key_error"
    )
  }
})

# print -----------------------------------------------------------------------

test_that("printing a backend reports schema, key, and cache state", {
  skip_if_no_backend_api()
  b <- local_backend(n = 200, key = "id")
  out <- cli::cli_fmt(print(b))
  text <- paste(out, collapse = "\n")

  expect_match(text, "reactable_duckdb_backend")
  expect_match(text, "key: id", fixed = TRUE)
  expect_match(text, "max_page_size: 500", fixed = TRUE)
  # `tags` is a LIST column, so it is display-only and must be called out.
  expect_match(text, "display-only")
  expect_match(text, "tags")
  # Cache counters are live, not a construction-time snapshot.
  expect_match(text, "0 hits / 0 misses", fixed = TRUE)
  server_data(b)
  expect_match(
    paste(cli::cli_fmt(print(b)), collapse = "\n"),
    "0 hits / 1 misses",
    fixed = TRUE
  )
})

test_that("printing reports a keyless backend and a disabled cache", {
  skip_if_no_backend_api()
  tbl <- local_duckdb_tbl(n = 50)
  b <- reactable_duckdb_backend(tbl, cache_counts = FALSE)
  text <- paste(cli::cli_fmt(print(b)), collapse = "\n")
  expect_match(text, "key: none", fixed = TRUE)
  expect_match(text, "count cache: disabled", fixed = TRUE)
})

test_that("print returns its input invisibly", {
  skip_if_no_backend_api()
  b <- local_backend(n = 50)
  cli::cli_fmt(visible <- withVisible(print(b)))
  expect_false(visible$visible)
  expect_identical(visible$value, b)
})
