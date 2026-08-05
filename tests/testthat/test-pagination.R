# Wrapper units --------------------------------------------------------------

test_that("duckdb_page_sql builds LIMIT/OFFSET over a wrapped subquery", {
  sql <- reactableduckdb:::duckdb_page_sql(
    "SELECT * FROM t",
    c('"a" ASC NULLS LAST', '"b" DESC NULLS LAST'),
    50,
    100
  )
  expect_match(sql, "^SELECT \\* FROM \\(SELECT \\* FROM t\\)")
  expect_match(sql, 'ORDER BY "a" ASC NULLS LAST, "b" DESC NULLS LAST')
  expect_match(sql, "LIMIT 50 OFFSET 100$")
})

test_that("duckdb_page_sql omits ORDER BY when there are no clauses", {
  sql <- reactableduckdb:::duckdb_page_sql("SELECT 1", character(0), 10, 0)
  expect_no_match(sql, "ORDER BY")
  expect_match(sql, "LIMIT 10 OFFSET 0$")
})

test_that("large offsets are formatted without scientific notation", {
  sql <- reactableduckdb:::duckdb_page_sql("SELECT 1", character(0), 500, 1e10)
  expect_match(sql, "OFFSET 10000000000$")
})

test_that("duckdb_page_sql refuses malformed inputs", {
  page_sql <- reactableduckdb:::duckdb_page_sql
  expect_error(
    page_sql("", character(0), 10, 0),
    class = "reactableduckdb_page_error"
  )
  expect_error(
    page_sql("SELECT 1", character(0), 0, 0),
    class = "reactableduckdb_page_error"
  )
  expect_error(
    page_sql("SELECT 1", character(0), 10.5, 0),
    class = "reactableduckdb_page_error"
  )
  expect_error(
    page_sql("SELECT 1", character(0), 10, -1),
    class = "reactableduckdb_page_error"
  )
  expect_error(
    page_sql("SELECT 1", character(0), 10, 2^53 + 2),
    class = "reactableduckdb_page_error"
  )
})

# check_page_params(), which gates every one of these, is unit-tested in
# test-validation.R alongside the other validators.

# Functional pages ------------------------------------------------------------

# One page shape per test: a failure then names the shape that broke rather
# than reporting that "pages" are broken.

test_that("the first page starts at the first row and reports the full count", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000, key = "id")
  first <- server_data(b, pageIndex = 0, pageSize = 150)
  expect_identical(nrow(first$data), 150L)
  expect_identical(first$data$id[1:3], c(0, 1, 2))
  expect_identical(first$rowCount, 2000)
})

test_that("a middle page starts at its offset", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000, key = "id")
  middle <- server_data(b, pageIndex = 5, pageSize = 150)
  expect_identical(middle$data$id[[1]], 750)
})

test_that("the final page is short rather than padded", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000, key = "id")
  # 2000 rows / 150 per page: the 14th page holds the last 50 rows.
  last <- server_data(b, pageIndex = 13, pageSize = 150)
  expect_identical(nrow(last$data), 50L)
  expect_identical(last$data$id[[50]], 1999)
})

test_that("a page past the end is empty but still counts the whole source", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000, key = "id")
  beyond <- server_data(b, pageIndex = 100, pageSize = 150)
  expect_identical(nrow(beyond$data), 0L)
  expect_identical(beyond$rowCount, 2000)
})

test_that("a filter matching nothing yields a zero-row page with the schema", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000, key = "id")
  none <- server_data(
    b,
    filters = list(list(id = "name", value = "no such value anywhere"))
  )
  expect_identical(nrow(none$data), 0L)
  expect_identical(none$rowCount, 0)
  # The columns must still be there: reactable renders from the page's shape.
  expect_identical(names(none$data), b$columns)
})

test_that("the count query is a single scalar aggregate", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000)
  reactableduckdb:::enable_query_log(b)
  server_data(b)
  count_sql <- last_count_sql(b)
  expect_match(count_sql, 'SELECT COUNT\\(\\*\\) AS "n" FROM \\(')
  expect_identical(nrow(DBI::dbGetQuery(b$con, count_sql)), 1L)
})

test_that("page SQL carries the requested LIMIT and OFFSET", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000)
  reactableduckdb:::enable_query_log(b)
  server_data(b, pageIndex = 7, pageSize = 25)
  expect_match(last_page_sql(b), "LIMIT 25 OFFSET 175$")
})

test_that("an oversized page request errors before any query executes", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000)
  reactableduckdb:::enable_query_log(b)
  expect_error(
    server_data(b, pageSize = 501),
    class = "reactableduckdb_page_error"
  )
  expect_length(query_log(b), 0)
})

test_that("negative and fractional pageIndex requests error, never clamp", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000)
  # Upstream's reference dfPaginate() clamps out-of-range page indices; this
  # backend deliberately refuses instead (plan section 11).
  expect_error(
    server_data(b, pageIndex = -1),
    class = "reactableduckdb_page_error"
  )
  expect_error(
    server_data(b, pageIndex = 1.5),
    class = "reactableduckdb_page_error"
  )
})
