# The input validators, as units. Every one of them exists to refuse rather
# than coerce or clamp, so each test asserts the refusal and its class -- a
# validator that quietly repaired its input would be the defect.
#
# The SQL builders they feed (duckdb_page_sql, build_order_clauses) are tested
# in test-pagination.R and test-safety.R, next to the injection surface they
# protect.

test_that("check_page_params validates and never clamps", {
  check <- reactableduckdb:::check_page_params
  expect_identical(check(0, 50, 500), list(limit = 50, offset = 0))
  expect_identical(check(3, 25, 500), list(limit = 25, offset = 75))
  expect_error(check(-1, 10, 500), class = "reactableduckdb_page_error")
  expect_error(check(0.5, 10, 500), class = "reactableduckdb_page_error")
  expect_error(check(0, 0, 500), class = "reactableduckdb_page_error")
  expect_error(check(0, NA, 500), class = "reactableduckdb_page_error")
  expect_error(check(0, 501, 500), class = "reactableduckdb_page_error")
  expect_error(check(2^45, 500, 500), class = "reactableduckdb_page_error")
})

test_that("a column id must be a single non-empty string", {
  check <- reactableduckdb:::check_column_id
  capability <- list(
    id = list(filterable = TRUE, sortable = TRUE, r_type = "numeric")
  )
  # These arrive from parsed JSON, so the shape cannot be assumed. Each is
  # refused as the caller's purpose, never coerced into one.
  for (bad in list("", NA_character_, character(0), c("id", "id"), 1L, NULL)) {
    expect_error(
      check(bad, capability, purpose = "filter"),
      class = "reactableduckdb_filter_error"
    )
    expect_error(
      check(bad, capability, purpose = "sort"),
      class = "reactableduckdb_sort_error"
    )
  }
})

test_that("check_count accepts whole numbers at or above the minimum", {
  check <- reactableduckdb:::check_count
  expect_identical(check(1, "n"), 1)
  expect_identical(check(500L, "n"), 500)
  for (bad in list(0, -1, 1.5, NA_integer_, Inf, "10", c(1, 2), NULL)) {
    expect_error(check(bad, "n"), class = "reactableduckdb_page_error")
  }
})

test_that("check_flag accepts only TRUE or FALSE", {
  check <- reactableduckdb:::check_flag
  expect_identical(check(TRUE, "f"), TRUE)
  expect_identical(check(FALSE, "f"), FALSE)
  for (bad in list(NA, "yes", 1, c(TRUE, TRUE), NULL)) {
    expect_error(check(bad, "f"), class = "reactableduckdb_source_error")
  }
})

test_that("check_backend refuses anything that is not a backend", {
  check <- reactableduckdb:::check_backend
  for (bad in list(list(), data.frame(x = 1), NULL, "backend")) {
    expect_error(check(bad), class = "reactableduckdb_source_error")
  }
})

test_that("check_connection refuses a closed connection and never reconnects", {
  skip_if_no_backend_api()
  con <- DBI::dbConnect(duckdb::duckdb())
  expect_identical(reactableduckdb:::check_connection(con), con)
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_error(
    reactableduckdb:::check_connection(con),
    class = "reactableduckdb_connection_error"
  )
  # Still invalid afterwards: the check reports, it does not repair.
  expect_false(DBI::dbIsValid(con))
})

test_that("is_whole_number is the exactness boundary the validators share", {
  whole <- reactableduckdb:::is_whole_number
  expect_true(whole(0))
  expect_true(whole(2^53))
  expect_true(whole(-5L))
  expect_false(whole(1.5))
  expect_false(whole(NA_real_))
  expect_false(whole(Inf))
  expect_false(whole(c(1, 2)))
  expect_false(whole("1"))
})
