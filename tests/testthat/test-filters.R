filter_one <- function(backend, id, value, pageSize = 200) {
  server_data(backend, pageSize = pageSize, filters = list(list(id = id, value = value)))
}

# Text ------------------------------------------------------------------------

test_that("hostile text values are matched literally and safely", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000)
  for (value in hostile_names) {
    rd <- filter_one(b, "name", value)
    expect_identical(rd$rowCount, 1, info = value)
    expect_identical(rd$data$name, value)
  }
})

test_that("text matching is case-insensitive substring", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000)
  rd <- filter_one(b, "name", "o'brien")
  expect_identical(rd$data$name, "O'Brien")
  # substring: "name_1" matches name_1, name_10..name_19, ...
  rd <- filter_one(b, "name", "NAME_11")
  expect_identical(sort(unique(rd$data$name)), "name_11")
  expect_gt(rd$rowCount, 1)
})

test_that("% and _ are literal, not SQL wildcards", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000)
  # "100%" must match only the hostile literal row, not every "100..." name.
  rd <- filter_one(b, "name", "100%")
  expect_identical(rd$rowCount, 1)
  # "a_b" must not act as a single-char wildcard.
  rd <- filter_one(b, "name", "a_b")
  expect_identical(rd$rowCount, 1)
  expect_identical(rd$data$name, "a_b")
})

test_that("filter values are bound as escaped literals, never raw SQL", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000)
  reactableduckdb:::enable_query_log(b)
  filter_one(b, "name", "'; DROP TABLE x; --")
  sql <- last_page_sql(b)
  expect_match(sql, "'''; DROP TABLE x; --'", fixed = TRUE)
  # The table is intact afterwards.
  expect_identical(server_data(b)$rowCount, 2000)
})

test_that("text that resembles other grammars stays a substring search", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000)
  rd <- filter_one(b, "name", "10..20")
  expect_identical(rd$rowCount, 1)
  expect_identical(rd$data$name, "10..20")
})

test_that("empty and NULL text filters mean no filter", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000)
  expect_identical(filter_one(b, "name", "")$rowCount, 2000)
  expect_identical(filter_one(b, "name", NULL)$rowCount, 2000)
})

test_that("filter_spec exact overrides substring matching", {
  skip_if_no_backend_api()
  tbl <- local_duckdb_tbl(n = 2000)
  b <- reactable_duckdb_backend(tbl, filter_spec = list(name = list(exact = TRUE)))
  rd <- filter_one(b, "name", "name_11")
  expect_identical(sort(unique(rd$data$name)), "name_11")
  # Exact: the substring "name_1" no longer matches name_11.
  rd <- filter_one(b, "name", "name_1")
  expect_identical(sort(unique(rd$data$name)), "name_1")
})

# Numeric ---------------------------------------------------------------------

test_that("the numeric grammar covers the documented forms", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000)
  reference <- collect_reference(b)
  cases <- list(
    list("10", reference$qty == 10),
    list("1.5", reference$amount == 1.5),
    list(" >= 90 ", reference$qty >= 90),
    list("<=5", reference$qty <= 5),
    list(">98", reference$qty > 98),
    list("<2", reference$qty < 2),
    list("10..20", reference$qty >= 10 & reference$qty <= 20),
    list("1e2..2e2", reference$qty >= 100 & reference$qty <= 200)
  )
  for (case in cases) {
    col <- if (grepl("1.5", case[[1]], fixed = TRUE)) "amount" else "qty"
    rd <- filter_one(b, col, case[[1]])
    expect_equal(rd$rowCount, sum(case[[2]]), info = case[[1]])
  }
})

test_that("negative numbers and scientific notation parse", {
  skip_if_no_backend_api()
  con <- local_duckdb_con()
  DBI::dbExecute(con, "CREATE TABLE neg AS SELECT (i - 50)::DOUBLE AS x, i AS id FROM range(100) r(i)")
  b <- reactable_duckdb_backend(dplyr::tbl(con, "neg"), key = "id")
  expect_identical(filter_one(b, "x", "-5")$rowCount, 1)
  expect_identical(filter_one(b, "x", ">-5")$rowCount, 54)
  expect_identical(filter_one(b, "x", "<-5")$rowCount, 45)
  expect_identical(filter_one(b, "x", "-10..-5")$rowCount, 6)
  expect_identical(filter_one(b, "x", "4.9e1")$rowCount, 1)
})

test_that("malformed numeric input raises a structured error", {
  skip_if_no_backend_api()
  b <- local_backend(n = 200)
  for (bad in c("abc", ">>5", "1..2..3", "5..", "..5", "1_000", "20..10")) {
    expect_error(
      filter_one(b, "qty", bad),
      class = "reactableduckdb_filter_error",
      info = bad
    )
  }
})

# Date / datetime -------------------------------------------------------------

test_that("date filters support equality, comparators, and ranges", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000)
  reference <- collect_reference(b)
  cases <- list(
    list("2026-01-05", reference$dt == as.Date("2026-01-05")),
    list(">=2026-12-01", reference$dt >= as.Date("2026-12-01")),
    list("<=2026-01-03", reference$dt <= as.Date("2026-01-03")),
    list("2026-01-01..2026-01-31", reference$dt >= as.Date("2026-01-01") & reference$dt <= as.Date("2026-01-31"))
  )
  for (case in cases) {
    rd <- filter_one(b, "dt", case[[1]])
    expect_equal(rd$rowCount, sum(case[[2]]), info = case[[1]])
  }
})

test_that("datetime filters compare as UTC instants", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000)
  reference <- collect_reference(b)
  cutoff <- as.POSIXct("2026-01-05 12:00:00", tz = "UTC")
  rd <- filter_one(b, "ts", ">=2026-01-05 12:00:00")
  expect_equal(rd$rowCount, sum(reference$ts >= cutoff))
  rd <- filter_one(b, "ts", "2026-01-01 05:00:00")
  expect_equal(rd$rowCount, sum(reference$ts == as.POSIXct("2026-01-01 05:00:00", tz = "UTC")))
})

test_that("impossible calendar dates are refused", {
  skip_if_no_backend_api()
  b <- local_backend(n = 200)
  for (bad in c("2026-13-01", "2026-02-30", "not-a-date")) {
    expect_error(
      filter_one(b, "dt", bad),
      class = "reactableduckdb_filter_error",
      info = bad
    )
  }
})

# Logical ---------------------------------------------------------------------

test_that("logical filters accept the documented spellings", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000)
  for (truthy in c("true", "TRUE", "t", "1", " T ")) {
    expect_identical(filter_one(b, "flag", truthy)$rowCount, 1000, info = truthy)
  }
  for (falsy in c("false", "F", "0")) {
    expect_identical(filter_one(b, "flag", falsy)$rowCount, 1000, info = falsy)
  }
  expect_error(filter_one(b, "flag", "yes"), class = "reactableduckdb_filter_error")
})

# Capability + spec refusals --------------------------------------------------

test_that("unknown and display-only filter columns are refused", {
  skip_if_no_backend_api()
  b <- local_backend(n = 200)
  expect_error(filter_one(b, "nope", "x"), class = "reactableduckdb_filter_error")
  expect_error(filter_one(b, "tags", "x"), class = "reactableduckdb_filter_error")
})

test_that("malformed filter requests are refused", {
  skip_if_no_backend_api()
  b <- local_backend(n = 200)
  expect_error(
    server_data(b, filters = list("name")),
    class = "reactableduckdb_filter_error"
  )
  expect_error(
    server_data(b, filters = "name"),
    class = "reactableduckdb_filter_error"
  )
})

test_that("filter_spec is validated at construction", {
  skip_if_no_backend_api()
  tbl <- local_duckdb_tbl(n = 50)
  expect_error(
    reactable_duckdb_backend(tbl, filter_spec = list(nope = list(exact = TRUE))),
    class = "reactableduckdb_spec_error"
  )
  expect_error(
    reactable_duckdb_backend(tbl, filter_spec = list(tags = list(type = "text"))),
    class = "reactableduckdb_spec_error"
  )
  expect_error(
    reactable_duckdb_backend(tbl, filter_spec = list(qty = list(exact = TRUE))),
    class = "reactableduckdb_spec_error"
  )
  expect_error(
    reactable_duckdb_backend(tbl, filter_spec = list(name = list(fuzzy = TRUE))),
    class = "reactableduckdb_spec_error"
  )
})

test_that("whitespace-tolerant parsing still trims nothing from text", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000)
  # " 10 " parses as numeric equality...
  expect_identical(filter_one(b, "qty", " 10 ")$rowCount, 20)
  # ...but text search for a leading space is a literal (and matches nothing).
  expect_identical(filter_one(b, "name", " name_11")$rowCount, 0)
})

# Non-syntactic column names --------------------------------------------------

test_that("filters and sorts work on non-syntactic column names", {
  skip_if_no_backend_api()
  b <- reactable_duckdb_backend(local_weird_tbl(n = 500), key = "order id")
  reactableduckdb:::enable_query_log(b)

  rd <- filter_one(b, "1st value", ">=25")
  expect_identical(rd$rowCount, 250)
  rd <- filter_one(b, "café", "v3")
  expect_gt(rd$rowCount, 0)
  rd <- filter_one(b, "select", "1")
  expect_gt(rd$rowCount, 0)
  rd <- filter_one(b, 'we"ird', "x42")
  expect_equal(rd$rowCount, sum(grepl("x42", paste0("x", 0:499), fixed = TRUE)))

  rd <- server_data(
    b,
    pageSize = 10,
    sortBy = list(list(id = "1st value", desc = TRUE), list(id = "select", desc = FALSE))
  )
  expect_identical(nrow(rd$data), 10L)
  sql <- last_page_sql(b)
  expect_match(sql, '"1st value" DESC NULLS LAST', fixed = TRUE)
  expect_match(sql, '"order id" ASC NULLS LAST', fixed = TRUE)
})
