filter_one <- function(backend, id, value, pageSize = 200) {
  server_data(
    backend,
    pageSize = pageSize,
    filters = list(list(id = id, value = value))
  )
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
  b <- reactable_duckdb_backend(
    tbl,
    filter_spec = list(name = list(exact = TRUE))
  )
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
  # Each case names its own column. Deriving it from the value (the old
  # `if (grepl("1.5", ...))`) meant the string under test silently decided
  # what was being tested.
  cases <- list(
    list(col = "qty", value = "10", expected = reference$qty == 10),
    list(col = "amount", value = "1.5", expected = reference$amount == 1.5),
    list(col = "qty", value = " >= 90 ", expected = reference$qty >= 90),
    list(col = "qty", value = "<=5", expected = reference$qty <= 5),
    list(col = "qty", value = ">98", expected = reference$qty > 98),
    list(col = "qty", value = "<2", expected = reference$qty < 2),
    list(
      col = "qty",
      value = "10..20",
      expected = reference$qty >= 10 & reference$qty <= 20
    ),
    list(
      col = "qty",
      value = "1e2..2e2",
      expected = reference$qty >= 100 & reference$qty <= 200
    )
  )
  for (case in cases) {
    rd <- filter_one(b, case$col, case$value)
    expect_equal(
      rd$rowCount,
      sum(case$expected),
      info = paste(case$col, case$value)
    )
  }
})

test_that("negative numbers and scientific notation parse", {
  skip_if_no_backend_api()
  con <- local_duckdb_con()
  DBI::dbExecute(
    con,
    "CREATE TABLE neg AS SELECT (i - 50)::DOUBLE AS x, i AS id FROM range(100) r(i)"
  )
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
    list(
      "2026-01-01..2026-01-31",
      reference$dt >= as.Date("2026-01-01") &
        reference$dt <= as.Date("2026-01-31")
    )
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
  expect_equal(
    rd$rowCount,
    sum(reference$ts == as.POSIXct("2026-01-01 05:00:00", tz = "UTC"))
  )
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

test_that("malformed datetimes are refused", {
  skip_if_no_backend_api()
  b <- local_backend(n = 200)
  bad_values <- c(
    "not-a-datetime",
    "2026-02-30 12:00:00", # impossible calendar day
    "12:00:00", # time with no date
    "2026-01-02..2026-01-01" # inverted range
  )
  for (bad in bad_values) {
    expect_error(
      filter_one(b, "ts", bad),
      class = "reactableduckdb_filter_error",
      info = bad
    )
  }
})

test_that("an out-of-range time is refused, never truncated to midnight", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000)
  # A loose time pattern let these through: as.POSIXct()'s tryFormats chain
  # falls back to "%Y-%m-%d", strptime matches that as a prefix, and the value
  # silently became midnight on the date -- filtering against an instant the
  # user never typed. Refusal is the only correct answer.
  for (bad in c(
    "2026-01-01 25:00:00",
    "2026-01-01 12:99:00",
    "2026-01-01 12:00:99",
    ">=2026-01-01 24:00:00",
    "2026-01-01 99:99:99"
  )) {
    expect_error(
      filter_one(b, "ts", bad),
      class = "reactableduckdb_filter_error",
      info = bad
    )
  }

  # The boundary values on either side of each range still parse.
  reference <- collect_reference(b)
  midnight <- filter_one(b, "ts", ">=2026-01-01 00:00:00")
  expect_equal(midnight$rowCount, nrow(reference))
  expect_error(filter_one(b, "ts", "2026-01-01 23:59:59"), NA)
  # The T separator is unaffected.
  expect_error(filter_one(b, "ts", ">=2026-01-01T23:59:59"), NA)
})

test_that("blank non-text filters mean no filter, and non-scalars are refused", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000)
  # Whitespace is "no filter" for every non-text type -- text keeps it, since
  # for text a space is a legitimate character to search for.
  expect_identical(filter_one(b, "qty", "   ")$rowCount, 2000)
  expect_identical(filter_one(b, "dt", "")$rowCount, 2000)

  # Values arrive from parsed JSON, so a scalar cannot be assumed.
  expect_error(
    filter_one(b, "qty", c("1", "2")),
    class = "reactableduckdb_filter_error"
  )
  expect_error(
    filter_one(b, "qty", NA_character_),
    class = "reactableduckdb_filter_error"
  )
})

# Logical ---------------------------------------------------------------------

test_that("logical filters accept the documented spellings", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000)
  for (truthy in c("true", "TRUE", "t", "1", " T ")) {
    expect_identical(
      filter_one(b, "flag", truthy)$rowCount,
      1000,
      info = truthy
    )
  }
  for (falsy in c("false", "F", "0")) {
    expect_identical(filter_one(b, "flag", falsy)$rowCount, 1000, info = falsy)
  }
  expect_error(
    filter_one(b, "flag", "yes"),
    class = "reactableduckdb_filter_error"
  )
})

# Capability + spec refusals --------------------------------------------------

test_that("unknown and display-only filter columns are refused", {
  skip_if_no_backend_api()
  b <- local_backend(n = 200)
  expect_error(
    filter_one(b, "nope", "x"),
    class = "reactableduckdb_filter_error"
  )
  expect_error(
    filter_one(b, "tags", "x"),
    class = "reactableduckdb_filter_error"
  )
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

test_that("filter_spec type reaches the capability map", {
  skip_if_no_backend_api()
  tbl <- local_duckdb_tbl(n = 2000)
  pinned <- reactable_duckdb_backend(
    tbl,
    filter_spec = list(qty = list(type = "numeric"))
  )
  expect_identical(pinned$capability$qty$type, "numeric")
  expect_identical(filter_one(pinned, "qty", "10")$rowCount, 20)
  expect_gt(filter_one(pinned, "qty", ">=90")$rowCount, 0)
})

test_that("a timestamp column can be pinned to date granularity", {
  skip_if_no_backend_api()
  tbl <- local_duckdb_tbl(n = 2000)
  reference <- DBI::dbGetQuery(
    dbplyr::remote_con(tbl),
    dbplyr::sql_render(tbl) |> as.character()
  )

  # DATE and TIMESTAMP compare freely in DuckDB, so this is the one cross-type
  # pin that is both valid and useful: users type a plain date at a timestamp
  # column instead of a full datetime.
  as_date <- reactable_duckdb_backend(
    tbl,
    filter_spec = list(ts = list(type = "date"))
  )
  expect_identical(as_date$capability$ts$type, "date")
  rd <- filter_one(as_date, "ts", ">=2026-01-05")
  expect_equal(
    rd$rowCount,
    sum(reference$ts >= as.POSIXct("2026-01-05", tz = "UTC"))
  )
  # ...and the datetime grammar is no longer what the column parses with.
  expect_error(
    filter_one(as_date, "ts", ">=2026-01-05 12:00:00"),
    class = "reactableduckdb_filter_error"
  )

  # The reverse pin is equally valid: a DATE column read at datetime precision.
  as_datetime <- reactable_duckdb_backend(
    tbl,
    filter_spec = list(dt = list(type = "datetime"))
  )
  expect_gt(filter_one(as_datetime, "dt", ">=2026-01-05 00:00:00")$rowCount, 0)
})

test_that("a type pin the column's SQL type cannot evaluate is refused", {
  skip_if_no_backend_api()
  tbl <- local_duckdb_tbl(n = 50)
  # Each of these built a predicate DuckDB could not bind, so the failure
  # surfaced as a raw binder error mid-request instead of a refusal here.
  # `qty` -> logical was worse: it bound fine and returned a surprising set.
  incompatible <- list(
    list(qty = list(type = "text")),
    list(qty = list(type = "date")),
    list(qty = list(type = "datetime")),
    list(qty = list(type = "logical")),
    list(name = list(type = "numeric")),
    list(name = list(type = "date")),
    list(name = list(type = "logical")),
    list(dt = list(type = "text")),
    list(dt = list(type = "numeric")),
    list(flag = list(type = "numeric")),
    list(flag = list(type = "text"))
  )
  for (spec in incompatible) {
    expect_error(
      reactable_duckdb_backend(tbl, filter_spec = spec),
      class = "reactableduckdb_spec_error",
      info = paste(names(spec), spec[[1]]$type)
    )
  }
})

test_that("pinning a column to its own grammar stays allowed", {
  skip_if_no_backend_api()
  tbl <- local_duckdb_tbl(n = 50)
  same <- list(
    list(name = list(type = "text")),
    list(qty = list(type = "numeric")),
    list(dt = list(type = "date")),
    list(ts = list(type = "datetime")),
    list(flag = list(type = "logical"))
  )
  for (spec in same) {
    expect_s3_class(
      reactable_duckdb_backend(tbl, filter_spec = spec),
      "reactable_duckdb_backend"
    )
  }
})

test_that("filter_spec is validated at construction", {
  skip_if_no_backend_api()
  tbl <- local_duckdb_tbl(n = 50)
  expect_error(
    reactable_duckdb_backend(
      tbl,
      filter_spec = list(nope = list(exact = TRUE))
    ),
    class = "reactableduckdb_spec_error"
  )
  expect_error(
    reactable_duckdb_backend(
      tbl,
      filter_spec = list(tags = list(type = "text"))
    ),
    class = "reactableduckdb_spec_error"
  )
  expect_error(
    reactable_duckdb_backend(tbl, filter_spec = list(qty = list(exact = TRUE))),
    class = "reactableduckdb_spec_error"
  )
  expect_error(
    reactable_duckdb_backend(
      tbl,
      filter_spec = list(name = list(fuzzy = TRUE))
    ),
    class = "reactableduckdb_spec_error"
  )
  # Not a named list at all.
  expect_error(
    reactable_duckdb_backend(tbl, filter_spec = list(list(exact = TRUE))),
    class = "reactableduckdb_spec_error"
  )
  # A named entry that is not a list.
  expect_error(
    reactable_duckdb_backend(tbl, filter_spec = list(name = "exact")),
    class = "reactableduckdb_spec_error"
  )
  # `type` present but not one of the supported values.
  expect_error(
    reactable_duckdb_backend(tbl, filter_spec = list(name = list(type = "regex"))),
    class = "reactableduckdb_spec_error"
  )
  # `exact` present but not a usable flag.
  expect_error(
    reactable_duckdb_backend(tbl, filter_spec = list(name = list(exact = NA))),
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
    sortBy = list(
      list(id = "1st value", desc = TRUE),
      list(id = "select", desc = FALSE)
    )
  )
  expect_identical(nrow(rd$data), 10L)
  sql <- last_page_sql(b)
  expect_match(sql, '"1st value" DESC NULLS LAST', fixed = TRUE)
  expect_match(sql, '"order id" ASC NULLS LAST', fixed = TRUE)
})
