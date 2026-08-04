test_that("multi-column sort follows reactable priority order", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000, key = "id")
  reference <- collect_reference(b)

  rd <- server_data(
    b,
    pageSize = 100,
    sortBy = list(
      list(id = "grp", desc = FALSE),
      list(id = "amount", desc = TRUE)
    )
  )
  expected <- reference[
    order(reference$grp, -reference$amount, reference$id, na.last = TRUE),
  ]
  expect_identical(rd$data$id, expected$id[1:100])
})

test_that("NULLS LAST holds in both sort directions", {
  skip_if_no_backend_api()
  b <- local_backend(n = 200, key = "id")
  n_null <- sum(is.na(collect_reference(b)$grp))
  expect_gt(n_null, 0)

  for (desc in c(FALSE, TRUE)) {
    rd <- server_data(
      b,
      pageSize = 200,
      sortBy = list(list(id = "grp", desc = desc))
    )
    expect_identical(which(is.na(rd$data$grp)), seq(200 - n_null + 1, 200))
  }
})

test_that("rendered ORDER BY uses explicit NULLS LAST for every clause", {
  skip_if_no_backend_api()
  b <- local_backend(n = 200, key = "id")
  reactableduckdb:::enable_query_log(b)
  server_data(
    b,
    sortBy = list(list(id = "grp", desc = TRUE), list(id = "qty", desc = FALSE))
  )
  sql <- last_page_sql(b)
  expect_match(sql, '"grp" DESC NULLS LAST')
  expect_match(sql, '"qty" ASC NULLS LAST')
  expect_match(sql, '"id" ASC NULLS LAST')
})

test_that("key is appended as tie-breaker only when not already sorted", {
  clauses <- reactableduckdb:::build_order_clauses(
    list(list(id = "qty", desc = FALSE)),
    key = "id",
    capability = list(
      qty = list(filterable = TRUE, sortable = TRUE, r_type = "integer"),
      id = list(filterable = TRUE, sortable = TRUE, r_type = "numeric")
    )
  )
  expect_identical(clauses, c('"qty" ASC NULLS LAST', '"id" ASC NULLS LAST'))

  clauses <- reactableduckdb:::build_order_clauses(
    list(list(id = "id", desc = TRUE)),
    key = "id",
    capability = list(
      id = list(filterable = TRUE, sortable = TRUE, r_type = "numeric")
    )
  )
  expect_identical(clauses, '"id" DESC NULLS LAST')
})

test_that("key tie-breaker makes ties deterministic", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000, key = "id")
  reference <- collect_reference(b)
  rd <- server_data(
    b,
    pageSize = 50,
    sortBy = list(list(id = "flag", desc = TRUE))
  )
  expected <- reference[order(!reference$flag, reference$id), ]
  expect_identical(rd$data$id, expected$id[1:50])
})

test_that("unknown and display-only sort columns are refused", {
  skip_if_no_backend_api()
  b <- local_backend(n = 200)
  expect_error(
    server_data(b, sortBy = list(list(id = "nope", desc = FALSE))),
    class = "reactableduckdb_sort_error"
  )
  expect_error(
    server_data(b, sortBy = list(list(id = "tags", desc = FALSE))),
    class = "reactableduckdb_sort_error"
  )
  expect_error(
    server_data(b, sortBy = list("grp")),
    class = "reactableduckdb_sort_error"
  )
})

test_that("validate_key catches duplicated keys; the default does not scan", {
  skip_if_no_backend_api()
  tbl <- local_duckdb_tbl(n = 200)
  # qty repeats every 100 rows: never unique here.
  expect_error(
    reactable_duckdb_backend(tbl, key = "qty", validate_key = TRUE),
    class = "reactableduckdb_key_error"
  )
  expect_s3_class(
    reactable_duckdb_backend(tbl, key = "qty"),
    "reactable_duckdb_backend"
  )
  expect_s3_class(
    reactable_duckdb_backend(tbl, key = "id", validate_key = TRUE),
    "reactable_duckdb_backend"
  )
})

test_that("key must exist and be orderable; validate_key requires key", {
  skip_if_no_backend_api()
  tbl <- local_duckdb_tbl(n = 50)
  expect_error(
    reactable_duckdb_backend(tbl, key = "nope"),
    class = "reactableduckdb_key_error"
  )
  expect_error(
    reactable_duckdb_backend(tbl, key = "tags"),
    class = "reactableduckdb_key_error"
  )
  expect_error(
    reactable_duckdb_backend(tbl, validate_key = TRUE),
    class = "reactableduckdb_key_error"
  )
})
