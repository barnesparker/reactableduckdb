# quote_ident -----------------------------------------------------------------

test_that("quote_ident quotes unconditionally and doubles embedded quotes", {
  quote_ident <- reactableduckdb:::quote_ident
  expect_identical(quote_ident("id"), '"id"')
  expect_identical(quote_ident("1st"), '"1st"')
  expect_identical(quote_ident("order id"), '"order id"')
  expect_identical(quote_ident('we"ird'), '"we""ird"')
  expect_identical(
    quote_ident('x"; DROP TABLE t; --'),
    '"x""; DROP TABLE t; --"'
  )
})

test_that("hostile column names execute safely once schema-validated", {
  skip_if_no_backend_api()
  con <- local_duckdb_con()
  DBI::dbExecute(
    con,
    'CREATE TABLE h AS SELECT i AS "x""; drop", i AS id FROM range(20) r(i)'
  )
  b <- reactable_duckdb_backend(dplyr::tbl(con, "h"), key = "id")
  rd <- server_data(b, sortBy = list(list(id = 'x"; drop', desc = TRUE)))
  expect_identical(nrow(rd$data), 10L)
})

test_that("unknown identifiers are refused before reaching the wrapper", {
  clauses <- reactableduckdb:::build_order_clauses
  capability <- list(
    id = list(filterable = TRUE, sortable = TRUE, r_type = "numeric")
  )
  expect_error(
    clauses(list(list(id = "nope", desc = FALSE)), NULL, capability),
    class = "reactableduckdb_sort_error"
  )
  expect_error(
    clauses(list(list(id = '"; DROP', desc = FALSE)), NULL, capability),
    class = "reactableduckdb_sort_error"
  )
  # A sortBy that is not a list at all, as opposed to a list of bad entries.
  expect_error(
    clauses("id", NULL, capability),
    class = "reactableduckdb_sort_error"
  )
  # No sort at all is not an error; it is simply no ORDER BY.
  expect_identical(clauses(NULL, NULL, capability), character(0))
})

# check_column_id()'s own shape validation is unit-tested in test-validation.R;
# what matters here is that an unknown identifier never reaches the wrapper.

# Constructor refusals (plan section 4a) --------------------------------------

test_that("reactable_duckdb refuses incompatible arguments", {
  skip_if_no_backend_api()
  b <- local_backend(n = 100)
  refused <- list(
    list(server = "df"),
    list(data = data.frame(x = 1)),
    list(searchable = TRUE),
    list(searchMethod = "contains"),
    list(groupBy = "grp"),
    list(pagination = FALSE),
    list(paginateSubRows = TRUE),
    list(rownames = TRUE),
    list(defaultPageSize = 501L),
    list(pageSizeOptions = c(10L, 501L)),
    list(pageSizeOptions = c(0L, 10L)),
    list(pageSizeOptions = "ten")
  )
  for (args in refused) {
    expect_error(
      do.call(reactable_duckdb, c(list(b), args)),
      class = "reactableduckdb_unsupported_arg",
      info = names(args)
    )
  }
})

test_that("allowed arguments still pass through", {
  skip_if_no_backend_api()
  # `selection` needs a key, so this backend has one.
  b <- local_backend(n = 100, key = "id")
  w <- reactable_duckdb(
    b,
    filterable = TRUE,
    searchable = FALSE,
    selection = "multiple",
    striped = TRUE,
    defaultPageSize = 25L
  )
  expect_s3_class(w, "reactable")
})

test_that("eager data frames and non-DuckDB sources are refused", {
  skip_if_no_backend_api()
  expect_error(
    reactable_duckdb_backend(data.frame(x = 1:10)),
    class = "reactableduckdb_source_error"
  )
  expect_error(
    reactable_duckdb_backend(structure(list(), class = c("tbl_lazy", "tbl"))),
    class = "reactableduckdb_source_error"
  )
})

test_that("constructor argument types are validated", {
  skip_if_no_backend_api()
  tbl <- local_duckdb_tbl(n = 50)
  expect_error(
    reactable_duckdb_backend(tbl, max_page_size = 0),
    class = "reactableduckdb_page_error"
  )
  expect_error(
    reactable_duckdb_backend(tbl, cache_counts = "yes"),
    class = "reactableduckdb_source_error"
  )
  expect_error(
    reactable_duckdb(list(), ),
    class = "reactableduckdb_source_error"
  )
})
