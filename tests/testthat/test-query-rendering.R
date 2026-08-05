# The seam between the lazy dbplyr query and the raw-SQL wrapper. Both
# functions are exercised indirectly by every request test; these pin their
# contracts directly, because "no predicates leaves the query untouched" is
# what keeps an unfiltered count identical to the base query.

test_that("render_sql returns a single SQL string, not an SQL object", {
  skip_if_no_backend_api()
  tbl <- local_duckdb_tbl(n = 20)
  sql <- reactableduckdb:::render_sql(tbl)
  # duckdb_page_sql() interpolates this with sprintf(), which needs a plain
  # character scalar rather than a dbplyr `sql` vector.
  expect_type(sql, "character")
  expect_length(sql, 1L)
  expect_match(sql, "SELECT")
})

test_that("no predicates leaves the query object untouched", {
  skip_if_no_backend_api()
  tbl <- local_duckdb_tbl(n = 20)
  # Identical, not merely equivalent: an empty dplyr::filter() would still add
  # a subquery layer, and the unfiltered count must be the base query's.
  expect_identical(reactableduckdb:::apply_predicates(tbl, list()), tbl)
  expect_identical(
    reactableduckdb:::render_sql(reactableduckdb:::apply_predicates(tbl, list())),
    reactableduckdb:::render_sql(tbl)
  )
})

test_that("predicates are applied lazily and compose", {
  skip_if_no_backend_api()
  tbl <- local_duckdb_tbl(n = 200)
  predicates <- list(
    rlang::call2(">=", rlang::sym("qty"), 50),
    rlang::call2("<", rlang::sym("qty"), 60)
  )
  filtered <- reactableduckdb:::apply_predicates(tbl, predicates)
  expect_s3_class(filtered, "tbl_lazy")

  sql <- reactableduckdb:::render_sql(filtered)
  expect_match(sql, "WHERE")
  # Both predicates survive, ANDed -- not just the last one.
  expect_match(sql, "qty")
  collected <- DBI::dbGetQuery(dbplyr::remote_con(tbl), sql)
  expect_true(all(collected$qty >= 50 & collected$qty < 60))
  expect_gt(nrow(collected), 0)
})
