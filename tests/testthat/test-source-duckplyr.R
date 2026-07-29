# Stingy duckplyr sources. Gate finding D1: stingy does NOT block collect()
# — it blocks automatic materialization (nrow/dim) and dplyr fallbacks
# (e.g. group_by). The no-materialization invariant is evidenced by the
# query log and allocation ceiling in test-source-dbplyr.R, not by stingy.

test_that("a stingy source blocks auto-materialization but not collect()", {
  skip_if_no_backend_api()
  src <- local_duckplyr_source(n = 2000)
  expect_error(nrow(src))
  expect_error(dim(src))
  # D1, pinned: collect() is the sanctioned escape hatch and succeeds.
  expect_identical(nrow(dplyr::collect(utils::head(src, 5))), 5L)
})

test_that("group_by is refused under stingy; summarise(.by=) stays lazy", {
  skip_if_no_backend_api()
  src <- local_duckplyr_source(n = 2000)
  expect_error(dplyr::group_by(src, grp))
  aggregated <- src |>
    dplyr::filter(qty > 5, !is.na(grp)) |>
    dplyr::mutate(revenue = amount * qty) |>
    dplyr::summarise(n = dplyr::n(), total = sum(revenue), .by = grp)
  expect_s3_class(aggregated, "duckplyr_df")
})

test_that("a backend builds from a stingy source without eager collection", {
  skip_if_no_backend_api()
  src <- local_duckplyr_source(n = 2000)
  lazy <- src |>
    dplyr::filter(qty >= 0) |>
    dplyr::mutate(revenue = amount * qty)
  b <- reactable_duckdb_backend(lazy, key = "id")
  expect_identical(nrow(b$prototype), 0L)
  expect_contains(b$columns, "revenue")

  rd <- server_data(b, pageSize = 25)
  expect_identical(nrow(rd$data), 25L)
  expect_identical(rd$rowCount, 2000)
  # The source is much larger than one page.
  expect_gt(rd$rowCount, 10 * nrow(rd$data))
})

test_that("an aggregated stingy pipeline paginates and counts correctly", {
  skip_if_no_backend_api()
  src <- local_duckplyr_source(n = 2000)
  aggregated <- src |>
    dplyr::filter(!is.na(grp)) |>
    dplyr::summarise(
      n = dplyr::n(),
      total = sum(amount),
      .by = c(grp, flag)
    )
  b <- reactable_duckdb_backend(aggregated, key = "grp")
  rd <- server_data(b, pageSize = 5, sortBy = list(list(id = "total", desc = TRUE)))
  expect_identical(rd$rowCount, 14)
  expect_identical(nrow(rd$data), 5L)
  expect_identical(order(rd$data$total, decreasing = TRUE), 1:5)
})

test_that("prototype schema is fully data-driven across shapes", {
  skip_if_no_backend_api()
  src <- local_duckplyr_source(n = 100)
  narrow <- src |> dplyr::select(dt, id, name)
  b <- reactable_duckdb_backend(narrow)
  expect_identical(b$columns, c("dt", "id", "name"))
  expect_identical(unname(b$column_types[["dt"]]), "Date")
})
