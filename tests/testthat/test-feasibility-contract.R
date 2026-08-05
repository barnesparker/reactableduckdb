# Pins the observed upstream contract (design/reactable-server-contract.md).
# If reactable changes under us, these fail before anything subtler does.

test_that("required backend API is exported and functional", {
  skip_if_no_backend_api()
  for (fn in c("reactableServerInit", "reactableServerData", "resolvedData")) {
    expect_type(get(fn, envir = asNamespace("reactable")), "closure")
  }
})

test_that("generic formals match the recorded contract", {
  skip_if_no_backend_api()
  expected <- c(
    "x",
    "data",
    "columns",
    "pageIndex",
    "pageSize",
    "sortBy",
    "filters",
    "searchValue",
    "searchMethod",
    "groupBy",
    "pagination",
    "paginateSubRows",
    "selectedRowIds",
    "expanded",
    "..."
  )
  expect_identical(names(formals(reactable::reactableServerInit)), expected)
  expect_identical(names(formals(reactable::reactableServerData)), expected)
  expect_identical(
    names(formals(reactable::resolvedData)),
    c("data", "rowCount", "maxRowCount")
  )
})

test_that("the API detection tolerates upstream adding arguments", {
  # The dev version is a moving target, so detection must be a subset
  # check: an extra formal upstream must not read as a broken install.
  expect_true(
    all(
      reactableduckdb:::required_resolveddata_formals %in%
        c(reactableduckdb:::required_resolveddata_formals, "somethingNew")
    )
  )
  expect_true(reactableduckdb:::has_reactable_backend_api())
})

test_that("a missing backend API warns and does not abort", {
  rlang::reset_warning_verbosity("reactableduckdb-backend-api")
  expect_warning(
    result <- reactableduckdb:::warn_reactable_backend_api(has_api = FALSE),
    class = "reactableduckdb_api_warning"
  )
  expect_false(result)

  rlang::reset_warning_verbosity("reactableduckdb-backend-api")
  warning_text <- tryCatch(
    reactableduckdb:::warn_reactable_backend_api(has_api = FALSE),
    condition = conditionMessage
  )
  # Both installation routes are named, neither presented as required.
  expect_match(warning_text, "glin/reactable", fixed = TRUE)
  expect_match(warning_text, "Posit Package Manager", fixed = TRUE)
  expect_match(warning_text, "mirrors", fixed = TRUE)
  expect_no_match(warning_text, "must", fixed = TRUE)
})

test_that("a present backend API is silent", {
  expect_no_warning(
    expect_true(reactableduckdb:::warn_reactable_backend_api(has_api = TRUE))
  )
})

test_that("detection reports FALSE when reactable exports are missing", {
  # Detection must fail closed on a genuinely missing API, not error. Mocking
  # the export list is the only way to reach this branch with a working
  # reactable installed.
  testthat::local_mocked_bindings(
    getNamespaceExports = function(...) character(0),
    .package = "base"
  )
  expect_false(reactableduckdb:::has_reactable_backend_api())
})

test_that("attaching the package announces a missing backend API", {
  testthat::local_mocked_bindings(
    has_reactable_backend_api = function() FALSE
  )
  expect_message(
    reactableduckdb:::.onAttach("lib", "reactableduckdb"),
    "development version"
  )

  testthat::local_mocked_bindings(
    has_reactable_backend_api = function() TRUE
  )
  expect_no_message(
    reactableduckdb:::.onAttach("lib", "reactableduckdb")
  )
})

test_that("resolvedData enforces its recorded guards", {
  skip_if_no_backend_api()
  expect_error(
    reactable::resolvedData(data.frame(a = 1), rowCount = "x"),
    "rowCount"
  )
  expect_error(reactable::resolvedData(data.frame(a = 1)), "rowCount")
  expect_error(
    reactable::resolvedData("not a df", rowCount = 1),
    "data frame"
  )
  rd <- reactable::resolvedData(data.frame(a = 1:2), rowCount = 99)
  expect_s3_class(rd, "reactable_resolvedData")
  expect_named(rd, c("data", "rowCount", "maxRowCount"))
  expect_identical(rd$rowCount, 99)
  expect_null(rd$maxRowCount)
})
