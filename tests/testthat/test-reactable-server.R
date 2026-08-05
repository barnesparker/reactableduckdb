# Direct S3-method tests with request objects shaped per the observed
# upstream contract, plus construction-time behaviour through reactable().

test_that("initial page request returns resolvedData with the full count", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000, key = "id")
  rd <- server_data(b, pageIndex = 0, pageSize = 50)
  expect_s3_class(rd, "reactable_resolvedData")
  expect_identical(nrow(rd$data), 50L)
  expect_type(rd$rowCount, "double")
  expect_identical(rd$rowCount, 2000)
  expect_null(rd$maxRowCount)
})

test_that("page transitions keep the count and change the rows", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000, key = "id")
  p0 <- server_data(b, pageIndex = 0, pageSize = 20)
  p1 <- server_data(b, pageIndex = 1, pageSize = 20)
  expect_identical(p1$rowCount, p0$rowCount)
  expect_identical(intersect(p0$data$id, p1$data$id), numeric(0))
})

test_that("filter + multi-sort + pagination compose in one request", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000, key = "id")
  reference <- collect_reference(b)
  rd <- server_data(
    b,
    pageIndex = 1,
    pageSize = 10,
    sortBy = list(
      list(id = "grp", desc = FALSE),
      list(id = "amount", desc = TRUE)
    ),
    filters = list(list(id = "qty", value = ">=50"))
  )
  subset <- reference[reference$qty >= 50, ]
  expected <- subset[
    order(subset$grp, -subset$amount, subset$id, na.last = TRUE),
  ]
  expect_equal(rd$rowCount, nrow(subset))
  expect_identical(rd$data$id, expected$id[11:20])
})

test_that("global search and grouping requests are refused defensively", {
  skip_if_no_backend_api()
  b <- local_backend(n = 100)
  expect_error(
    server_data(b, searchValue = "find me"),
    class = "reactableduckdb_unsupported_arg"
  )
  expect_error(
    server_data(b, groupBy = list("grp")),
    class = "reactableduckdb_unsupported_arg"
  )
  # Empty values are not requests.
  expect_s3_class(
    server_data(b, searchValue = "", groupBy = list()),
    "reactable_resolvedData"
  )
})

test_that("an invalidated connection raises a rebuild error", {
  skip_if_no_backend_api()
  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con, paste("CREATE TABLE test_data AS", test_table_sql(100)))
  b <- reactable_duckdb_backend(dplyr::tbl(con, "test_data"))
  expect_s3_class(server_data(b), "reactable_resolvedData")
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_error(server_data(b), class = "reactableduckdb_connection_error")
  expect_error(
    reactable::reactableServerInit(b, pageIndex = 0, pageSize = 10),
    class = "reactableduckdb_connection_error"
  )
})

test_that("request failures are logged with ids only, then re-raised", {
  skip_if_no_backend_api()
  b <- local_backend(n = 100)
  messages <- character(0)
  withCallingHandlers(
    expect_error(
      server_data(
        b,
        filters = list(
          list(id = "name", value = "secret-value"),
          list(id = "nope", value = "x")
        )
      ),
      class = "reactableduckdb_filter_error"
    ),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  log_text <- paste(messages, collapse = "\n")
  expect_match(log_text, "reactableduckdb request failed")
  expect_match(log_text, "filter_ids=\\[name,nope\\]")
  expect_no_match(log_text, "secret-value")
})

# Construction through reactable() (the §4b surface) --------------------------

test_that("reactable() construction yields a real first page and count", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000, key = "id")
  w <- reactable_duckdb(b, filterable = TRUE, defaultPageSize = 25L)
  attribs <- w$x$tag$attribs
  expect_identical(attribs$serverRowCount, 2000)
  embedded <- jsonlite::fromJSON(as.character(attribs$data))
  expect_length(embedded$id, 25)
})

test_that("display-only columns get filterable/sortable FALSE automatically", {
  skip_if_no_backend_api()
  b <- local_backend(n = 100)
  w <- reactable_duckdb(
    b,
    filterable = TRUE,
    sortable = TRUE,
    defaultColDef = reactable::colDef(filterable = TRUE)
  )
  cols <- w$x$tag$attribs$columns
  tags_col <- Filter(function(col) identical(col$id, "tags"), cols)[[1]]
  expect_false(tags_col$filterable)
  expect_false(tags_col$sortable)
})

test_that("a fully filterable schema passes no columns list at all", {
  skip_if_no_backend_api()
  con <- local_duckdb_con()
  # No LIST/STRUCT column, so nothing is display-only and there is nothing for
  # the package to say about any column. It must then leave `columns` alone
  # rather than sending reactable an empty list.
  DBI::dbExecute(
    con,
    "CREATE TABLE plain AS SELECT i AS id, 'n' || i AS name FROM range(20) r(i)"
  )
  b <- reactable_duckdb_backend(dplyr::tbl(con, "plain"), key = "id")
  expect_null(reactableduckdb:::build_widget_columns(b, NULL))
  w <- reactable_duckdb(b, filterable = TRUE)
  expect_s3_class(w, "reactable")
})

test_that("cosmetic user colDefs merge over capability defaults", {
  skip_if_no_backend_api()
  b <- local_backend(n = 100)
  w <- reactable_duckdb(
    b,
    filterable = TRUE,
    columns = list(
      tags = reactable::colDef(name = "Tag list"),
      qty = reactable::colDef(name = "Quantity")
    )
  )
  cols <- w$x$tag$attribs$columns
  tags_col <- Filter(function(col) identical(col$id, "tags"), cols)[[1]]
  expect_identical(tags_col$name, "Tag list")
  expect_false(tags_col$filterable)
  qty_col <- Filter(function(col) identical(col$id, "qty"), cols)[[1]]
  expect_identical(qty_col$name, "Quantity")
  expect_null(qty_col$filterable)
})

test_that("explicitly re-enabling filter/sort on display-only errors", {
  skip_if_no_backend_api()
  b <- local_backend(n = 100)
  expect_error(
    reactable_duckdb(
      b,
      columns = list(tags = reactable::colDef(filterable = TRUE))
    ),
    class = "reactableduckdb_unsupported_arg"
  )
  expect_error(
    reactable_duckdb(
      b,
      columns = list(tags = reactable::colDef(sortable = TRUE))
    ),
    class = "reactableduckdb_unsupported_arg"
  )
  # An explicit FALSE is redundant but allowed.
  w <- reactable_duckdb(
    b,
    columns = list(tags = reactable::colDef(filterable = FALSE))
  )
  expect_s3_class(w, "reactable")
})

test_that("columns must name real schema columns with real colDefs", {
  skip_if_no_backend_api()
  b <- local_backend(n = 100)
  expect_error(
    reactable_duckdb(b, columns = list(nope = reactable::colDef(name = "X"))),
    class = "reactableduckdb_unsupported_arg"
  )
  expect_error(
    reactable_duckdb(b, columns = list(qty = list(name = "X"))),
    class = "reactableduckdb_unsupported_arg"
  )
  expect_error(
    reactable_duckdb(b, columns = list(reactable::colDef(name = "X"))),
    class = "reactableduckdb_unsupported_arg"
  )
})

test_that("defaultSorted is validated against capability at construction", {
  skip_if_no_backend_api()
  b <- local_backend(n = 100, key = "id")
  w <- reactable_duckdb(b, defaultSorted = "qty")
  expect_s3_class(w, "reactable")
  expect_error(
    reactable_duckdb(b, defaultSorted = "tags"),
    class = "reactableduckdb_sort_error"
  )
})
