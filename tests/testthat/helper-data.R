# Shared fixtures. All sources are DuckDB-backed and substantially larger
# than one page. Hostile text values sit at known ids so filter tests can
# assert exact matches.

skip_if_no_backend_api <- function() {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("reactable")
  if (!reactableduckdb:::has_reactable_backend_api()) {
    skip("installed reactable lacks the custom server backend API")
  }
}

hostile_names <- c(
  "O'Brien",
  "100%",
  "a_b",
  'say "hi"',
  "back\\slash",
  "-- comment",
  "'; DROP TABLE x; --",
  "10..20"
)

# One statement builds the standard table: wide type spread, NULLs in grp,
# hostile strings at ids 0..7, a LIST column that must be display-only.
test_table_sql <- function(n) {
  hostile_sql <- paste(
    sprintf(
      "WHEN i = %d THEN '%s'",
      seq_along(hostile_names) - 1,
      gsub("'", "''", hostile_names, fixed = TRUE)
    ),
    collapse = " "
  )
  sprintf(
    "SELECT i AS id,
            CASE %s ELSE 'name_' || (i %% 97) END AS name,
            CASE WHEN i %% 13 = 0 THEN NULL ELSE 'grp_' || (i %% 7) END AS grp,
            (i %% 100)::INTEGER AS qty,
            (i * 1.5)::DOUBLE AS amount,
            (i %% 10000)::DECIMAL(18,4) AS price,
            DATE '2026-01-01' + (i %% 365)::INTEGER AS dt,
            TIMESTAMP '2026-01-01 00:00:00'
              + ((i %% 365)::INTEGER * INTERVAL 1 HOUR) AS ts,
            (i %% 2 = 0) AS flag,
            [i, i + 1] AS tags
     FROM range(%d) r(i)",
    hostile_sql,
    n
  )
}

local_duckdb_con <- function(env = parent.frame()) {
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = env)
  con
}

local_duckdb_tbl <- function(n = 2000, env = parent.frame()) {
  con <- local_duckdb_con(env = env)
  DBI::dbExecute(con, paste("CREATE TABLE test_data AS", test_table_sql(n)))
  dplyr::tbl(con, "test_data")
}

local_backend <- function(n = 2000, ..., env = parent.frame()) {
  reactable_duckdb_backend(local_duckdb_tbl(n = n, env = env), ...)
}

# Non-syntactic column names: spaces, dots, leading digits, unicode,
# reserved words, embedded quotes. The seam between JSON ids,
# quote_ident(), and !!rlang::sym().
local_weird_tbl <- function(n = 500, env = parent.frame()) {
  con <- local_duckdb_con(env = env)
  DBI::dbExecute(con, sprintf(
    'CREATE TABLE weird AS
     SELECT i AS "order id",
            (i %% 50)::INTEGER AS "1st value",
            \'v\' || (i %% 9) AS "café",
            (i %% 3)::INTEGER AS "select",
            \'x\' || i AS "we""ird",
            \'d\' || i AS "dot.ted"
     FROM range(%d) r(i)',
    n
  ))
  dplyr::tbl(con, "weird")
}

# A stingy duckplyr source over temp parquet, plus its file path.
local_duckplyr_source <- function(n = 2000, env = parent.frame()) {
  skip_if_not_installed("duckplyr")
  dir <- withr::local_tempdir(.local_envir = env)
  path <- file.path(dir, "src.parquet")
  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con, sprintf(
    "COPY (%s) TO '%s' (FORMAT PARQUET)",
    test_table_sql(n),
    path
  ))
  DBI::dbDisconnect(con, shutdown = TRUE)
  duckplyr::read_parquet_duckdb(path, prudence = "stingy")
}

server_data <- function(backend, pageIndex = 0, pageSize = 10, ...) {
  reactable::reactableServerData(
    backend,
    pageIndex = pageIndex,
    pageSize = pageSize,
    ...
  )
}

# Reference copy for computing expected orders/counts in R.
collect_reference <- function(backend) {
  DBI::dbGetQuery(backend$con, backend$base_query)
}

query_log <- function(backend) backend$state$query_log

last_page_sql <- function(backend) {
  log <- query_log(backend)
  pages <- log[grepl("reactableduckdb_base", log, fixed = TRUE)]
  pages[[length(pages)]]
}

last_count_sql <- function(backend) {
  log <- query_log(backend)
  counts <- log[grepl("reactableduckdb_count", log, fixed = TRUE)]
  counts[[length(counts)]]
}
