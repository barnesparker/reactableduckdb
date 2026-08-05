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

# Tests that spawn an app with callr::r_bg() need the package inside a fresh R
# session, and it must be the working source tree -- an installed copy would
# make them pass or fail for code that is not the code under test.
#
# Under R CMD check there is no source tree to load: tests run from the
# .Rcheck directory, and the only available copy is the installed build. The
# honest options there are to load that build or to skip. Loading it would
# mean a `library(reactableduckdb)` in the suite, which is precisely the shape
# that silently accepted a stale install during development, so these tests
# skip instead. CI runs `make test` against the checked-out tree, where they
# do run. See the "Validating against the source tree" section of CLAUDE.md.
#
# A DESCRIPTION two levels up is NOT enough to prove a source tree: an
# *installed* package directory has one too, and covr runs the suite from
# `<lib>/reactableduckdb/reactableduckdb-tests/testthat/`, which puts an
# installed package root exactly there. Handing that to pkgload::load_all()
# would kill the spawned app process for the wrong reason. A source tree also
# has R/*.R; an installed one has only R/<pkg>.rdb and friends, so that is the
# discriminator.
has_source_tree <- function(dir) {
  !is.na(dir) &&
    file.exists(file.path(dir, "DESCRIPTION")) &&
    length(list.files(file.path(dir, "R"), pattern = "[.][Rr]$")) > 0
}

skip_without_source_tree <- function() {
  dir <- tryCatch(
    normalizePath(testthat::test_path("..", ".."), mustWork = FALSE),
    error = function(cnd) NA_character_
  )
  skip_if(
    !has_source_tree(dir),
    "no source tree here (R CMD check / covr); `make test` exercises this"
  )
  dir
}

# The browser-driven tests are the only check that reactable's undocumented
# `__state` row-identity hook still exists. If it disappears upstream, every
# R-level test still passes while the client silently reverts to identifying
# rows by their position in the delivered page.
#
# So a missing Chrome is a skip on a developer machine, but a *failure* on CI:
# a test with seven independent ways to skip itself needs at least one place
# where it cannot go quietly dark. CI is that place.
on_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI", "false")))
}

local_chromote_session <- function(env = parent.frame()) {
  session <- tryCatch(
    chromote::ChromoteSession$new(),
    error = function(cnd) cnd
  )
  if (inherits(session, "condition")) {
    reason <- conditionMessage(session)
    if (on_ci()) {
      stop(
        "chromote could not start a Chrome session on CI, so the browser ",
        "tests would have skipped silently. These tests are load-bearing: ",
        "they are the only check that reactable still honours the `__state` ",
        "row-identity hook. Original error: ",
        reason,
        call. = FALSE
      )
    }
    skip(paste0("chromote could not start a Chrome session: ", reason))
  }
  withr::defer(session$close(), envir = env)
  session
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

# duckdb >= 1.5.5 prints a one-time note on the first connection of a session
# saying where it will keep downloaded extensions, unless it is told which home
# to use. It is harmless, but it lands in whichever expect_snapshot() happens to
# open that first connection -- so which test it breaks depends on test order,
# and it never appears on a machine that already has a ~/.duckdb. Silence it at
# the source. `shared_home` does not exist before 1.5.5, which is also the
# version that introduced the message, so the guard covers both.
local_duckdb_con <- function(env = parent.frame()) {
  args <- list()
  if ("shared_home" %in% names(formals(duckdb::duckdb))) {
    args$shared_home <- FALSE
  }
  con <- DBI::dbConnect(do.call(duckdb::duckdb, args))
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
#
# One of the names is non-ASCII, so this fixture needs a UTF-8 locale. Under
# a C locale R cannot represent the name at all: DuckDB's `café` comes back
# as the 11-character literal "caf<c3><a9>", which then matches nothing. That
# is the locale's limitation rather than a defect in the package, so the
# fixture skips instead of reporting a failure it cannot act on.
local_weird_tbl <- function(n = 500, env = parent.frame()) {
  skip_if_not(
    isTRUE(l10n_info()[["UTF-8"]]),
    "non-ASCII column names need a UTF-8 locale"
  )
  con <- local_duckdb_con(env = env)
  DBI::dbExecute(
    con,
    sprintf(
      'CREATE TABLE weird AS
     SELECT i AS "order id",
            (i %% 50)::INTEGER AS "1st value",
            \'v\' || (i %% 9) AS "café",
            (i %% 3)::INTEGER AS "select",
            \'x\' || i AS "we""ird",
            \'d\' || i AS "dot.ted"
     FROM range(%d) r(i)',
      n
    )
  )
  dplyr::tbl(con, "weird")
}

# A stingy duckplyr source over temp parquet, plus its file path.
local_duckplyr_source <- function(n = 2000, env = parent.frame()) {
  skip_if_not_installed("duckplyr")
  dir <- withr::local_tempdir(.local_envir = env)
  path <- file.path(dir, "src.parquet")
  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(
    con,
    sprintf(
      "COPY (%s) TO '%s' (FORMAT PARQUET)",
      test_table_sql(n),
      path
    )
  )
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
