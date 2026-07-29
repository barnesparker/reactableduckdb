# The single isolated raw-SQL wrapper (plan section 11). Everything that
# reaches this file is either schema-validated (column ids), keyword-only
# (ASC/DESC NULLS LAST), or an exact validated integer (LIMIT/OFFSET).
# Filter *values* never enter here: they are bound inside the dbplyr-rendered
# base query upstream.

# Unconditional ANSI identifier quoting (embedded quotes doubled).
# duckdb's dbQuoteIdentifier() leaves "simple-looking" identifiers bare and
# misjudges names like `1st` (a parser error in ORDER BY, empirically), so
# the safety path never relies on its heuristic.
quote_ident <- function(id) {
  paste0('"', gsub('"', '""', id, fixed = TRUE), '"')
}

# Build "ORDER BY"-ready clauses from a validated sortBy request.
# `sort_by` is a list of list(id =, desc =) in reactable priority order —
# the shape reactable uses both at construction (columnSortDefs) and in
# parsed JSON requests. Every id is re-validated here (defense in depth) and
# quoted unconditionally. When `key` is supplied and not already sorted, it
# is appended ascending as a deterministic tie-breaker.
build_order_clauses <- function(sort_by, key, capability,
                                call = rlang::caller_env()) {
  sort_by <- sort_by %||% list()
  if (!is.list(sort_by)) {
    rd_abort(
      "{.arg sortBy} must be a list of {.code list(id =, desc =)} entries.",
      class = "sort_error",
      call = call
    )
  }
  clauses <- character(0)
  sorted_ids <- character(0)
  for (entry in sort_by) {
    if (!is.list(entry) || is.null(entry$id)) {
      rd_abort(
        "{.arg sortBy} must be a list of {.code list(id =, desc =)} entries.",
        class = "sort_error",
        call = call
      )
    }
    check_column_id(entry$id, capability, purpose = "sort", call = call)
    desc <- isTRUE(entry$desc)
    quoted <- quote_ident(entry$id)
    direction <- if (desc) "DESC" else "ASC"
    # Explicit NULLS LAST in both directions: deterministic, and independent
    # of DuckDB's default null ordering.
    clauses <- c(clauses, paste(quoted, direction, "NULLS LAST"))
    sorted_ids <- c(sorted_ids, entry$id)
  }
  if (!is.null(key) && !key %in% sorted_ids) {
    clauses <- c(clauses, paste(quote_ident(key), "ASC NULLS LAST"))
  }
  clauses
}

# Pure string builder for the page query. `base_sql` is the dbplyr-rendered
# filtered query (values already bound/escaped by dbplyr); `order_clauses`
# come from build_order_clauses(); limit/offset are validated exact whole
# numbers. Formatted without scientific notation so large offsets stay legal
# SQL.
duckdb_page_sql <- function(base_sql, order_clauses, limit, offset,
                            call = rlang::caller_env()) {
  if (!is.character(base_sql) || length(base_sql) != 1L || !nzchar(base_sql)) {
    rd_abort(
      "{.arg base_sql} must be a single non-empty SQL string.",
      class = "page_error",
      call = call
    )
  }
  if (!is_whole_number(limit) || limit < 1) {
    rd_abort(
      "{.arg limit} must be a single whole number >= 1.",
      class = "page_error",
      call = call
    )
  }
  if (!is_whole_number(offset) || offset < 0 || offset > 2^53) {
    rd_abort(
      "{.arg offset} must be a single whole number between 0 and 2^53.",
      class = "page_error",
      call = call
    )
  }
  order_sql <- if (length(order_clauses) > 0) {
    paste0(" ORDER BY ", paste(order_clauses, collapse = ", "))
  } else {
    ""
  }
  sprintf(
    'SELECT * FROM (%s) AS "reactableduckdb_base"%s LIMIT %s OFFSET %s',
    base_sql,
    order_sql,
    format(limit, scientific = FALSE),
    format(offset, scientific = FALSE)
  )
}

# Scalar-count SQL over the filtered base query. Wrapping as a subquery is
# what makes the count correct for aggregated (summarise) sources too.
duckdb_count_sql <- function(base_sql) {
  sprintf(
    'SELECT COUNT(*) AS "n" FROM (%s) AS "reactableduckdb_count"',
    base_sql
  )
}

# The only execution path for raw SQL in the package. Verifies the
# connection, records the SQL in the backend's bounded query log, executes.
run_query <- function(backend, sql, call = rlang::caller_env()) {
  check_connection(backend$con, call = call)
  log_query(backend$state, sql)
  DBI::dbGetQuery(backend$con, sql)
}
