# The backend object: an S3 list whose only mutable state lives in a
# dedicated environment (plan section 7). Separate backends have separate
# state environments, so caches can never be shared accidentally.

count_cache_capacity <- 100L
query_log_capacity <- 200L

new_backend_state <- function() {
  state <- new.env(parent = emptyenv())
  state$cache <- list() # cache_key -> count
  state$order <- character(0) # cache keys, most recently used first
  state$hits <- 0L
  state$misses <- 0L
  state$log_queries <- FALSE # test-only switch
  state$query_log <- character(0) # bounded ring, oldest first
  state$query_log_dropped <- 0L
  # Shiny session token -> list(keys =, rv =). A backend is usually shared by
  # every session, but a selection belongs to one; see R/selection.R.
  state$selection <- list()
  state
}

# Bounded LRU count cache. Excluding page/sort from the key is what lets
# paging and re-sorting reuse a count; a filter change is a different key.
cache_fetch <- function(state, key, compute) {
  if (key %in% names(state$cache)) {
    state$hits <- state$hits + 1L
    state$order <- c(key, setdiff(state$order, key))
    return(state$cache[[key]])
  }
  state$misses <- state$misses + 1L
  value <- compute()
  state$cache[[key]] <- value
  state$order <- c(key, setdiff(state$order, key))
  if (length(state$order) > count_cache_capacity) {
    evict <- utils::tail(state$order, -count_cache_capacity)
    state$order <- utils::head(state$order, count_cache_capacity)
    state$cache[evict] <- NULL
  }
  value
}

# Bounded query log: off by default, enabled by tests to assert on executed
# SQL. A ring buffer with a dropped counter — never an unbounded accumulator.
log_query <- function(state, sql) {
  if (!isTRUE(state$log_queries)) {
    return(invisible(NULL))
  }
  state$query_log <- c(state$query_log, sql)
  overflow <- length(state$query_log) - query_log_capacity
  if (overflow > 0) {
    state$query_log <- utils::tail(state$query_log, query_log_capacity)
    state$query_log_dropped <- state$query_log_dropped + overflow
  }
  invisible(NULL)
}

enable_query_log <- function(backend) {
  backend$state$log_queries <- TRUE
  invisible(backend)
}

#' Create a DuckDB-backed server backend for reactable
#'
#' Wraps a lazy DuckDB query — a duckplyr data frame or a dbplyr
#' `tbl_sql`/`tbl_dbi` — as a server-side data backend for
#' [reactable_duckdb()]. DuckDB performs all filtering, counting, sorting,
#' and pagination. Only three things are ever materialized into R: a
#' zero-row schema prototype (at construction), a scalar filtered count, and
#' the requested page of rows.
#'
#' `r lifecycle::badge("experimental")`
#'
#' The backend holds a reference to the source's DuckDB connection but never
#' owns it: it will not reconnect, refresh, or disconnect. When the
#' underlying data, query, or schema changes (for example after a pin
#' refresh), the application must construct a new backend and re-render the
#' widget — a live backend cannot detect the change.
#'
#' @param data A lazy `duckplyr_df` (e.g. from
#'   [duckplyr::read_parquet_duckdb()], ideally with `prudence = "stingy"`)
#'   or a dbplyr lazy table backed by a DuckDB connection. Eager data frames
#'   are refused.
#' @param key Optional name of a column with unique, orderable values, used
#'   as a deterministic sort tie-breaker. Uniqueness is the caller's
#'   responsibility unless `validate_key = TRUE`; a duplicated key still
#'   yields nondeterministic order between pages.
#' @param filter_spec Optional named list of per-column filter overrides.
#'   Each entry may contain `exact = TRUE` (text columns match exactly
#'   instead of by substring) and/or `type` (pin one of `"text"`,
#'   `"numeric"`, `"date"`, `"datetime"`, `"logical"`).
#' @param max_page_size Hard upper bound on rows per page. Requests beyond
#'   it are refused with an error, never clamped.
#' @param cache_counts Cache filtered row counts (bounded LRU, keyed by the
#'   canonical filter set)? Paging and re-sorting reuse a cached count;
#'   changing filters does not.
#' @param validate_key Run one scalar `COUNT(*) = COUNT(DISTINCT key)` query
#'   at construction to verify `key` is unique? Off by default because it
#'   scans the source once.
#'
#' @return A `reactable_duckdb_backend` object, to be passed to
#'   [reactable_duckdb()].
#'
#' @examplesIf requireNamespace("duckdb", quietly = TRUE) && reactableduckdb:::has_reactable_backend_api()
#' con <- DBI::dbConnect(duckdb::duckdb())
#' DBI::dbWriteTable(con, "flights", data.frame(id = 1:100, dist = rnorm(100)))
#' src <- dplyr::tbl(con, "flights")
#' backend <- reactable_duckdb_backend(src, key = "id")
#' backend
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' @export
reactable_duckdb_backend <- function(
  data,
  key = NULL,
  filter_spec = NULL,
  max_page_size = 500L,
  cache_counts = TRUE,
  validate_key = FALSE
) {
  warn_reactable_backend_api()
  max_page_size <- check_count(max_page_size, "max_page_size", min = 1)
  check_flag(cache_counts, "cache_counts")
  check_flag(validate_key, "validate_key")

  source <- as_reactable_source(data)
  con <- source_connection(source)
  check_connection(con)
  base_query <- source_query(source)
  prototype <- source_prototype(source)
  # `__state` is reactable's per-row state column (R/selection.R). A source
  # column of that name would collide with the row identity we attach, so it
  # is refused rather than silently shadowed.
  if (rowstate_column %in% names(prototype)) {
    rd_abort(
      c(
        "The source has a column named {.val {rowstate_column}}, which reactable reserves for per-row state.",
        "i" = "Rename or drop it in the source query."
      ),
      class = "schema_error"
    )
  }
  capability <- build_capability_map(prototype, filter_spec)

  key_kind <- NA_character_
  if (!is.null(key)) {
    if (!is.character(key) || length(key) != 1L || is.na(key)) {
      rd_abort(
        "{.arg key} must be a single column name.",
        class = "key_error"
      )
    }
    cap <- capability[[key]]
    if (is.null(cap)) {
      rd_abort(
        "{.arg key} column {.val {key}} is not present in the source schema.",
        class = "key_error"
      )
    }
    if (!cap$sortable) {
      rd_abort(
        "{.arg key} column {.val {key}} (type {.cls {cap$r_type}}) is not orderable and cannot be a sort tie-breaker.",
        class = "key_error"
      )
    }
    # Recorded, not required: a key of an unsupported kind is still a valid
    # sort tie-breaker. It only rules out selection, which is refused at
    # widget construction with a message naming the type.
    key_kind <- selection_key_kind(prototype[[key]])
  }

  backend <- structure(
    list(
      source = source,
      con = con,
      base_query = base_query,
      prototype = prototype,
      columns = names(prototype),
      column_types = vapply(
        prototype,
        function(col) paste(class(col), collapse = "/"),
        character(1)
      ),
      capability = capability,
      key = key,
      key_kind = key_kind,
      filter_spec = filter_spec,
      max_page_size = max_page_size,
      cache_counts = cache_counts,
      schema_hash = rlang::hash(list(
        names(prototype),
        vapply(
          prototype,
          function(col) paste(class(col), collapse = "/"),
          character(1)
        )
      )),
      state = new_backend_state()
    ),
    class = "reactable_duckdb_backend"
  )

  if (validate_key) {
    if (is.null(key)) {
      rd_abort(
        "{.arg validate_key} requires {.arg key} to be set.",
        class = "key_error"
      )
    }
    quoted_key <- quote_ident(key)
    sql <- sprintf(
      'SELECT COUNT(*) AS "n", COUNT(DISTINCT %s) AS "d" FROM (%s) AS "reactableduckdb_key"',
      quoted_key,
      base_query
    )
    counts <- run_query(backend, sql)
    if (counts$n[[1]] != counts$d[[1]]) {
      rd_abort(
        c(
          "{.arg key} column {.val {key}} is not unique: {.val {format(counts$n[[1]], scientific = FALSE)}} rows but only {.val {format(counts$d[[1]], scientific = FALSE)}} distinct values.",
          "i" = "A duplicated key cannot guarantee a deterministic page order. Choose a unique column."
        ),
        class = "key_error"
      )
    }
  }

  backend
}

#' @export
print.reactable_duckdb_backend <- function(x, ...) {
  display_only <- x$columns[!vapply(x$capability, `[[`, TRUE, "filterable")]
  cli::cli_text("{.cls reactable_duckdb_backend}")
  cli::cli_ul(c(
    "source: {.cls {class(x$source)[1]}} on {.cls {class(x$con)[1]}}",
    "columns: {length(x$columns)} ({.val {utils::head(x$columns, 6)}}{if (length(x$columns) > 6) ', ...' else ''})",
    if (length(display_only) > 0) "display-only: {.val {display_only}}",
    "key: {if (is.null(x$key)) 'none' else x$key}",
    "max_page_size: {x$max_page_size}",
    "count cache: {if (x$cache_counts) sprintf('%d hits / %d misses', x$state$hits, x$state$misses) else 'disabled'}"
  ))
  invisible(x)
}
