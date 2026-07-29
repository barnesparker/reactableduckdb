# Structured error classes and input validators. Every condition this package
# raises carries a `reactableduckdb_<class>` class plus the common
# `reactableduckdb_error` parent, so callers and tests can catch precisely.

rd_abort <- function(message, class, ..., call = rlang::caller_env(),
                     .envir = rlang::caller_env()) {
  cli::cli_abort(
    message,
    class = c(paste0("reactableduckdb_", class), "reactableduckdb_error"),
    ...,
    call = call,
    .envir = .envir
  )
}

check_backend <- function(x, arg = "backend", call = rlang::caller_env()) {
  if (!inherits(x, "reactable_duckdb_backend")) {
    rd_abort(
      "{.arg {arg}} must be a {.cls reactable_duckdb_backend}, not {.obj_type_friendly {x}}.",
      class = "source_error",
      call = call
    )
  }
  invisible(x)
}

# Before every query: an invalid connection means the app must rebuild the
# backend. Never reconnect automatically.
check_connection <- function(con, call = rlang::caller_env()) {
  if (!DBI::dbIsValid(con)) {
    rd_abort(
      c(
        "The backend's DuckDB connection is no longer valid.",
        "i" = "The connection was closed or the source was released.",
        "i" = "Rebuild the backend with {.fn reactable_duckdb_backend} from a live source. reactableduckdb never reconnects automatically."
      ),
      class = "connection_error",
      call = call
    )
  }
  invisible(con)
}

is_whole_number <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) && x == trunc(x)
}

check_count <- function(x, arg, min = 1, call = rlang::caller_env()) {
  if (!is_whole_number(x) || x < min) {
    rd_abort(
      "{.arg {arg}} must be a single whole number >= {min}, not {.obj_type_friendly {x}}.",
      class = "page_error",
      call = call
    )
  }
  invisible(as.double(x))
}

check_flag <- function(x, arg, call = rlang::caller_env()) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    rd_abort(
      "{.arg {arg}} must be `TRUE` or `FALSE`.",
      class = "source_error",
      call = call
    )
  }
  invisible(x)
}

# Validates a page request. Never clamps: an oversized or malformed request is
# refused with a structured error. Returns limit/offset as exact doubles.
check_page_params <- function(page_index, page_size, max_page_size,
                              call = rlang::caller_env()) {
  if (!is_whole_number(page_index) || page_index < 0) {
    rd_abort(
      "{.arg pageIndex} must be a single whole number >= 0, not {.obj_type_friendly {page_index}}.",
      class = "page_error",
      call = call
    )
  }
  if (!is_whole_number(page_size) || page_size < 1) {
    rd_abort(
      "{.arg pageSize} must be a single whole number >= 1, not {.obj_type_friendly {page_size}}.",
      class = "page_error",
      call = call
    )
  }
  if (page_size > max_page_size) {
    rd_abort(
      c(
        "{.arg pageSize} ({page_size}) exceeds the backend's {.arg max_page_size} ({max_page_size}).",
        "i" = "Oversized page requests are refused, never clamped. Raise {.arg max_page_size} in {.fn reactable_duckdb_backend} if larger pages are intended."
      ),
      class = "page_error",
      call = call
    )
  }
  offset <- as.double(page_index) * as.double(page_size)
  # 2^53 is the largest integer a double represents exactly; beyond it the
  # OFFSET arithmetic is no longer trustworthy.
  if (offset > 2^53) {
    rd_abort(
      "The requested page offset ({.val {page_index}} * {.val {page_size}}) overflows exact integer arithmetic.",
      class = "page_error",
      call = call
    )
  }
  list(limit = as.double(page_size), offset = offset)
}

# Column-id validation against the schema and capability map. `purpose` is
# "filter" or "sort"; display-only columns are refused for both.
check_column_id <- function(id, capability, purpose, call = rlang::caller_env()) {
  if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
    rd_abort(
      "Each {purpose} request must name a column with a single non-empty string id.",
      class = paste0(purpose, "_error"),
      call = call
    )
  }
  cap <- capability[[id]]
  if (is.null(cap)) {
    rd_abort(
      "Unknown {purpose} column {.val {id}}: not present in the source schema.",
      class = paste0(purpose, "_error"),
      call = call
    )
  }
  refused <- if (purpose == "sort") !cap$sortable else !cap$filterable
  if (refused) {
    rd_abort(
      c(
        "Column {.val {id}} (type {.cls {cap$r_type}}) is display-only and cannot be used in a {purpose}.",
        "i" = "Nested and opaque types (LIST, STRUCT, MAP, BLOB) render but are not filterable or sortable in v1."
      ),
      class = paste0(purpose, "_error"),
      call = call
    )
  }
  invisible(cap)
}
