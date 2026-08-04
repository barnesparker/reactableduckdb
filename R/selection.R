# Row identity and row selection.
#
# reactable's client identifies a row by a string id. With a server backend
# and no help from us it falls back to the row's index *within the delivered
# page*, so "row 2" means a different row on every page: selection state then
# bleeds across pages and marks rows the user never chose (observed; see
# design/selection-contract.md section 1).
#
# The client honours a per-row state column named `__state` for server
# backends: `getRowId()` reads `row.__state.id`, and the server-side rows hook
# assigns `row.index` from `__state.index`. Supplying it is what makes a row
# id mean the same row on every page. This is an undocumented upstream
# extension point, so the browser test in test-selection.R is load-bearing —
# if it disappears upstream, identity silently reverts to page indices.

rowstate_column <- "__state"

# Key types we can round-trip between an R value and the client's string row
# id. Anything else is refused at construction rather than producing ids we
# cannot map back to rows.
selection_key_kind <- function(col) {
  if (is.character(col) || is.factor(col)) {
    "character"
  } else if (inherits(col, "integer64")) {
    "integer64"
  } else if (is.integer(col)) {
    "integer"
  } else if (is.numeric(col)) {
    "double"
  } else {
    NA_character_
  }
}

# Format key values as the string row ids the client will use.
format_row_ids <- function(values, kind, call = rlang::caller_env()) {
  switch(
    kind,
    character = as.character(values),
    integer = as.character(values),
    integer64 = as.character(values),
    double = format_double_row_ids(values, call = call),
    rd_abort("Unsupported key kind {.val {kind}}.", class = "selection_error")
  )
}

# Doubles are the common case, not an exotic one: DuckDB BIGINT arrives in R
# as a double, so most integer keys land here.
#
# They are formatted as plain integers rather than with a general float
# format, because no decimal float format round-trips reliably in R --
# as.character() keeps 15 significant digits, and even "%.17g" fails on a
# substantial fraction of doubles because as.numeric() does not correctly
# round every 17-digit input (measured, not assumed). Whole numbers up to 2^53
# have exact decimal forms, so this path is exact for every value it accepts,
# and refuses the rest.
#
# Refusing a fractional key is the right outcome regardless: a row identity
# compared by floating-point equality is a defect, not a configuration. 2^53 is
# the same exactness boundary the package already applies to page offsets.
format_double_row_ids <- function(values, call = rlang::caller_env()) {
  present <- values[!is.na(values)]
  bad <- !is.finite(present) | present != trunc(present) | abs(present) > 2^53
  if (any(bad)) {
    rd_abort(
      c(
        "The {.arg key} column has values that cannot be a row identity: {.val {utils::head(present[bad], 3)}}.",
        "i" = "A key stored as a double must hold whole numbers no larger than 2^53, so that its row ids survive the round-trip through the browser exactly.",
        "*" = "Use an integer, integer64, or character key column."
      ),
      class = "selection_error",
      call = call
    )
  }
  ids <- sprintf("%.0f", values)
  ids[is.na(values)] <- NA_character_
  ids
}

# Invert format_row_ids(). Ids that do not parse are a client/schema
# mismatch, not user input, so they are refused rather than dropped.
parse_row_ids <- function(ids, kind, call = rlang::caller_env()) {
  ids <- as.character(ids)
  if (
    identical(kind, "integer64") && !requireNamespace("bit64", quietly = TRUE)
  ) {
    rd_abort(
      c(
        "The {.arg key} column is {.cls integer64}, which needs the {.pkg bit64} package to read row ids back.",
        "i" = "Install {.pkg bit64}, or use an integer, double, or character key column."
      ),
      class = "selection_error",
      call = call
    )
  }
  if (length(ids) == 0) {
    return(switch(
      kind,
      character = character(0),
      integer = integer(0),
      double = numeric(0),
      integer64 = bit64::as.integer64(character(0))
    ))
  }
  values <- switch(
    kind,
    character = ids,
    integer = suppressWarnings(as.integer(ids)),
    double = suppressWarnings(as.numeric(ids)),
    integer64 = suppressWarnings(bit64::as.integer64(ids))
  )
  if (anyNA(values)) {
    bad <- ids[is.na(values)]
    rd_abort(
      c(
        # qty() is explicit because `kind` would otherwise be a second
        # candidate quantity and cli refuses to guess.
        "Could not read {cli::qty(length(bad))}selected row id{?s} {.val {bad}} as type {kind}.",
        "i" = "Selected row ids are the {.arg key} column formatted as strings; this means the widget's rows and the backend's schema disagree."
      ),
      class = "selection_error",
      call = call
    )
  }
  values
}

# Attach the `__state` column to a page. Only when a `key` is set: without a
# stable id the client falls back to page indices, and a `__state` column
# lacking `id` would leave getRowId() undefined — worse than the fallback.
attach_row_state <- function(
  backend,
  page,
  offset,
  call = rlang::caller_env()
) {
  if (is.null(backend$key) || is.na(backend$key_kind)) {
    return(page)
  }
  n <- nrow(page)
  if (n == 0) {
    return(page)
  }
  ids <- format_row_ids(page[[backend$key]], backend$key_kind, call = call)
  if (anyNA(ids) || !all(nzchar(ids))) {
    rd_abort(
      c(
        "The {.arg key} column {.val {backend$key}} has missing values on the requested page.",
        "i" = "A row id must identify a row. Choose a key column with no {.val NA}s, or drop {.arg key}."
      ),
      class = "selection_error",
      call = call
    )
  }
  page[[rowstate_column]] <- data.frame(
    id = ids,
    index = offset + seq_len(n) - 1,
    stringsAsFactors = FALSE
  )
  page
}

# --- per-session selection registry ------------------------------------------
#
# A backend is normally built once and shared by every session, but a
# selection belongs to one session. The registry therefore keys by the Shiny
# session token, which `reactableServerData()` can read: the data handler runs
# with the serving session as the default reactive domain (observed; see
# design/selection-contract.md section 3).

selection_session <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    return(NULL)
  }
  shiny::getDefaultReactiveDomain()
}

# Called from the data handler on every request. The client re-sends the whole
# selection with each request, so this is a replace, not a merge.
record_selection <- function(backend, selected_row_ids) {
  session <- selection_session()
  if (is.null(session)) {
    return(invisible(NULL))
  }
  keys <- if (length(selected_row_ids)) {
    as.character(names(selected_row_ids))
  } else {
    character(0)
  }
  token <- session$token
  entry <- backend$state$selection[[token]] %||%
    list(keys = character(0), rv = NULL)
  entry$keys <- keys
  backend$state$selection[[token]] <- entry
  # The accessor may not have been called yet; it seeds itself from $keys.
  if (!is.null(entry$rv)) {
    entry$rv(keys)
  }
  invisible(NULL)
}

selection_entry <- function(backend, session) {
  token <- session$token
  entry <- backend$state$selection[[token]]
  if (is.null(entry)) {
    entry <- list(keys = character(0), rv = NULL)
  }
  if (is.null(entry$rv)) {
    entry$rv <- shiny::reactiveVal(entry$keys)
    backend$state$selection[[token]] <- entry
    session$onSessionEnded(function() {
      backend$state$selection[[token]] <- NULL
    })
  }
  entry
}

check_selection_available <- function(backend, call = rlang::caller_env()) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    rd_abort(
      "Row selection requires the {.pkg shiny} package.",
      class = "selection_error",
      call = call
    )
  }
  session <- selection_session()
  if (is.null(session)) {
    rd_abort(
      c(
        "No Shiny session is active.",
        "i" = "Call this inside a Shiny {.code server} function. Selection is delivered by the widget's data requests, which only exist in a live session."
      ),
      class = "selection_error",
      call = call
    )
  }
  if (is.null(backend$key)) {
    rd_abort(
      c(
        "This backend has no {.arg key}, so its rows have no stable identity.",
        "i" = "Rebuild it with {.code reactable_duckdb_backend(..., key = \"<column>\")} to use selection."
      ),
      class = "selection_error",
      call = call
    )
  }
  session
}

#' Selected row keys
#'
#' The `key` values of the rows currently selected in a widget served by
#' `backend`, as a Shiny reactive. Unlike
#' [reactable::getReactableState()], which can only report selections on the
#' page currently on screen, this spans every page: the client re-sends the
#' complete selection with each data request.
#'
#' `r lifecycle::badge("experimental")`
#'
#' Call this inside a Shiny `server` function. Values are the `key` column
#' formatted as strings; [reactable_duckdb_selected()] maps them back to rows.
#'
#' @param backend A backend created by [reactable_duckdb_backend()], with a
#'   `key` set.
#'
#' @return A Shiny reactive returning a character vector of key values, in the
#'   order the client reports them.
#'
#' @seealso [reactable_duckdb_selected()] for the selected rows themselves.
#' @export
reactable_duckdb_selected_keys <- function(backend) {
  check_backend(backend)
  session <- check_selection_available(backend)
  rv <- selection_entry(backend, session)$rv
  shiny::reactive(rv())
}

#' Selected rows, as a lazy query
#'
#' The selected rows as a **lazy** `tbl` over the backend's source, filtered to
#' the selected keys. Nothing is materialized: the result is a query the
#' application composes with, and collects (or aggregates) on its own terms.
#'
#' `r lifecycle::badge("experimental")`
#'
#' The selection is a set of key values, so the returned query is independent
#' of the widget's current filter and sort state. With no rows selected the
#' query matches nothing.
#'
#' @inheritParams reactable_duckdb_selected_keys
#'
#' @return A Shiny reactive returning a lazy `tbl`.
#'
#' @seealso [reactable_duckdb_selected_keys()] for the raw key values.
#' @export
reactable_duckdb_selected <- function(backend) {
  keys <- reactable_duckdb_selected_keys(backend)
  shiny::reactive(selection_subset(backend, keys()))
}

selection_subset <- function(backend, ids, call = rlang::caller_env()) {
  col <- rlang::sym(backend$key)
  if (length(ids) == 0) {
    return(dplyr::filter(backend$source, !!rlang::call2("==", 1L, 0L)))
  }
  values <- parse_row_ids(ids, backend$key_kind, call = call)
  dplyr::filter(backend$source, !!rlang::call2("%in%", col, values))
}
