# The reactable-facing surface: the widget constructor with its refusal
# rules (plan sections 4a/4b) and the two server S3 methods (plan section 8).

# --- capability-derived column definitions (plan section 4b) ----------------

# Overlay the user's explicitly-set colDef fields onto ours. colDef() drops
# unset arguments entirely (gate-verified), so every field present in `user`
# was set deliberately.
merge_coldef <- function(ours, user) {
  for (field in names(user)) {
    if (!is.null(user[[field]])) {
      ours[[field]] <- user[[field]]
    }
  }
  structure(ours, class = "colDef")
}

# Build the columns list passed to reactable(): capability-derived
# display-only defaults underneath, the user's definitions on top. Errors
# only when the user explicitly re-enables filtering/sorting on a
# display-only column. A user `defaultColDef` cannot override these defaults
# because reactable merges defaultColDef *underneath* per-column definitions
# (upstream reactable.R:355-358, gate-verified).
build_widget_columns <- function(
  backend,
  user_columns,
  call = rlang::caller_env()
) {
  if (!is.null(user_columns)) {
    if (
      !is.list(user_columns) ||
        is.null(names(user_columns)) ||
        any(!nzchar(names(user_columns)))
    ) {
      rd_abort(
        "{.arg columns} must be a fully named list of {.fn reactable::colDef} definitions.",
        class = "unsupported_arg",
        call = call
      )
    }
    if (!all(vapply(user_columns, inherits, logical(1), "colDef"))) {
      rd_abort(
        "Every {.arg columns} entry must be created with {.fn reactable::colDef}.",
        class = "unsupported_arg",
        call = call
      )
    }
    # `.selection` is reactable's own column, not one of ours; it is merged in
    # by build_selection_column() rather than checked against the schema.
    unknown <- setdiff(
      names(user_columns),
      c(backend$columns, selection_col_id)
    )
    if (length(unknown) > 0) {
      rd_abort(
        "{.arg columns} name{?s} {.val {unknown}} {?is/are} not in the source schema.",
        class = "unsupported_arg",
        call = call
      )
    }
  }
  display_only <- backend$columns[
    !vapply(backend$capability, `[[`, TRUE, "filterable")
  ]
  columns <- list()
  for (id in display_only) {
    ours <- reactable::colDef(filterable = FALSE, sortable = FALSE)
    user <- user_columns[[id]]
    if (!is.null(user)) {
      if (isTRUE(user$filterable) || isTRUE(user$sortable)) {
        cap <- backend$capability[[id]]
        rd_abort(
          c(
            "Column {.val {id}} (type {.cls {cap$r_type}}) is display-only: it cannot be made filterable or sortable.",
            "i" = "Nested and opaque types are rendered but never filtered or sorted in v1. Remove {.code filterable = TRUE}/{.code sortable = TRUE} from its {.fn colDef}."
          ),
          class = "unsupported_arg",
          call = call
        )
      }
      ours <- merge_coldef(ours, user)
    }
    columns[[id]] <- ours
  }
  for (id in setdiff(names(user_columns), c(display_only, selection_col_id))) {
    columns[[id]] <- user_columns[[id]]
  }
  if (length(columns) == 0) NULL else columns
}

# reactable's built-in selection column.
selection_col_id <- ".selection"

# The header cell of a `selection = "multiple"` column carries a select-all
# checkbox whose handler iterates only the rows the client currently holds —
# one page. On a served table it therefore selects the visible page while
# rendering itself as fully checked, i.e. it reports a selection that does not
# exist (observed; design/selection-contract.md section 2). It cannot be
# removed from R (reactable overwrites the header for any selectable column),
# so it is neutralized instead: `visibility` hides it and takes it out of the
# tab order, and `pointerEvents` stops the cell's click handler from firing.
# Per-row checkboxes are untouched.
#
# Anything the caller sets in a `.selection` colDef wins, so a caller who
# understands the semantics can put it back.
build_selection_column <- function(selection, user_columns) {
  user <- user_columns[[selection_col_id]]
  if (!identical(selection, "multiple")) {
    return(user)
  }
  ours <- reactable::colDef(
    headerStyle = list(visibility = "hidden", pointerEvents = "none")
  )
  if (is.null(user)) ours else merge_coldef(ours, user)
}

# --- widget constructor ------------------------------------------------------

refuse_arg <- function(condition, message, call, envir = rlang::caller_env()) {
  if (condition) {
    rd_abort(message, class = "unsupported_arg", call = call, .envir = envir)
  }
}

# Selection needs a stable row id, and a row id is the `key` column. Without
# one, reactable falls back to the row's index within the page and selection
# silently follows positions instead of rows.
check_selection_args <- function(backend, dots, call) {
  selection <- dots$selection
  if (is.null(selection)) {
    # Both of these identify rows the same way selection does, so they are
    # equally wrong without a key -- but they are reactable's own arguments
    # and harmless when nothing is selectable.
    return(invisible(NULL))
  }
  if (is.null(backend$key)) {
    rd_abort(
      c(
        "{.arg selection} requires the backend to have a {.arg key}.",
        "i" = "Without one, reactable identifies a row by its position in the delivered page, so a selection follows positions across pages instead of rows.",
        "*" = "Rebuild the backend with {.code reactable_duckdb_backend(..., key = \"<column>\")}."
      ),
      class = "unsupported_arg",
      call = call
    )
  }
  if (is.na(backend$key_kind)) {
    cap <- backend$capability[[backend$key]]
    rd_abort(
      c(
        "{.arg selection} does not support a {.arg key} of type {.cls {cap$r_type}}.",
        "i" = "Row ids cross to the browser as strings, and this type cannot be recovered from one exactly.",
        "*" = "Use a character, integer, double, or integer64 key column."
      ),
      class = "unsupported_arg",
      call = call
    )
  }
  # Both take row *indices* and resolve them through the client's row-id map.
  # Our ids are key values, so an index would silently mean "the row whose key
  # is N" -- a different row, with no error.
  refuse_arg(
    !is.null(dots$defaultSelected),
    "{.arg defaultSelected} is not supported: it selects by row index, but a served table identifies rows by their {.arg key} value, so an index would select the wrong rows.",
    call
  )
  refuse_arg(
    !is.null(dots$selectionId),
    "{.arg selectionId} is not supported: it reports row indices for the current page only. Use {.fn reactable_duckdb_selected_keys} instead.",
    call
  )
  invisible(NULL)
}

#' Render a reactable served by a DuckDB backend
#'
#' Builds a [reactable::reactable()] widget whose filtering, sorting,
#' counting, and pagination are executed by DuckDB through `backend`. The
#' widget embeds only a zero-row schema prototype plus the first page;
#' every later page, filter, or sort request runs as a bounded DuckDB query.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @section Refused arguments:
#' Features the backend does not implement are refused at construction with
#' a `reactableduckdb_unsupported_arg` error rather than silently producing
#' wrong results: `server`, `data`, `searchable = TRUE`, `searchMethod`,
#' `groupBy`, `pagination = FALSE`, `paginateSubRows = TRUE`, and
#' `rownames = TRUE`. Display-only columns (LIST/STRUCT/MAP/BLOB) are
#' automatically rendered with `filterable = FALSE, sortable = FALSE`;
#' explicitly re-enabling either in a [reactable::colDef()] for such a
#' column is refused. With `selection`, `defaultSelected` and `selectionId`
#' are refused as well — both address rows by index (see below).
#'
#' @section Row selection:
#' `selection` requires the backend to have a `key`, which becomes each row's
#' identity. Selections then follow rows across pages, filters, and sorts, and
#' are read with [reactable_duckdb_selected_keys()] or
#' [reactable_duckdb_selected()].
#'
#' Two consequences are worth knowing:
#'
#' * [reactable::getReactableState()] reports only the selected rows on the
#'   page currently on screen — it resolves ids against the rows the client
#'   holds. Use [reactable_duckdb_selected_keys()] for the whole selection.
#' * With `selection = "multiple"` the header select-all checkbox is hidden
#'   and disabled. Its handler only ever covers the delivered page, so on a
#'   served table it would select one page while presenting itself as having
#'   selected everything. Selecting all matching rows is not implementable
#'   against reactable's current client. Set `headerStyle` in a `.selection`
#'   [reactable::colDef()] to override.
#'
#' `defaultSelected` and `selectionId` address rows by index; a served table's
#' ids are `key` values, so an index would silently mean "the row whose key is
#' N". Both are refused rather than reinterpreted.
#'
#' A duplicated `key` gives two rows the same identity, so selecting one
#' selects both. Pass `validate_key = TRUE` to
#' [reactable_duckdb_backend()] to check uniqueness up front.
#'
#' Each selection change costs one data request, and so one page query; the
#' filtered count is served from cache.
#'
#' @section Outside Shiny:
#' The widget renders empty (the prototype); the server backend needs a live
#' session to answer data requests.
#'
#' @param backend A backend created by [reactable_duckdb_backend()].
#' @param ... Further arguments passed to [reactable::reactable()], after
#'   the refusal checks above. `columns` and `defaultColDef` are supported
#'   and merged over the capability-derived defaults.
#' @param defaultPageSize Initial page size. Must not exceed the backend's
#'   `max_page_size`.
#' @param pageSizeOptions Page sizes offered in the UI. Every value must be
#'   positive and no greater than the backend's `max_page_size`, so the UI
#'   can never request a page the backend will refuse.
#'
#' @return An `htmlwidget`, as from [reactable::reactable()].
#'
#' @examplesIf requireNamespace("duckdb", quietly = TRUE) && reactableduckdb:::has_reactable_backend_api()
#' con <- DBI::dbConnect(duckdb::duckdb())
#' DBI::dbWriteTable(con, "flights", data.frame(id = 1:100, dist = rnorm(100)))
#' backend <- reactable_duckdb_backend(dplyr::tbl(con, "flights"), key = "id")
#' reactable_duckdb(backend, filterable = TRUE, defaultPageSize = 25L)
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' @export
reactable_duckdb <- function(
  backend,
  ...,
  defaultPageSize = 50L,
  pageSizeOptions = c(10L, 25L, 50L, 100L)
) {
  warn_reactable_backend_api()
  check_backend(backend)
  call <- environment()
  dots <- list(...)

  refuse_arg(
    "server" %in% names(dots),
    "{.arg server} is set internally by reactableduckdb and cannot be supplied.",
    call
  )
  refuse_arg(
    "data" %in% names(dots),
    "{.arg data} is reserved: the zero-row schema prototype is passed internally.",
    call
  )
  refuse_arg(
    isTRUE(dots$searchable),
    "{.arg searchable} is not supported: global search is not pushed down to DuckDB in v1.",
    call
  )
  refuse_arg(
    !is.null(dots$searchMethod),
    "{.arg searchMethod} is not supported: global search is not pushed down to DuckDB in v1.",
    call
  )
  refuse_arg(
    !is.null(dots$groupBy),
    "{.arg groupBy} is not supported: server-side grouping is not implemented in v1.",
    call
  )
  refuse_arg(
    isFALSE(dots$pagination),
    "{.arg pagination = FALSE} is not supported: the backend only ever returns bounded pages.",
    call
  )
  refuse_arg(
    isTRUE(dots$paginateSubRows),
    "{.arg paginateSubRows} is not supported: it requires server-side grouping.",
    call
  )
  refuse_arg(
    isTRUE(dots$rownames),
    "{.arg rownames} is not supported: the server backend has no row names.",
    call
  )
  dots$rownames <- NULL

  check_selection_args(backend, dots, call)

  defaultPageSize <- check_count(defaultPageSize, "defaultPageSize", min = 1)
  refuse_arg(
    defaultPageSize > backend$max_page_size,
    "{.arg defaultPageSize} ({defaultPageSize}) exceeds the backend's {.arg max_page_size} ({backend$max_page_size}).",
    call
  )
  if (
    !is.numeric(pageSizeOptions) ||
      length(pageSizeOptions) == 0 ||
      anyNA(pageSizeOptions) ||
      !all(vapply(pageSizeOptions, is_whole_number, logical(1)))
  ) {
    rd_abort(
      "{.arg pageSizeOptions} must be a vector of whole numbers.",
      class = "unsupported_arg",
      call = call
    )
  }
  refuse_arg(
    any(pageSizeOptions < 1),
    "Every {.arg pageSizeOptions} value must be positive.",
    call
  )
  refuse_arg(
    any(pageSizeOptions > backend$max_page_size),
    "Every {.arg pageSizeOptions} value must be <= the backend's {.arg max_page_size} ({backend$max_page_size}), so the UI can never request a page the backend will refuse.",
    call
  )

  user_columns <- dots$columns
  dots$columns <- NULL
  columns <- build_widget_columns(backend, user_columns, call = call)
  if (!is.null(dots$selection)) {
    selection_col <- build_selection_column(dots$selection, user_columns)
    if (!is.null(selection_col)) {
      columns <- c(
        columns %||% list(),
        stats::setNames(list(selection_col), selection_col_id)
      )
    }
  }

  args <- c(
    list(
      backend$prototype,
      server = backend,
      defaultPageSize = defaultPageSize,
      pageSizeOptions = pageSizeOptions,
      rownames = FALSE
    ),
    if (!is.null(columns)) list(columns = columns),
    dots
  )
  do.call(reactable::reactable, args)
}

# --- server S3 methods -------------------------------------------------------

refuse_request_features <- function(
  searchValue,
  groupBy,
  call = rlang::caller_env()
) {
  if (!is.null(searchValue) && nzchar(as.character(searchValue)[1])) {
    rd_abort(
      "Global search requests are not supported by this backend (refused at construction; this request should never arrive).",
      class = "unsupported_arg",
      call = call
    )
  }
  if (!is.null(groupBy) && length(groupBy) > 0) {
    rd_abort(
      "Grouping requests are not supported by this backend (refused at construction; this request should never arrive).",
      class = "unsupported_arg",
      call = call
    )
  }
  invisible(NULL)
}

# Short structured request summary for logs: ids only, never filter values
# (they may be user data).
request_summary <- function(pageIndex, pageSize, sortBy, filters) {
  safe_ids <- function(entries) {
    tryCatch(
      vapply(
        entries %||% list(),
        function(e) as.character(e$id)[1],
        character(1)
      ),
      error = function(e) "<unparseable>"
    )
  }
  sprintf(
    "pageIndex=%s pageSize=%s sort=[%s] filter_ids=[%s]",
    paste(format(pageIndex), collapse = ","),
    paste(format(pageSize), collapse = ","),
    paste(safe_ids(sortBy), collapse = ","),
    paste(safe_ids(filters), collapse = ",")
  )
}

#' @exportS3Method reactable::reactableServerInit
reactableServerInit.reactable_duckdb_backend <- function(
  x,
  data = NULL,
  columns = NULL,
  pageIndex = 0,
  pageSize = 0,
  sortBy = NULL,
  filters = NULL,
  searchValue = NULL,
  searchMethod = NULL,
  groupBy = NULL,
  pagination = NULL,
  paginateSubRows = NULL,
  selectedRowIds = NULL,
  expanded = NULL,
  ...
) {
  check_connection(x$con)
  refuse_request_features(searchValue, groupBy)
  # Validate any default sort against schema + capability up front.
  build_order_clauses(sortBy, x$key, x$capability)
  # Warm the count for the initial filter state so the construction-time
  # data call is a cache hit.
  if (x$cache_counts) {
    parsed <- parse_filters_request(filters, x$capability)
    filtered <- source_filtered_query(x, parsed$predicates)
    source_count(x, filtered, rlang::hash(parsed$canonical))
  }
  invisible(NULL)
}

#' @exportS3Method reactable::reactableServerData
reactableServerData.reactable_duckdb_backend <- function(
  x,
  data = NULL,
  columns = NULL,
  pageIndex = 0,
  pageSize = 0,
  sortBy = NULL,
  filters = NULL,
  searchValue = NULL,
  searchMethod = NULL,
  groupBy = NULL,
  pagination = NULL,
  paginateSubRows = NULL,
  selectedRowIds = NULL,
  expanded = NULL,
  ...
) {
  # Any failure is logged (with the backend's schema hash and a values-free
  # request summary) and re-raised unchanged: inside Shiny's data handler a
  # bare error may surface only as a failed HTTP request, so the app's logs
  # must carry the story.
  withCallingHandlers(
    {
      check_connection(x$con)
      refuse_request_features(searchValue, groupBy)
      # The client re-sends the entire selection with every request, so this
      # is the one channel carrying selections made on pages that are no
      # longer on screen. Recorded before the queries run, so a refused
      # request does not also lose the selection.
      record_selection(x, selectedRowIds)
      # Page params are validated before any query executes; source_page()
      # revalidates as defense in depth.
      check_page_params(pageIndex, pageSize, x$max_page_size)
      parsed <- parse_filters_request(filters, x$capability)
      filtered <- source_filtered_query(x, parsed$predicates)
      total <- source_count(x, filtered, rlang::hash(parsed$canonical))
      page <- source_page(x, filtered, sortBy, pageIndex, pageSize)
      reactable::resolvedData(data = page, rowCount = as.numeric(total))
    },
    error = function(cnd) {
      cli::cli_inform(c(
        "!" = "reactableduckdb request failed (schema {substr(x$schema_hash, 1, 8)}): {conditionMessage(cnd)}",
        "i" = request_summary(pageIndex, pageSize, sortBy, filters)
      ))
    }
  )
}
