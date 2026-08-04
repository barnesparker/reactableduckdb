# Filter grammar and column capability map (plan section 9; the user-facing
# grammar is documented in design/filter-contract.md and the vignette).
#
# Values are parsed into typed R values in R, then bound into the lazy query
# through dbplyr expressions, which escape them. No user-entered text is ever
# pasted into SQL.

filter_types <- c("text", "numeric", "date", "datetime", "logical")

infer_capability_type <- function(col) {
  if (is.character(col) || is.factor(col)) {
    "text"
  } else if (inherits(col, "Date")) {
    "date"
  } else if (inherits(col, "POSIXt")) {
    "datetime"
  } else if (is.logical(col)) {
    "logical"
  } else if (inherits(col, "integer64") || is.numeric(col)) {
    "numeric"
  } else {
    # list (LIST/MAP/JSON), data.frame (STRUCT), blob/raw, and anything
    # unrecognized: rendered but display-only.
    "none"
  }
}

# Capability map built once at construction from the prototype's R types,
# with per-column overrides from `filter_spec`. Each entry:
# list(type, filterable, sortable, exact, r_type).
build_capability_map <- function(
  prototype,
  filter_spec = NULL,
  call = rlang::caller_env()
) {
  validate_filter_spec(filter_spec, prototype, call = call)
  capability <- lapply(names(prototype), function(id) {
    col <- prototype[[id]]
    type <- infer_capability_type(col)
    spec <- filter_spec[[id]]
    if (!is.null(spec$type)) {
      type <- spec$type
    }
    list(
      type = type,
      filterable = type != "none",
      sortable = type != "none",
      exact = isTRUE(spec$exact),
      r_type = paste(class(col), collapse = "/")
    )
  })
  stats::setNames(capability, names(prototype))
}

validate_filter_spec <- function(
  filter_spec,
  prototype,
  call = rlang::caller_env()
) {
  if (is.null(filter_spec)) {
    return(invisible(NULL))
  }
  if (
    !is.list(filter_spec) ||
      is.null(names(filter_spec)) ||
      any(!nzchar(names(filter_spec)))
  ) {
    rd_abort(
      "{.arg filter_spec} must be a fully named list of per-column specs.",
      class = "spec_error",
      call = call
    )
  }
  unknown <- setdiff(names(filter_spec), names(prototype))
  if (length(unknown) > 0) {
    rd_abort(
      "{.arg filter_spec} names {.val {unknown}} are not columns of the source.",
      class = "spec_error",
      call = call
    )
  }
  for (id in names(filter_spec)) {
    spec <- filter_spec[[id]]
    if (!is.list(spec)) {
      rd_abort(
        "{.arg filter_spec} entry {.val {id}} must be a list.",
        class = "spec_error",
        call = call
      )
    }
    bad_keys <- setdiff(names(spec), c("exact", "type"))
    if (length(bad_keys) > 0) {
      rd_abort(
        c(
          "{.arg filter_spec} entry {.val {id}} has unsupported field{?s} {.val {bad_keys}}.",
          "i" = "Supported fields: {.val exact} (logical) and {.val type} ({.val {filter_types}})."
        ),
        class = "spec_error",
        call = call
      )
    }
    inferred <- infer_capability_type(prototype[[id]])
    if (!is.null(spec$type)) {
      if (
        !is.character(spec$type) ||
          length(spec$type) != 1L ||
          !spec$type %in% filter_types
      ) {
        rd_abort(
          "{.arg filter_spec} entry {.val {id}}: {.field type} must be one of {.val {filter_types}}.",
          class = "spec_error",
          call = call
        )
      }
      if (inferred == "none") {
        rd_abort(
          c(
            "{.arg filter_spec} cannot pin a filter type on display-only column {.val {id}} (type {.cls {paste(class(prototype[[id]]), collapse = '/')}}).",
            "i" = "Nested and opaque columns are not filterable in v1."
          ),
          class = "spec_error",
          call = call
        )
      }
    }
    if (!is.null(spec$exact)) {
      if (
        !is.logical(spec$exact) || length(spec$exact) != 1L || is.na(spec$exact)
      ) {
        rd_abort(
          "{.arg filter_spec} entry {.val {id}}: {.field exact} must be `TRUE` or `FALSE`.",
          class = "spec_error",
          call = call
        )
      }
      effective <- spec$type %||% inferred
      if (effective != "text") {
        rd_abort(
          "{.arg filter_spec} entry {.val {id}}: {.field exact} only applies to text columns, and {.val {id}} filters as {.val {effective}}.",
          class = "spec_error",
          call = call
        )
      }
    }
  }
  invisible(NULL)
}

# --- value parsing -----------------------------------------------------------

number_pattern <- "[+-]?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?"
date_pattern <- "[0-9]{4}-[0-9]{2}-[0-9]{2}"
datetime_pattern <- paste0(
  date_pattern,
  "(?:[ T][0-9]{2}:[0-9]{2}(?::[0-9]{2})?)?"
)

filter_value_abort <- function(
  id,
  value,
  type,
  hint = NULL,
  call = rlang::caller_env()
) {
  msg <- "Cannot parse filter value {.val {value}} for column {.val {id}} as a {type} filter."
  if (!is.null(hint)) {
    msg <- c(msg, "i" = hint)
  }
  rd_abort(msg, class = "filter_error", call = call)
}

# Split a comparator/range/plain expression against a value pattern.
# Returns list(op, parts) with raw string parts, or NULL when the input does
# not match the grammar at all.
match_comparison <- function(value, pattern) {
  trimmed <- trimws(value)
  comparator_re <- paste0("^(>=|<=|>|<)\\s*(", pattern, ")$")
  m <- regmatches(trimmed, regexec(comparator_re, trimmed))[[1]]
  if (length(m) > 0) {
    return(list(op = m[[2]], parts = m[[3]]))
  }
  if (grepl("..", trimmed, fixed = TRUE)) {
    parts <- trimws(strsplit(trimmed, "..", fixed = TRUE)[[1]])
    part_re <- paste0("^", pattern, "$")
    if (length(parts) == 2 && all(grepl(part_re, parts))) {
      return(list(op = "between", parts = parts))
    }
    return(NULL)
  }
  if (grepl(paste0("^", pattern, "$"), trimmed)) {
    return(list(op = "==", parts = trimmed))
  }
  NULL
}

parse_numeric_parts <- function(id, value, matched, call) {
  numbers <- as.numeric(matched$parts)
  if (anyNA(numbers)) {
    filter_value_abort(id, value, "numeric", call = call)
  }
  numbers
}

parse_date_parts <- function(id, value, matched, call) {
  dates <- as.Date(matched$parts, format = "%Y-%m-%d")
  if (anyNA(dates)) {
    filter_value_abort(
      id,
      value,
      "date",
      hint = "Dates must be real ISO dates like {.val 2026-01-31}.",
      call = call
    )
  }
  dates
}

parse_datetime_parts <- function(id, value, matched, call) {
  parts <- gsub("T", " ", matched$parts, fixed = TRUE)
  parsed <- as.POSIXct(
    parts,
    tz = "UTC",
    tryFormats = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d"),
    optional = TRUE
  )
  if (anyNA(parsed)) {
    filter_value_abort(
      id,
      value,
      "datetime",
      hint = "Datetimes are ISO, interpreted as UTC: {.val 2026-01-31} or {.val 2026-01-31 12:00:00}.",
      call = call
    )
  }
  parsed
}

# Parse one filter value for one column. Returns NULL for "no filter", or
# list(op, values, type) with typed R values. Malformed input raises a
# structured reactableduckdb_filter_error, never a raw DB error.
parse_filter_value <- function(id, value, cap, call = rlang::caller_env()) {
  if (is.null(value)) {
    return(NULL)
  }
  if (length(value) != 1L || is.na(value)) {
    filter_value_abort(id, value, cap$type, call = call)
  }
  value <- as.character(value)
  type <- cap$type

  if (type == "text") {
    # Text is taken literally and untrimmed: every character, including
    # whitespace, %, _, quotes, and things that look like other grammars
    # (e.g. "10..20"), is part of the search string. Empty means no filter.
    if (!nzchar(value)) {
      return(NULL)
    }
    op <- if (isTRUE(cap$exact)) "exact" else "contains"
    return(list(op = op, values = value, type = "text"))
  }

  if (!nzchar(trimws(value))) {
    return(NULL)
  }

  if (type == "numeric") {
    matched <- match_comparison(value, number_pattern)
    if (is.null(matched)) {
      filter_value_abort(
        id,
        value,
        "numeric",
        hint = "Supported: {.val 10}, {.val -5}, {.val 1.5}, {.val 1e6}, {.val >=10}, {.val <-5}, {.val 10..20}.",
        call = call
      )
    }
    values <- parse_numeric_parts(id, value, matched, call)
  } else if (type == "date") {
    matched <- match_comparison(value, date_pattern)
    if (is.null(matched)) {
      filter_value_abort(
        id,
        value,
        "date",
        hint = "Supported: {.val 2026-01-01}, {.val >=2026-01-01}, {.val 2026-01-01..2026-01-31}.",
        call = call
      )
    }
    values <- parse_date_parts(id, value, matched, call)
  } else if (type == "datetime") {
    matched <- match_comparison(value, datetime_pattern)
    if (is.null(matched)) {
      filter_value_abort(
        id,
        value,
        "datetime",
        hint = "Supported: ISO datetimes (UTC) with the numeric comparators and {.val ..} ranges.",
        call = call
      )
    }
    values <- parse_datetime_parts(id, value, matched, call)
  } else if (type == "logical") {
    lowered <- tolower(trimws(value))
    if (lowered %in% c("true", "t", "1")) {
      values <- TRUE
    } else if (lowered %in% c("false", "f", "0")) {
      values <- FALSE
    } else {
      filter_value_abort(
        id,
        value,
        "logical",
        hint = "Supported (case-insensitive): {.val true}, {.val false}, {.val t}, {.val f}, {.val 1}, {.val 0}.",
        call = call
      )
    }
    return(list(op = "==", values = values, type = "logical"))
  } else {
    # Unreachable when ids are capability-checked first; kept as a guard.
    filter_value_abort(id, value, type, call = call)
  }

  if (matched$op == "between") {
    if (values[[1]] > values[[2]]) {
      filter_value_abort(
        id,
        value,
        type,
        hint = "Range lower bound is greater than the upper bound.",
        call = call
      )
    }
  }
  list(op = matched$op, values = values, type = type)
}

# --- predicate construction --------------------------------------------------

# Built with rlang::call2() so the lazy query carries exactly the intended
# AST; dbplyr binds the typed values as escaped literals.
build_filter_predicate <- function(id, parsed) {
  col <- rlang::sym(id)
  op <- parsed$op
  values <- parsed$values
  if (op == "contains") {
    # Case-insensitive literal substring: strpos(LOWER(col), LOWER(value)) > 0.
    # The sole text translation (gate-verified); % and _ stay literal because
    # this is not LIKE.
    return(rlang::call2(
      ">",
      rlang::call2(
        "strpos",
        rlang::call2("tolower", col),
        rlang::call2("tolower", values)
      ),
      0L
    ))
  }
  if (op == "exact") {
    return(rlang::call2("==", col, values))
  }
  if (op == "between") {
    return(rlang::call2(
      rlang::call2("::", rlang::sym("dplyr"), rlang::sym("between")),
      col,
      values[[1]],
      values[[2]]
    ))
  }
  rlang::call2(op, col, values)
}

# Parse a reactable filters request: an unnamed list of list(id =, value =)
# entries (the shape reactable sends both at construction and from parsed
# JSON). Returns predicates plus the canonical form used as the count cache
# key.
parse_filters_request <- function(
  filters,
  capability,
  call = rlang::caller_env()
) {
  filters <- filters %||% list()
  if (!is.list(filters)) {
    rd_abort(
      "{.arg filters} must be a list of {.code list(id =, value =)} entries.",
      class = "filter_error",
      call = call
    )
  }
  predicates <- list()
  canonical <- list()
  for (entry in filters) {
    if (!is.list(entry) || is.null(entry$id)) {
      rd_abort(
        "{.arg filters} must be a list of {.code list(id =, value =)} entries.",
        class = "filter_error",
        call = call
      )
    }
    cap <- check_column_id(
      entry$id,
      capability,
      purpose = "filter",
      call = call
    )
    parsed <- parse_filter_value(entry$id, entry$value, cap, call = call)
    if (is.null(parsed)) {
      next
    }
    predicates <- c(predicates, list(build_filter_predicate(entry$id, parsed)))
    canonical <- c(
      canonical,
      list(list(
        id = entry$id,
        type = parsed$type,
        op = parsed$op,
        values = as.character(parsed$values)
      ))
    )
  }
  if (length(canonical) > 0) {
    canonical <- canonical[order(vapply(canonical, `[[`, "", "id"))]
  }
  list(predicates = predicates, canonical = canonical)
}
