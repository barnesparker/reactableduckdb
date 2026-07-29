# Adapter layer: normalizes the two supported source kinds into one dbplyr
# tbl_dbi over a DuckDB connection, and provides the only three
# materializations the package ever performs (prototype, scalar count,
# bounded page).

# duckplyr-specific logic is isolated here, especially the experimental
# duckplyr::as_tbl() call. Note (gate D3): as_tbl() registers a randomly
# named temporary VIEW, so the rendered base query is `SELECT * FROM
# as_tbl_duckplyr_<random>` — the app's own pipeline stays inside the view
# and the name differs on every call.
as_reactable_source <- function(data, call = rlang::caller_env()) {
  if (inherits(data, "duckplyr_df")) {
    tbl <- duckplyr::as_tbl(data)
  } else if (inherits(data, "tbl_sql") || inherits(data, "tbl_dbi")) {
    tbl <- data
  } else {
    rd_abort(
      c(
        "{.arg data} must be a lazy duckplyr data frame or a dbplyr {.cls tbl_sql}/{.cls tbl_dbi}, not {.obj_type_friendly {data}}.",
        "i" = "Eager data frames are refused: reactableduckdb never collects a full source into R. Register the data with DuckDB first."
      ),
      class = "source_error",
      call = call
    )
  }
  con <- dbplyr::remote_con(tbl)
  if (!inherits(con, "duckdb_connection")) {
    rd_abort(
      c(
        "The source's connection is {.cls {class(con)[1]}}; only DuckDB connections are supported.",
        "i" = "reactableduckdb v1 generates DuckDB SQL (NULLS LAST, strpos, LIMIT/OFFSET) and refuses other backends rather than producing wrong queries."
      ),
      class = "source_error",
      call = call
    )
  }
  tbl
}

source_connection <- function(tbl) {
  dbplyr::remote_con(tbl)
}

source_query <- function(tbl) {
  render_sql(tbl)
}

# Materialization 1 of 3: the zero-row schema prototype.
source_prototype <- function(tbl, call = rlang::caller_env()) {
  prototype <- as.data.frame(
    dplyr::collect(utils::head(tbl, 0)),
    stringsAsFactors = FALSE
  )
  if (ncol(prototype) < 1) {
    rd_abort(
      "The source has no columns; reactable requires at least one.",
      class = "schema_error",
      call = call
    )
  }
  prototype
}

source_filtered_query <- function(backend, predicates) {
  apply_predicates(backend$source, predicates)
}

# Materialization 2 of 3: the scalar filtered count. The already-built
# `filtered` query is passed in — never reconstructed from the filters — so
# the count and the page cannot diverge. `cache_key` is only a cache key.
source_count <- function(backend, filtered, cache_key,
                         call = rlang::caller_env()) {
  compute <- function() {
    sql <- duckdb_count_sql(render_sql(filtered))
    result <- run_query(backend, sql, call = call)
    # COUNT(*) arrives as plain double from DuckDB (gate-verified); the
    # coercion is defensive only. Counts <= 2^53 are exact.
    as.numeric(result$n[[1]])
  }
  if (!backend$cache_counts) {
    return(compute())
  }
  cache_fetch(backend$state, cache_key, compute)
}

# Materialization 3 of 3: the requested page, via the isolated wrapper.
source_page <- function(backend, filtered, sort_by, page_index, page_size,
                        call = rlang::caller_env()) {
  params <- check_page_params(
    page_index,
    page_size,
    backend$max_page_size,
    call = call
  )
  clauses <- build_order_clauses(
    sort_by,
    backend$key,
    backend$capability,
    call = call
  )
  sql <- duckdb_page_sql(
    render_sql(filtered),
    clauses,
    params$limit,
    params$offset,
    call = call
  )
  run_query(backend, sql, call = call)
}
