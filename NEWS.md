# reactableduckdb 0.1.0

Initial release.

* Never materializes more than a zero-row schema prototype, a scalar
  filtered count, and the requested page of rows; DuckDB executes all
  filtering, counting, sorting, and pagination.
* Unsupported behaviour errors loudly (structured
  `reactableduckdb_*` conditions) instead of silently falling back to eager
  execution: unknown or display-only filter/sort columns, oversized page
  requests, and incompatible reactable arguments are refused, never
  clamped or ignored.
* `reactable_duckdb_backend()` wraps a lazy duckplyr data frame or a
  DuckDB-backed dbplyr table: schema prototype, per-column filter/sort
  capability map, bounded LRU count cache, optional unique-`key`
  validation (`validate_key = TRUE`).
* `reactable_duckdb()` builds the widget, refuses incompatible arguments
  at construction, and automatically renders nested/opaque columns
  (LIST/STRUCT/MAP/BLOB) as display-only — user `columns` definitions
  merge over these defaults.
* Per-column filter grammar for text (case-insensitive literal substring,
  injection-safe), numeric, date, datetime (UTC), and logical columns;
  documented in `vignette("reactable-duckdb")`.
* Deterministic ordering: explicit `NULLS LAST` in both directions, with
  `key` appended as a tie-breaker.
* Requests that fail inside Shiny surface as HTTP 500 for the client;
  the server log carries a structured line with the backend's schema hash
  and offending column ids (never filter values).
* Requires the reactable development version (>= 0.4.5.9000), installable
  from GitHub (`glin/reactable`) or from a Posit Package Manager
  repository that mirrors it. If the backend API is not
  detected, the package warns once per session naming both routes rather
  than erroring, since the development version is a moving target.
