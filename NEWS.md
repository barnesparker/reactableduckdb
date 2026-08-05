# reactableduckdb (development version)

* A datetime filter with an out-of-range time (`2026-01-01 25:00:00`,
  `2026-01-01 12:99:00`) was silently read as midnight on that date and
  filtered against an instant the user never typed. Hours, minutes, and
  seconds are now range-checked and such a value is refused. The value shape
  passed the old pattern, and `as.POSIXct()`'s format fallback ends at
  `"%Y-%m-%d"`, which `strptime()` matches as a prefix.

* `filter_spec`'s `type` is now checked against the column it pins. A column
  may be pinned to its own grammar, and a `date`/`datetime` column to either
  of those two — `DATE` and `TIMESTAMP` compare freely in DuckDB, so a
  timestamp column can be filtered at date granularity. Every other
  combination built a predicate the column's SQL type could not evaluate and
  surfaced as a raw DuckDB binder error on the first request, except a
  `numeric` column pinned to `logical`, which bound successfully and returned
  a surprising row set. Incompatible pins are now refused at construction with
  a `reactableduckdb_spec_error`.

* Row selection previously identified a row by its position within the
  delivered page, so a selection made on one page also marked the row in that
  same position on every other page — rows the user never chose, with no error.
  Pages of a keyed backend now carry reactable's `__state` row-identity column,
  so selections follow rows across pages, filters, and sorts. The old behaviour
  was documented as "applies to the current page only", which was never
  accurate.

* A source column named `__state` is now refused, as it would collide with
  reactable's per-row state column.

* `reactable_duckdb()` now requires the backend to have a `key` when
  `selection` is set, and that key must be `character`, `integer`, `integer64`,
  or a whole-number `double` (DuckDB `BIGINT` arrives as one). Other types stay
  valid sort tie-breakers but cannot survive the round-trip through the browser
  as a string row id.

* `reactable_duckdb()` hides and disables the header select-all checkbox for
  `selection = "multiple"`. Its handler only ever covers the delivered page, so
  on a served table it selected one page while rendering itself as having
  selected everything. Override with `headerStyle` in a `.selection` `colDef`.

* `reactable_duckdb()` refuses `defaultSelected` and `selectionId` alongside
  `selection`: both address rows by index, which on a served table would
  silently mean "the row whose key is N".

* `reactable_duckdb_selected()` is new. It returns the selected rows as a lazy
  query, so a selection materializes nothing until the application collects it.

* `reactable_duckdb_selected_keys()` is new. It reports the **complete**
  selection, including rows on pages that are not on screen — which
  `reactable::getReactableState()` structurally cannot do.

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
