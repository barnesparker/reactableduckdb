# Observed upstream contract — Phase 0 feasibility gate

Everything below was observed empirically against the **installed** packages on
2026-07-28. Formals, error messages, SQL, and request shapes are reproduced
**verbatim** from console output. Nothing here is filled in from prior knowledge.
Items that could not be verified are labelled **UNVERIFIED**.

Gate scripts lived in a session temp directory and are deliberately not committed.

> **Note on citations.** This document was written against a requirements spec and an
> implementation plan that are no longer kept in the repository. References below of
> the form "plan §N" or `design/spec.md` are preserved as written, so the record stays
> verbatim; they point at documents you will not find here. The behaviour they describe
> is what shipped — see the source, tests, and `vignette("reactable-duckdb")`.

## 0. Environment the gate ran in

```
R.version.string : R version 4.4.2 (2024-10-31)
platform         : aarch64-apple-darwin24.1.0
sysname          : Darwin
release          : 25.5.0
machine          : arm64
.Platform$pkgType: source
.Platform$OS.type: unix
```

| Package | Version |
| --- | --- |
| reactable | 0.4.5.9000 |
| duckplyr | 1.2.1 |
| duckdb | 1.5.0 |
| dbplyr | 2.5.0 |
| dplyr | 1.2.0 |
| DBI | 1.2.3 |
| bit64 | 4.6.0.1 |
| rlang | 1.1.7 |

### How reactable was obtained

reactable-dev was installed from GitHub — the canonical source for the development
version — at the pinned SHA. Installed provenance:

```
Version          0.4.5.9000
RemoteType       github
RemoteUsername   glin
RemoteRepo       reactable
RemoteRef        4a438397eaa83ec71a4dc3e895bbdcc595232d78
RemoteSha        4a438397eaa83ec71a4dc3e895bbdcc595232d78
Built            R 4.4.2; ; 2026-07-28 18:41:22 UTC; unix
```

Tarball `sha256 = f63e78c833ed48d546202b3a2823de98f1a975f397c4480d526b1f7f1b83246d`.

The hard requirement is the reactable *development package*, of which GitHub is the
canonical source; a Posit Package Manager repository mirroring it is a convenience
channel, not a requirement. This gate therefore exercised the real requirement.

- **UNVERIFIED:** anything about a Linux target platform. This gate ran on macOS
  arm64 only.
- The package declares no GitHub `Remotes`, so neither channel is forced on
  consumers.

## 1. CRAN reactable lacks the API (verified)

CRAN `reactable` 0.4.5 `NAMESPACE` (18 lines) contains no match for any of the three
functions:

```
CRAN reactable version : 0.4.5
  export reactableServerInit    : ABSENT from CRAN NAMESPACE
  export reactableServerData    : ABSENT from CRAN NAMESPACE
  export resolvedData           : ABSENT from CRAN NAMESPACE
```

Dev build:

```
reactableServerInit    exported=TRUE  is.function=TRUE
reactableServerData    exported=TRUE  is.function=TRUE
resolvedData           exported=TRUE  is.function=TRUE
```

## 2. Exact formals (verbatim)

```r
reactableServerInit(x, data = NULL, columns = NULL, pageIndex = 0, pageSize = 0,
    sortBy = NULL, filters = NULL, searchValue = NULL, searchMethod = NULL,
    groupBy = NULL, pagination = NULL, paginateSubRows = NULL,
    selectedRowIds = NULL, expanded = NULL, ...)

reactableServerData(x, data = NULL, columns = NULL, pageIndex = 0, pageSize = 0,
    sortBy = NULL, filters = NULL, searchValue = NULL, searchMethod = NULL,
    groupBy = NULL, pagination = NULL, paginateSubRows = NULL,
    selectedRowIds = NULL, expanded = NULL, ...)

resolvedData(data, rowCount = NULL, maxRowCount = NULL)
```

Both generics have identical signatures. **Matches plan §1.1 exactly.**

## 3. `resolvedData()` — source and guards (verbatim)

```r
function(data, rowCount = NULL, maxRowCount = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame")
  }
  if (!is.numeric(rowCount)) {
    stop("`rowCount` must be provided and numeric")
  }
  if (!is.null(maxRowCount) && !is.numeric(maxRowCount)) {
    stop("`maxRowCount` must be numeric")
  }
  structure(
    list(data = data, rowCount = rowCount, maxRowCount = maxRowCount),
    class = "reactable_resolvedData"
  )
}
```

Observed error messages:

```
resolvedData(data.frame(a=1), rowCount = 'x') -> simpleError: `rowCount` must be provided and numeric
resolvedData(data.frame(a=1), rowCount = NULL) -> simpleError: `rowCount` must be provided and numeric
resolvedData(data.frame(a=1))                 -> simpleError: `rowCount` must be provided and numeric
resolvedData('notadf', rowCount = 1)          -> simpleError: `data` must be a data frame
resolvedData(df, rowCount = 5L)               -> <no error>
resolvedData(df, rowCount = 5, maxRowCount=9) -> <no error>
```

Return shape:

```
class : reactable_resolvedData
names : data, rowCount, maxRowCount
```

`rowCount` **must be numeric** — confirmed as plan §1.1 states.

### integer64 is a non-issue — plan's stated rationale is wrong

Plan review-round-2 asserts "integer64 fails `is.numeric`, which `resolvedData()`
requires". Observed with bit64 4.6.0.1:

```
is.numeric(integer64)            : TRUE
resolvedData(rowCount=integer64) : <no error>
toJSON(resolvedData)             : {"data":{"a":[1]},"rowCount":1234567,"maxRowCount":null}
```

`typeof(integer64)` is `"double"`, so `is.numeric()` returns `TRUE` and serialization is
correct. Separately, `COUNT(*)` fetched through DBI on DuckDB already returns a plain
double (§8), so integer64 never arises on the count path here. The defensive
`as.numeric()` coercion is harmless and may stay, but **the justification in the plan
should be corrected** — it is not guarding against a real failure observed here.

## 4. `rowCount` semantics — Rd contradicts the reference implementation

`man/resolvedData.Rd:12`, verbatim:

```
\item{rowCount}{The row count of the current page.}
```

`R/server-df.R:163-182`, verbatim:

```r
dfPaginate <- function(df, pageIndex = 0, pageSize = NULL) {
  if (is.null(pageSize)) {
    return(resolvedData(df, rowCount = nrow(df)))
  }

  # Ensure page index is within boundaries
  rowCount <- nrow(df)
  maxPageIndex <- max(ceiling(rowCount / pageSize) - 1, 0)
  if (pageIndex < 0) {
    pageIndex <- 0
  } else if (pageIndex > maxPageIndex) {
    pageIndex <- maxPageIndex
  }

  rowStart <- min(pageIndex * pageSize + 1, nrow(df))
  rowEnd <- min(pageIndex * pageSize + pageSize, nrow(df))
  page <- df[rowStart:rowEnd, ]

  resolvedData(page, rowCount = rowCount)
}
```

`df` arrives already filtered; `rowCount <- nrow(df)` is taken **before** slicing, and
the slice is returned as `page`. So `rowCount` = **total filtered rows before
pagination**, and the Rd sentence is wrong. Plan §1.1's reading is confirmed.

Note also that the reference implementation **silently clamps** an out-of-range
`pageIndex` to `maxPageIndex`. Plan §11 requires us to raise instead of clamp — a
deliberate divergence from upstream behaviour, not an oversight.

## 5. Construction lifecycle (verbatim, `R/reactable.R:683-706`)

```r
    initialProps <- list(
      data = data,
      columns = cols,
      pagination = pagination,
      paginateSubRows = paginateSubRows,
      pageIndex = 0,
      pageSize = defaultPageSize,
      sortBy = defaultSorted,
      groupBy = groupBy,
      searchMethod = searchMethod
      # TODO add expanded, selectedRowIds
    )

    do.call(reactableServerInit, c(list(backend), initialProps))

    # Pre-calculate initial page. This could be undesired in some cases, so it
    # may be optional in the future.
    initialPage <- do.call(reactableServerData, c(list(backend), initialProps))
    if (!is.resolvedData(initialPage)) {
      stop("reactable server backends must return a `resolvedData()` object from `reactableServerData()`")
    }
    data <- initialPage$data
    serverRowCount <- initialPage$rowCount
    serverMaxRowCount <- initialPage$maxRowCount
```

Observed live with a stub S3 backend (`defaultPageSize = 7`, `rowCount = 4321`):

```
reactableServerInit called n times : 1
reactableServerData called n times : 1
```

Arguments actually received by **both** methods (9 named items; `filters`,
`searchValue`, `selectedRowIds`, `expanded` are **not** passed at construction and fall
back to their formal defaults):

```
 $ data           :'data.frame': 0 obs. of  2 variables
 $ columns        :List of 2   # each: list(id=, name=, type=)
 $ pagination     : logi TRUE
 $ paginateSubRows: logi FALSE
 $ pageIndex      : num 0
 $ pageSize       : num 7
 $ sortBy         : NULL
 $ groupBy        : NULL
 $ searchMethod   : NULL
```

`columns` entries observed as e.g. `list(id = "id", name = "id", type = "numeric")`.

**The first real page is produced at construction time, outside Shiny.** The S3 methods
are therefore directly testable without a Shiny session — confirmed:

```
attribs$data value :
{"id":[1,2,3,4,5,6,7],"txt":["row1","row2","row3","row4","row5","row6","row7"]}
attribs$serverRowCount: 4321
attribs$dataKey       : fde8f343c8827ff724137a8c645661f0
attribs$static        : FALSE
```

### `preRenderHook` resets data outside Shiny (confirmed)

```
--- after preRenderHook (what renders outside Shiny) ---
data after hook  : {"id":[],"txt":[]}
serverRowCount after hook: 4321
```

Confirms plan §1.1/§17: a server-backed widget renders **empty** outside Shiny, so
README/vignette need a pre-captured screenshot.

### Serialized props with `maxRowCount = NULL`

```
prop names: data, columns, defaultPageSize, dataKey, static, serverRowCount
serverRowCount       4321
serverMaxRowCount    NULL      # omitted from props entirely when NULL
defaultPageSize      7
```

`serverMaxRowCount` is simply absent when `maxRowCount = NULL`; `serverRowCount` alone
carries the total. Expected page count `ceiling(4321/7) = 618`.
**UNVERIFIED:** the browser's actual rendered pager. Only the serialized props were
checked (headless). Visual confirmation is deferred to the example-app UAT (plan §18).

## 6. Shiny request path (verbatim)

```r
reactableFilterFunc <- function(data, req) {
  body <- rawToChar(req$rook.input$read())
  params <- parseParams(body)

  start <- Sys.time()
  resolvedData <- do.call(reactableServerData, c(list(data$backend), mergeLists(data, params)))
  end <- Sys.time()

  if (!is.resolvedData(resolvedData)) {
    stop("reactable server backends must return a `resolvedData()` object from `reactableServerData()`")
  }

  debugLog(sprintf("(reactableFilterFunc) time to resolve data: %s\n%s", format(end - start, units = "secs"), toJSON(resolvedData)))

  shiny::httpResponse(
    status = 200L,
    content_type = "application/json",
    content = toJSON(resolvedData)
  )
}

parseParams <- function(json) {
  params <- jsonlite::parse_json(json, simplifyDataFrame = FALSE)
  params
}

getServerBackend <- function(backend = NULL) {
  if (!is.null(backend) && !is.character(backend)) {
    return(backend)
  }
  ...
}
```

`getServerBackend()` returns any non-`NULL`, non-character value unchanged — confirming
an arbitrary S3 object is the documented hook. `mergeLists(a, b)` (verbatim in gate log)
overlays parsed request params onto the stored `initialProps`, skipping `NULL` values in
`b`, so request fields override construction defaults and absent fields retain them.

`simplifyDataFrame = FALSE` means post-parse request shapes are plain nested lists.

- ~~UNVERIFIED: the concrete JSON body the *client* sends.~~ **CLOSED 2026-07-28** — see
  §12: a real browser session was driven with chromote and the genuine client bodies
  were captured. `filters = [{id, value}]` and the overall request shape are confirmed.

`reactable::toJSON()` options, verbatim: `dataframe = "columns"`, `rownames = FALSE`,
`POSIXt = "ISO8601"`, `Date = "ISO8601"`, `UTC = TRUE`, `force = TRUE`,
`auto_unbox = TRUE`, `null = "null"`, `json_verbatim = TRUE`.

## 7. duckplyr → dbplyr handoff

Source: 20,000-row Parquet written by DuckDB `COPY`, columns covering `BIGINT`,
`VARCHAR` (with NULLs), `INTEGER`, `DOUBLE`, `DECIMAL(18,4)`, `DATE`, `TIMESTAMP`,
`TIMESTAMPTZ`, `BOOLEAN`, `BIGINT[]` (LIST), and a `STRUCT`.

```
read_parquet_duckdb(pq, prudence = "stingy")
class: prudent_duckplyr_df, duckplyr_df, tbl_df, tbl, data.frame
```

`as_tbl()` handoff:

```
class          : tbl_duckdb_connection, tbl_dbi, tbl_sql, tbl_lazy, tbl
inherits tbl_dbi: TRUE
inherits tbl_sql: TRUE
remote_con class : duckdb_connection
dbIsValid(con)   : TRUE
```

`remote_con()`, `remote_query()`, `sql_render()` all work on the result. **Plan §1.2's
core claim holds.**

### The three permitted materializations — all proven

Aggregated source (`summarise(.by=)`, 14 groups):

```
(1) prototype rows/cols: 0 / 6
    names : grp, big, n, total, avg_price, first_dt
    types : character, logical, integer, numeric, numeric, Date
(2) scalar count : 14 | length=1 | class=numeric | is.numeric=TRUE
(3) bounded page rows : 5      # LIMIT 5 OFFSET 5 via DBI::dbGetQuery
```

Flat source (18,800 rows after filter):

```
prototype rows : 0 cols: 11
scalar count   : 18800 (class numeric)
page rows      : 3             # LIMIT 3 OFFSET 100
```

Prototype column classes for the flat source:

```
       id           name            grp            qty         amount 
  "numeric"    "character"    "character"      "integer"      "numeric" 
    price             dt             ts           tstz           flag 
  "numeric"         "Date" "POSIXct/POSIXt" "POSIXct/POSIXt"    "logical" 
```

`DECIMAL(18,4)` → `numeric`. `TIMESTAMP` and `TIMESTAMPTZ` both → `POSIXct/POSIXt`.

## 8. `COUNT(*)` typing (verified)

Straight from DBI on DuckDB:

```
class : numeric | typeof: double | is.numeric: TRUE | value: 18800
resolvedData accepts it directly? : <NO ERROR>
```

No integer64 involved. See also §3.

## 9. LIST / STRUCT columns — prototype serialization (verified)

Prototype classes: `tags` → `"list"`, `meta` (STRUCT) → `"data.frame"` (a nested
data frame column).

```
reactable::toJSON(zero-row prototype):
{"id":[],"name":[],"grp":[],"qty":[],"amount":[],"price":[],"dt":[],"ts":[],
 "tstz":[],"flag":[],"tags":[],"meta":{"a":[],"b":[]}}

reactable() accepts this prototype? : <NO ERROR>
```

One real row:

```
{"id":[6],"name":["name_6"],"grp":["grp_6"],"qty":[6],"amount":[9],"price":[6],
 "dt":["2026-01-07"],"ts":["2026-01-01T06:00:00Z"],"tstz":["2026-01-01T06:00:00Z"],
 "flag":[true],"tags":[[6,7]],"meta":{"a":[6],"b":["x6"]}}
```

LIST serializes as a nested array; STRUCT as an object of column arrays. Both survive
`reactable()` construction. Plan §9's "display-only" classification for these types is
workable — nothing crashes, they simply must not be filterable or sortable.

## 10. Case-insensitive substring translation

`grepl(..., fixed = TRUE)` is **rejected by the DuckDB dbplyr backend**:

```
ERR: Parameters `perl`, `fixed` and `useBytes` in grepl are not currently supported in DuckDB backend
```

`strpos()` translates and executes correctly:

```sql
SELECT as_tbl_duckplyr_NACMXDlyGO.*
FROM as_tbl_duckplyr_NACMXDlyGO
WHERE (strpos(LOWER("name"), LOWER('NAME_1')) > 0.0)
```

```
matching rows for 'NAME_1' (case-insensitive substring): 2090
```

**Plan §9 should be corrected:** `strpos(LOWER(col), LOWER(value)) > 0` is the *primary*
translation, not the fallback. The `contains()`/`grepl(fixed=TRUE)` form named first in
the plan does not work here.

## 11. Filter-value escaping through dbplyr (verified safe)

Rendered `WHERE` clauses for hostile inputs — values are bound as escaped literals, never
interpolated:

```
[O'Brien]              -> WHERE ("name" = 'O''Brien')
[100%]                 -> WHERE ("name" = '100%')
[a_b]                  -> WHERE ("name" = 'a_b')
[say "hi"]             -> WHERE ("name" = 'say "hi"')
[back\slash]           -> WHERE ("name" = 'back\slash')
[-- comment]           -> WHERE ("name" = '-- comment')
['; DROP TABLE x; --]  -> WHERE ("name" = '''; DROP TABLE x; --')
[10..20]               -> WHERE ("name" = '10..20')
```

Apostrophes doubled, `%` and `_` literal (not wildcards, because `==` is not `LIKE`),
comment markers and injection strings inert. Plan §9's safety claim holds.

---

# Divergences from plan §1 — these change the plan

## D1. `prudence = "stingy"` does NOT block `collect()` — the guardrail is weaker than assumed

Plan §2.2 says "Assert eager access fails: `collect(src)` / `nrow(src)` error under
stingy." Only the second half is true. Observed:

```
nrow(s)                            duckdb_error: Materialization is disabled, use `collect()` or `as_tibble()` to materialize.
dim(s)                             duckdb_error: Materialization is disabled, use `collect()` or `as_tibble()` to materialize.
print(s) [capture]                 <NO ERROR>
head(s, 3)                         <NO ERROR>
collect(s)                         <NO ERROR>
as_tibble(s)                       <NO ERROR>
as.data.frame(s)                   <NO ERROR>
group_by(s, grp)                   rlang_error: This operation cannot be carried out by DuckDB, ...
```

And it really does materialize everything:

```
nrow(collect(s))     : 20000  (source has 20000 rows)
object.size(collect) : 4.3 Mb
```

`collect()` and `as_tibble()` are the *sanctioned* materialization escape hatches under
stingy — the DuckDB error message names them explicitly. Stingy blocks **automatic**
materialization and unsupported-verb fallbacks, not explicit collection.

**Consequences:**

- Plan §14 `test-source-duckplyr.R`'s planned assertion "eager `collect(src)` fails"
  is **unachievable** and must be dropped or rewritten.
- A stingy source does **not** make an accidental full materialization throw. The
  no-materialization invariant cannot lean on stingy as its safety net.
- Plan §14's query-log evidence and memory-scaling test therefore become **load-bearing
  primary evidence**, not supplementary cross-checks.
- What stingy *does* still prove: no silent dplyr fallback, and no automatic
  materialization on `nrow`/`dim`.

## D2. `group_by()` cannot be used in a stingy pipeline — spec's example workflow does not run

The pipeline written in `design/spec.md` ("Intended application workflow") and plan
§1.2/§2.2 uses `group_by() |> summarise()`. Under stingy that errors, verbatim:

```
rlang_error: This operation cannot be carried out by DuckDB, and the input is a
stingy duckplyr frame.
• Try `summarise(.by = ...)` or `mutate(.by = ...)` instead of `group_by()` and
  `ungroup()`.
ℹ Use `compute(prudence = "lavish")` to materialize to temporary storage and
  continue with duckplyr.
```

duckplyr's own recommended form works and stays lazy:

```r
src0 |>
  dplyr::filter(qty > 5, !is.na(grp)) |>
  dplyr::mutate(revenue = amount * qty, big = qty > 50) |>
  dplyr::summarise(n = dplyr::n(), total = sum(revenue), avg_price = mean(price),
                   first_dt = min(dt), .by = c(grp, big)) |>
  dplyr::select(grp, big, n, total, avg_price, first_dt)
# class: prudent_duckplyr_df, duckplyr_df, tbl_df, tbl, data.frame
```

This is an **app-facing constraint**: applications must use `summarise(.by=)`, not
`group_by()`, when `prudence = "stingy"`. It must be documented in the vignette, README,
and example app. The stop condition "a representative stingy pipeline cannot stay lazy"
does **not** trigger — a representative pipeline does stay lazy in the `.by` form.

## D3. `as_tbl()` yields a query over a randomly-named temp VIEW — breaks the plan's fingerprint

```
remote_query(t1): SELECT * FROM as_tbl_duckplyr_8yyOJEWM4T
remote_query(t2): SELECT * FROM as_tbl_duckplyr_S4QyPkfQNw
identical t1/t2 query: FALSE
remote_name(t1) : as_tbl_duckplyr_8yyOJEWM4T
```

Both `t1` and `t2` came from the **same** lazy duckplyr object. The referenced objects
are temp views:

```
                  table_name table_type          database_name schema_name
1 as_tbl_duckplyr_8yyOJEWM4T       VIEW                   temp        main
2 as_tbl_duckplyr_S4QyPkfQNw       VIEW                   temp        main
```

**Consequences for plan §7:**

- `fingerprint = rlang::hash(remote_query(base))` does **not** identify the query. It
  hashes a randomly generated view name, so it is effectively a per-`as_tbl()`-call
  random id.
- It therefore **cannot** detect a changed source, and two structurally identical
  sources hash differently. The plan's stated purpose for the fingerprint ("identifies
  the query; used to detect/reject a changed source") is not achievable this way.
- It still trivially keeps separate backends distinct — but their separate state
  environments already do that, so the fingerprint adds nothing as designed.
- The pipeline logic is hidden inside the view, so rendered base SQL never contains the
  app's filter/mutate/summarise expressions. Tests asserting on *our* predicates in
  rendered SQL still work, because we add those on top of the view via dbplyr (§11
  confirms). Tests asserting on the *app's* pipeline appearing in SQL would fail.

A different fingerprint source is needed — e.g. hashing the prototype schema plus the
expanded view definition from `duckdb_views()`, or accepting that source-change
detection is the application's responsibility (which plan §12 already requires). This
needs a decision before `backend.R` is written.

## D4. Minor — `AT TIME ZONE` needs the DuckDB `icu` extension

Generating a `TIMESTAMPTZ` column with `AT TIME ZONE 'UTC'` failed:

```
Extension Autoloading Error: An error occurred while trying to automatically install the required extension 'icu':
Extension ".../v1.5.0/osx_arm64/icu.duckdb_extension" not found.
```

`INSTALL icu` / `LOAD icu` succeed when the network is available, and a plain
`::TIMESTAMPTZ` cast works with no extension at all. Tests and the benchmark should use
the cast so they do not require network access or a preinstalled extension.

---

# Stop-condition evaluation (plan §3)

| # | Stop condition | Result | Evidence |
| --- | --- | --- | --- |
| 1 | reactable custom-backend API unavailable | **PASS** | §1, §2 — all three exported and functions. Obtained from GitHub, the canonical source for the development version. |
| 2 | Observed contract materially differs from plan §1.1 | **PASS** | §2, §4, §5, §6 — formals, `initialProps`, `do.call` sites, `rowCount` semantics, and `preRenderHook` behaviour all match §1.1 exactly. Divergences D1–D3 are against §1.2 (duckplyr), not §1.1. |
| 3 | duckplyr→dbplyr handoff materializes the full source | **PASS** | §7 — handoff yields a `tbl_dbi` over a view; only prototype (0 rows), scalar count, and bounded page were fetched. Note D1: stingy would not have caught a violation, so this rests on direct observation. |
| 4 | Connection cannot be retrieved via `dbplyr::remote_con()` | **PASS** | §7 — `duckdb_connection`, `dbIsValid() = TRUE`. |
| 5 | A representative stingy pipeline cannot stay lazy through `as_tbl()` | **PASS** (qualified) | §7 — the `summarise(.by=)` form stays lazy end-to-end. The `group_by()` form in spec/plan does not (D2). |
| 6 | Pagination would require unsupported reactable internals | **PASS** | §7 — `LIMIT`/`OFFSET` over `sql_render()` output via `DBI::dbGetQuery`; only exported `resolvedData()` is needed to return it. |

**No stop condition triggers. The gate passes.** Divergences D1–D3 require plan
amendments before implementation; D3 requires a design decision.

---

# 12. Observed Shiny HTTP behaviour (implementation phase, 2026-07-28)

Observed end-to-end with a real app (`callr` background process), a real Chrome
session (`chromote`, so the genuine reactable JS client issued requests), and crafted
`httr2` POSTs to the live session's dataobj URL. Reproduced continuously by
`tests/testthat/test-shiny-refusal.R`.

## 12.1 Real client request bodies (closes the §6 UNVERIFIED item)

Captured verbatim from Chrome's network layer. Initial render request:

```json
{"pageIndex":0,"pageSize":5,"sortBy":[],"filters":[],"groupBy":[],"expanded":{},"selectedRowIds":{}}
```

After typing in a column filter box:

```json
{"pageIndex":0,"pageSize":5,"sortBy":[],"filters":[{"id":"id","value":"name_5"}],"groupBy":[],"expanded":{},"selectedRowIds":{}}
```

- `filters` = array of `{id, value}` — the inferred shape is now confirmed observed.
- Empty `sortBy`/`filters`/`groupBy` arrive as `[]` (parsed to empty lists), and
  `expanded`/`selectedRowIds` as `{}`.
- The URL shape: `POST /session/<token>/dataobj/<outputId>?w=&nonce=<nonce>`.
- Note: the client fires the filter request per keystroke and also re-sends the
  *current* pageIndex with the new filter before resetting to page 0 — two requests per
  keystroke were observed. The debouncing guidance in the vignette stands.

## 12.2 What a thrown refusal looks like over HTTP

`reactableFilterFunc` has no `tryCatch`; the error propagates into Shiny's handler,
which answers:

```
HTTP 500, Content-Type: text/html
<html>...<title>An error has occurred</title>...
```

Observed identically for: an unknown-column filter, an oversized `pageSize`, and an
invalidated DuckDB connection. A valid crafted POST answers `200` with the
`resolvedData` JSON (`{"data":{...},"rowCount":N,"maxRowCount":null}`).

**Client-side, the failure is silent**: the table keeps its previous rows and simply
stops updating. No error UI appears. This confirms the risk that motivated the
log-and-rethrow wrapper.

## 12.3 Server-side logging (the mitigation, verified)

The app's stderr carries the package's structured log line *before* Shiny's own
traceback, e.g. (verbatim):

```
! reactableduckdb request failed (schema 4d1edb23): Unknown filter column
  "nope": not present in the source schema.
ℹ pageIndex=0 pageSize=5 sort=[] filter_ids=[nope]
Warning: Error in <Anonymous>: Unknown filter column "nope": not present in the source schema.
```

Filter *values* never appear in the log (asserted in the test) — only column ids.

## 12.4 Remaining v1 posture

Returning a structured JSON error payload (instead of throwing into Shiny's generic
500) would require shaping a `resolvedData()`-compatible error contract that the
upstream JS client does not currently understand — upstream renders only successful
payloads. v1 therefore keeps: refuse loudly server-side, log with context, document
that the client table freezes on a refused request. Revisit if upstream grows an
error-payload contract.
