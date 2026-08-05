# Filter contract — `reactableduckdb` v1

The grammar users can type into reactable's per-column filter boxes, and
exactly what it does. Parsing happens in R (`R/filters.R`); parsed values are
bound into the lazy query through dbplyr expressions, which escape them.
**No user-entered text is ever pasted into SQL.** Malformed input raises a
structured `reactableduckdb_filter_error`, never a raw database error.

## Column capability

Capability is inferred once, at backend construction, from the zero-row
prototype's R types:

| R type (DuckDB origin) | Filter grammar | Sortable |
| --- | --- | --- |
| character, factor (VARCHAR, ENUM) | text | yes |
| integer, double, numeric, integer64, DECIMAL → double | numeric | yes |
| Date (DATE) | date | yes |
| POSIXct/POSIXt (TIMESTAMP, TIMESTAMPTZ) | datetime | yes |
| logical (BOOLEAN) | logical | yes |
| list (LIST/MAP), data.frame (STRUCT), blob/raw, anything else | **display-only** | no |

Display-only columns render but are neither filterable nor sortable; the
widget disables both automatically, and a request against one (or against an
unknown column id) errors. `filter_spec` may override per column: `exact =
TRUE` (text only) or `type` pinning one of the five grammars (never on a
display-only column).

A pinned `type` selects the SQL predicate built for the column, so it is only
accepted when the column's SQL type can evaluate it:

| Inferred grammar | May be pinned to |
| --- | --- |
| text | `text` |
| numeric | `numeric` |
| date | `date`, `datetime` |
| datetime | `date`, `datetime` |
| logical | `logical` |

`DATE` and `TIMESTAMP` compare freely in DuckDB, which is what makes the one
cross-type pin both valid and useful — a timestamp column can be filtered at
date granularity, or a date column at datetime precision. Every other
combination is refused at construction. Measured, not assumed: the rest
produced raw DuckDB binder errors on the first request, except numeric →
logical, which bound successfully and returned a surprising row set.

## Text (default: case-insensitive literal substring)

- Translation: `strpos(LOWER(col), LOWER(value)) > 0` — the sole translation
  (the `grepl(fixed = TRUE)` route is rejected by the DuckDB dbplyr backend).
- The value is **literal and untrimmed**: every character counts, including
  leading/trailing whitespace.
- Empty input (`""`) means **no filter**.
- `%` and `_` are literal — this is not `LIKE`, so they are never wildcards.
- Apostrophes, quotes, backslashes, comment markers (`--`), and
  injection-shaped strings are safe bound literals (test-covered).
- Input that looks like another grammar (`10..20`, `>=5`, `true`) stays a
  plain substring search on a text column.
- With `filter_spec = list(col = list(exact = TRUE))`: exact equality
  (`col == value`), still case-sensitive as stored.

## Numeric

Whitespace around operators and values is accepted. An unadorned value means
equality.

```
10        -5        1.5       1e6          (equality)
>=10      >-5       <=10      <-5          (comparators)
10..20    -10..-5   1e2..2e2               (inclusive range)
```

- Numbers: optional sign, decimals, scientific notation.
- A range whose lower bound exceeds its upper bound is refused.
- Anything else (`abc`, `>>5`, `1..2..3`, `5..`, `1_000`) is refused.

## Date

ISO dates only, same comparator/range forms:

```
2026-01-01     >=2026-01-01     <=2026-01-31     2026-01-01..2026-01-31
```

Impossible calendar dates (`2026-13-01`, `2026-02-30`) are refused.

## Datetime

ISO date or datetime, **interpreted as UTC**, compared as instants:

```
2026-01-01                    (midnight UTC, exact instant)
2026-01-01 12:00:00           (space or T separator; seconds optional)
>=2026-01-01 12:00            2026-01-01..2026-01-02 06:00:00
```

A bare date means the exact midnight instant — equality on a bare date
matches only rows at exactly `00:00:00`. Use a range to cover a day. This is
deliberately literal rather than a hidden day-window interpretation.

Time components are range-checked as part of the shape: hours `00`–`23`,
minutes and seconds `00`–`59`. An out-of-range time (`2026-01-01 25:00:00`,
`12:99:00`) is refused. It has to be caught here rather than by the parser:
`as.POSIXct()`'s format fallback ends at `"%Y-%m-%d"`, and `strptime()`
matches that as a *prefix*, so a loose pattern silently turned such a value
into midnight on that date and filtered against an instant the user never
typed.

## Logical

Case-insensitive, whitespace-tolerant: `true`, `false`, `t`, `f`, `1`, `0`.
Anything else is refused.

## Missing values

Filtering for NULL/missing values is **deferred in v1** (no grammar token is
reserved). Rows with NULL in a filtered column simply never match any filter
(standard SQL three-valued logic). Sorting places NULLs last in both
directions.

## Cache semantics

The filtered count is cached per backend under a canonical key: the parsed
`(id, op, normalized values, type)` tuples sorted by id. Page and sort
changes reuse the count; any change to the effective filter set does not.
