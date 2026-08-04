# Observed contract — row identity and row selection

Everything below was observed empirically against the **installed** packages on
2026-08-04, driving a real Shiny app (`callr`) with a real Chrome session
(`chromote`), so the genuine reactable JS client issued the requests. Console
output, request bodies, and computed styles are reproduced **verbatim**.
Nothing here is filled in from prior knowledge. Items that could not be
verified are labelled **UNVERIFIED**.

Environment as in `reactable-server-contract.md` section 0; reactable
0.4.5.9000 at SHA `4a438397eaa83ec71a4dc3e895bbdcc595232d78`.

Probe scripts lived in a session scratch directory and are deliberately not
committed. The behaviour they established is asserted continuously by
`tests/testthat/test-selection.R`.

## 1. Without help, a served table identifies rows by page position

`selection` was previously passed through untouched, and documented as applying
"to the current page only". That description was wrong, in a way that produced
incorrect data rather than a missing feature.

2000 rows, `defaultPageSize = 5`, `selection = "multiple"`. The third row of
page 1 (`id = 2`) was clicked, then the pager advanced. Verbatim:

```
===== PAGE 1 after selecting 3rd row =====
[{"cells":["​","0","name_0"],"checked":false},{"cells":["​","1","name_1"],"checked":false},{"cells":["​","2","name_2"],"checked":true},...]
getReactableState selected:  SELECTED=[3]

===== PAGE 2 =====
[{"cells":["​","5","name_5"],"checked":false},{"cells":["​","6","name_6"],"checked":false},{"cells":["​","7","name_7"],"checked":true},...]
getReactableState selected:  SELECTED=[3]
```

Row `id = 7` renders checked on page 2. It was never selected. The request body
shows the cause:

```json
{"pageIndex":1,"pageSize":5,"sortBy":[],"filters":[],"groupBy":[],"expanded":{},"selectedRowIds":{"2":true}}
```

`"2"` is the row's index *within the delivered page*, not a row identity.
`getRowId()` falls back to it when the backend supplies no row state
(`Reactable.js` `getRowId`), and `autoResetSelectedRows: false` then carries
that index onto every later page.

## 2. `__state` supplies real row identity

The client honours an undocumented per-row state column for server backends,
`rowStateKey = '__state'` (`srcjs/columns.js`). `getRowId()` returns
`row.__state.id` when present, and `useServerSideRows` assigns
`row.index = rowState.index`.

Attaching `data.frame(id = <key as string>, index = <global offset>)` as the
page's `__state` column, same interaction sequence:

```
===== PAGE 2 (ghost selection gone?) =====
[{"cells":["​","5","name_5"],"checked":false},...,{"cells":["​","7","name_7"],"checked":false},...]
getReactableState:  SELECTED=[]

===== PAGE 2 after also selecting id=5 =====
getReactableState:  SELECTED=[6]

===== back on PAGE 1 (is id=2 still checked?) =====
[...,{"cells":["​","2","name_2"],"checked":true},...]
getReactableState:  SELECTED=[3]

visible header cells:  ["​","id","name"]
```

Established:

- ghost selection is gone;
- selection follows rows back and forth across pages;
- `selectedRowIds` keys become key values (`{"2":true,"5":true}` — note `"5"`,
  which is the *first* row of page 2, so the id is the key and not the
  position);
- `__state.index` feeds `getReactableState()`, which now reports global
  positions (id 5 → 6) instead of page-relative ones;
- `__state` renders no extra column and raises no error.

**This is an undocumented extension point.** If it is renamed or dropped
upstream, identity silently reverts to section 1 behaviour with no error
anywhere. `test-selection.R`'s browser test is the only thing that would catch
that, which is why it is not optional.

## 3. `getReactableState()` cannot report a cross-page selection

`selectedRowIndexes` is built by resolving each selected id against
`rowsById`, which under `manualPagination` holds only the delivered page
(`Reactable.js`, the `React.useMemo` over `state.selectedRowIds`). With ids 2
and 5 both selected, page 2 reported `SELECTED=[6]` and page 1 reported
`SELECTED=[3]` — never both.

The complete selection exists only in the `selectedRowIds` object the client
sends with each request. It is therefore read from `reactableServerData()`.

### The data handler runs with the serving session as its reactive domain

`reactableFilterFunc` is reached through `session$registerDataObj`, so whether
a reactive value could be published from it was not obvious. Verbatim, with
the app-visible outputs alongside:

```
[HANDLER] call 3 keys=[2]     domain=session:4f24666e
[HANDLER] call 5 keys=[2,5]   domain=session:4f24666e
[HANDLER] call 6 keys=[2,5,8] domain=session:4f24666e
[HANDLER] call 7 keys=[2,8]   domain=session:4f24666e

-- also selected id=5 on page 2 --   RV=[2,5]     STATE=[6]
-- also selected id=8 --             RV=[2,5,8]   STATE=[6,9]
-- deselected id=5 --                RV=[2,8]     STATE=[9]
```

- Setting a `reactiveVal` from inside the handler works; the value flushes.
- `shiny::getDefaultReactiveDomain()` returns the **serving session**, so a
  backend shared across sessions can key its selection registry by token
  rather than forcing per-session construction.
- The client re-sends the entire selection each time, so recording it is a
  replace and not a merge.

**UNVERIFIED:** behaviour under rapid successive clicks (a settle of ~2s was
used throughout), and isolation between two concurrent sessions — only one
session token was ever observed.

## 4. Select-all cannot be made correct, so it is neutralized

`toggleAllRowsSelected` iterates `rowsById` — one page. On 2000 rows the
header checkbox selected 5 and rendered itself as fully checked:

```
===== after clicking header select-all =====
getReactableState selected:  SELECTED=[1,2,3,4,5]
header checked:  TRUE
```

Upstream flags this itself, verbatim in `Reactable.js`:

```js
// TODO for when server-side row selection is implemented - need the ability to select all first
// manualRowSelectedKey: useServerData ? rowSelectedKey : null,
```

That commented line is **not** a sufficient fix. `manualRowSelectedKey` is
consulted only inside react-table's `defaultGetToggleRowSelectedProps`, and
reactable never calls it — it renders the checkbox itself from
`checked={row.isSelected}`, and reads `row.isSelected` in four places. Genuine
server-side selection would need that threaded through, a selection descriptor
in place of the `selectedRowIds` map, server semantics for
`toggleAllRowsSelected`, an affordance distinguishing "this page" from "all
matching", and a replacement for the index-based `getReactableState()` channel.
None of it is reachable from R.

The control is therefore neutralized via `headerStyle` on the `.selection`
colDef. Reactable merges a caller-supplied `.selection` colDef over its own
(`reactable.R`), and `selectable: TRUE` survives the merge, so per-row
checkboxes are unaffected. Measured in the browser:

| | unsuppressed | suppressed |
| --- | --- | --- |
| header checkbox computed `visibility` | `visible` | `hidden` |
| hit-testable at its own centre | `TRUE` | `FALSE` |
| header cell computed `pointer-events` | `auto` | `none` |
| real click at header cell centre | lands on `rt-select-input`, `SELECTED=[1,2,3,4,5]` | passes through to `rt-tr-header`, `SELECTED=[]` |
| per-row checkbox | works | works |
| keyboard focusable | — | `FALSE` |

A predicate-based selection ("everything matching filter F, less an exclusion
set") would suit a DuckDB backend well — it costs no extra query, since page
checkboxes are decided in R over at most `max_page_size` rows and the selected
count is the cached filtered count minus the exclusions. It is blocked purely
on the client's state model, not on anything about DuckDB or the
three-materializations invariant.

## 5. Row ids are strings, and the round-trip constrains key types

Row ids cross to the browser as JSON object keys, i.e. strings, and come back
as strings. A key type is usable only if its values survive that exactly.

No decimal floating-point format round-trips reliably through R. Measured over
9011 doubles (structured edge cases plus random normals, uniforms, and
exponentials), counting bitwise-identical recoveries:

```
as.character       exact:   587 /  9011
                     e.g. 3.00000000000000044409e-01 -> 0.3 -> 2.99999999999999988898e-01
sprintf %.17g      exact:  7383 /  9011
                     e.g. -2.49999999999999994774e-17 -> -2.4999999999999999e-17 -> -2.49999999999999963960e-17
sprintf %.18g      exact:  6416 /  9011
sprintf %a (hex)   exact:  9009 /  9011
format digits=17   exact:  7276 /  9011
deparse digits17   exact:  7383 /  9011
```

17 significant digits is the textbook sufficient precision for a double, so
`%.17g` failing is a property of R's `as.numeric()` not correctly rounding
every 17-digit input — worth knowing before reaching for it elsewhere.

DuckDB `BIGINT` arrives in R as a **double**, so integer keys land on the
double path and it cannot simply be refused. Whole numbers up to 2^53 have
exact decimal forms, so ids are formatted with `sprintf("%.0f", x)` and
fractional or oversized values are refused. 2^53 is the same exactness
boundary the package already applies to page offsets. A key compared by
floating-point equality is a defect regardless, so refusing it is the correct
outcome and not merely a limitation.

Supported key kinds: `character`/`factor`, `integer`, `integer64`, and
whole-number `double`. Others (notably `Date` and `POSIXct`) remain valid sort
tie-breakers but are refused for `selection`, at widget construction.
