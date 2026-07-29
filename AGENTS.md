# reactableduckdb

An R package that streams a lazy DuckDB query — a `duckplyr_df` or a dbplyr
`tbl_sql`/`tbl_dbi` — into a `reactable` widget through reactable's custom server-side
data backend API. DuckDB performs all filtering, counting, sorting, and pagination; the
package owns none of the application's data pipeline (no pins, no transformations, no
hidden global database, no connections it did not create).

## The invariant (non-negotiable)

Only three materializations into R are permitted:

1. a zero-row schema prototype,
2. a scalar filtered count,
3. the requested page of rows.

Nothing else may be collected. The complete source dataset must never enter R. Any change
that adds a fourth materialization is wrong, regardless of how convenient it is.

## The refusal principle

Unimplemented behaviour errors loudly. It never silently returns wrong data and never
falls back to eager dplyr execution. Unknown filter/sort column ids, unfilterable column
types, oversized page requests, and unsupported widget arguments all raise structured
errors — they are never ignored, clamped, or approximated. A degraded-but-quiet result is
a defect, not a fallback.

## Dependency fact

The hard requirement is the **reactable development version** (>= 0.4.5.9000), which
exports the custom backend API: `reactableServerInit()`, `reactableServerData()`,
`resolvedData()`. Its canonical source is GitHub (`glin/reactable`). A Posit Package
Manager repository that **mirrors that repository** is a convenient channel, not a
requirement — never describe it as one, and never name a specific organization's
instance. DESCRIPTION carries no `Remotes:` entry, so either channel works.

Because a mirror tracks the latest repo, the dependency is a moving target. Detect the
API by subset checks, never by exact signature equality, and **warn** rather than error
when detection fails: a stale check is as likely as a broken install, and a genuinely
missing API still fails loudly from reactable itself.

## Design documents (authoritative)

- `design/reactable-server-contract.md` — the empirically observed upstream reactable
  contract. Recorded verbatim from the installed package; anything unverified is labelled
  as such. Never fill its gaps from prior knowledge — reactable's dev API is unstable and
  training-data recall about it is unreliable.
- `design/filter-contract.md` — the per-column filter grammar and capability map.

Both are records of observed behaviour, not wish lists. They cite an earlier spec and
implementation plan that are no longer kept in the repository; those citations are
dangling by design, and the shipped source and tests are the current authority on
mechanics.

## Evidence rule

Never claim a command succeeded without pasting its actual output. Never claim a phase is
complete without pasted `devtools::test()` output. "Confirmed", "verified", and "passing"
are not claims that can be made without the console text that backs them. Report failures
and skipped steps explicitly.
