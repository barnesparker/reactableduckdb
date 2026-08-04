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

## Validating against the source tree (mandatory)

**The working source tree is authoritative.** A test run that loads an installed copy of
reactableduckdb proves nothing about the code you just changed.

- **Test files must not load reactableduckdb themselves.** No `library(reactableduckdb)`,
  `require()`, `requireNamespace()`, or `loadNamespace()` anywhere under `tests/testthat/`.
  The runner supplies the package.
- **Run tests with `make test`** — equivalently `devtools::test()` or
  `testthat::test_local(".", load_package = "source")`. All three load the working tree
  with pkgload.
- **Never use `library(reactableduckdb)` or `testthat::test_package("reactableduckdb")` to
  validate source changes.** Both resolve to the installed library and will happily pass
  against code you did not write.
- **`pkgload::load_all()` is for interactive development outside the test suite.** The one
  exception inside the suite is a spawned R process (`callr::r_bg()`), which is a fresh
  session and must load the source tree explicitly. Call
  `pkgload::load_all(pkg_dir, quiet = TRUE)` **unconditionally**, followed by
  `stopifnot(pkgload::is_dev_package("reactableduckdb"))`. Never make it a `tryCatch()`
  fallback behind `library()` — that shape silently tests the installed build whenever one
  exists, which is exactly the bug this rule exists to prevent.
- `tests/testthat.R` is R CMD check's entry point and the sole sanctioned `library()` call.
  R CMD check installs a build of *this* tree, so it is not a stale copy. Do not use it as
  a development loop.

`tests/testthat/test-source-tree.R` enforces the first and fourth bullets and asserts the
suite is running against a dev package. If it fails, fix the loading, not the test.

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
