# a double key that is not whole-number-valued is refused

    Code
      format_row_ids(c(1, 2.5), "double")
    Condition
      Error:
      ! The `key` column has values that cannot be a row identity: 2.5.
      i A key stored as a double must hold whole numbers no larger than 2^53, so that its row ids survive the round-trip through the browser exactly.
      * Use an integer, integer64, or character key column.

---

    Code
      format_row_ids(2^53 + 2, "double")
    Condition
      Error:
      ! The `key` column has values that cannot be a row identity: 9007199254740994.
      i A key stored as a double must hold whole numbers no larger than 2^53, so that its row ids survive the round-trip through the browser exactly.
      * Use an integer, integer64, or character key column.

---

    Code
      format_row_ids(c(1, Inf), "double")
    Condition
      Error:
      ! The `key` column has values that cannot be a row identity: Inf.
      i A key stored as a double must hold whole numbers no larger than 2^53, so that its row ids survive the round-trip through the browser exactly.
      * Use an integer, integer64, or character key column.

# unreadable row ids are refused, never dropped

    Code
      parse_row_ids("abc", "double")
    Condition
      Error:
      ! Could not read selected row id "abc" as type double.
      i Selected row ids are the `key` column formatted as strings; this means the widget's rows and the backend's schema disagree.

---

    Code
      parse_row_ids(c("1", "x"), "integer")
    Condition
      Error:
      ! Could not read selected row id "x" as type integer.
      i Selected row ids are the `key` column formatted as strings; this means the widget's rows and the backend's schema disagree.

# a source column named __state is refused

    Code
      reactable_duckdb_backend(dplyr::tbl(con, "clash"), key = "id")
    Condition
      Error in `reactable_duckdb_backend()`:
      ! The source has a column named "__state", which reactable reserves for per-row state.
      i Rename or drop it in the source query.

# selection requires a key of a round-trippable type

    Code
      reactable_duckdb(local_backend(n = 50), selection = "multiple")
    Condition
      Error in `reactable_duckdb()`:
      ! `selection` requires the backend to have a `key`.
      i Without one, reactable identifies a row by its position in the delivered page, so a selection follows positions across pages instead of rows.
      * Rebuild the backend with `reactable_duckdb_backend(..., key = "<column>")`.

---

    Code
      reactable_duckdb(b, selection = "single")
    Condition
      Error in `reactable_duckdb()`:
      ! `selection` does not support a `key` of type <Date>.
      i Row ids cross to the browser as strings, and this type cannot be recovered from one exactly.
      * Use a character, integer, double, or integer64 key column.

# index-based selection arguments are refused

    Code
      reactable_duckdb(b, selection = "multiple", defaultSelected = c(1, 2))
    Condition
      Error in `reactable_duckdb()`:
      ! `defaultSelected` is not supported: it selects by row index, but a served table identifies rows by their `key` value, so an index would select the wrong rows.

---

    Code
      reactable_duckdb(b, selection = "multiple", selectionId = "sel")
    Condition
      Error in `reactable_duckdb()`:
      ! `selectionId` is not supported: it reports row indices for the current page only. Use `reactable_duckdb_selected_keys()` instead.

---

    Code
      reactable_duckdb(b, defaultSelected = c(1, 2))
    Condition
      Error:
      ! `defaultSelected` row indices must be within range

# selection accessors refuse outside a session or without a key

    Code
      reactable_duckdb_selected_keys(b)
    Condition
      Error in `reactable_duckdb_selected_keys()`:
      ! No Shiny session is active.
      i Call this inside a Shiny `server` function. Selection is delivered by the widget's data requests, which only exist in a live session.

---

    Code
      reactable_duckdb_selected_keys(keyless)
    Condition
      Error in `reactable_duckdb_selected_keys()`:
      ! This backend has no `key`, so its rows have no stable identity.
      i Rebuild it with `reactable_duckdb_backend(..., key = "<column>")` to use selection.

