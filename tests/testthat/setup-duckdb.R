# duckdb >= 1.5.5 prints a note on the first connection of a session, saying
# where it will keep downloaded extensions and secrets. It is emitted once per
# session, so it lands in whichever test happens to open that first connection
# -- and inside an expect_snapshot() it becomes part of the recorded output and
# fails the snapshot.
#
# That is exactly how it surfaced: green locally (duckdb 1.5.0, which has
# neither the message nor `shared_home`, on a machine that already had a
# ~/.duckdb) and red on CI (duckdb 1.5.5, no ~/.duckdb), in whichever snapshot
# test ran first.
#
# Spend it here, before any test runs, so no test can capture it. Tests that
# manage their own connections to exercise connection ownership are left
# untouched by this.
if (requireNamespace("duckdb", quietly = TRUE)) {
  local({
    con <- try(
      suppressMessages(DBI::dbConnect(duckdb::duckdb())),
      silent = TRUE
    )
    if (!inherits(con, "try-error")) {
      DBI::dbDisconnect(con, shutdown = TRUE)
    }
  })
}

# duckplyr's fallback notice ("Number of reports ready for upload: N") cannot
# be silenced from here. It is emitted by an on-load hook, so it has already
# printed by the time any setup file runs, and its header is unconditional --
# neither DUCKPLYR_FALLBACK_COLLECT nor DUCKPLYR_FALLBACK_INFO suppresses it
# (both checked). The only levers are duckplyr::fallback_config(), which
# persists settings to the developer's own machine rather than the project, and
# wrapping the whole run in suppressMessages(), which would hide real output
# too. It is noise in the test log and nothing more; left alone deliberately.
