# reactableduckdb example app
#
# Self-contained: builds a temporary pins board, writes synthetic Parquet,
# downloads it back with pin_download(), reads it lazily with
# read_parquet_duckdb(prudence = "stingy"), applies a lazy duckplyr
# transformation (note: summarise(.by = ) — group_by() is refused under
# stingy), and serves it through reactable_duckdb(). No external board or
# network access is needed.
#
# The `tags` LIST column is display-only: it renders, but has no filter box
# and is not sortable — that is the capability merge working, not a bug.
#
# ---- Regenerating the README/vignette screenshot ---------------------------
# The screenshot at man/figures/example-app.png is a MANUAL artifact and can
# drift as the UI changes. To regenerate after UI changes:
#
#   1. shiny::runApp(system.file("examples", package = "reactableduckdb"),
#                    port = 8642)     # or run this file from the source tree
#   2. In a second R session:
#        b <- chromote::ChromoteSession$new(width = 1200, height = 800)
#        b$Page$navigate("http://127.0.0.1:8642"); Sys.sleep(3)
#        b$screenshot("man/figures/example-app.png", selector = "body")
#   3. Commit the new PNG. The image caption flags it as manually captured.
# ----------------------------------------------------------------------------

# ---- Running this app -------------------------------------------------------
#   shiny::runApp(system.file("examples", package = "reactableduckdb"))
# or, from a checkout of the source tree:
#   shiny::runApp("inst/examples/app.R")
# Both need reactableduckdb to be *installed* (see the check below); in a
# development checkout, devtools::load_all() also satisfies it.
# ----------------------------------------------------------------------------

local({
  needed <- c(
    reactableduckdb = "R CMD INSTALL . (or devtools::install())",
    reactable = "pak::pak(\"glin/reactable\")  # development version required",
    duckdb = "install.packages(\"duckdb\")",
    duckplyr = "install.packages(\"duckplyr\")",
    shiny = "install.packages(\"shiny\")",
    bslib = "install.packages(\"bslib\")",
    pins = "install.packages(\"pins\")"
  )
  # suppressMessages() here because loading duckplyr's namespace prints its
  # fallback-telemetry banner ("N reports ready for upload"), which is
  # informational but reads like a failure in an example app.
  missing <- names(needed)[
    !suppressMessages(
      vapply(names(needed), requireNamespace, logical(1), quietly = TRUE)
    )
  ]
  if (length(missing) > 0) {
    stop(
      "The reactableduckdb example app cannot start; ",
      length(missing),
      " package(s) are not installed:\n",
      paste0(
        "  - ",
        missing,
        "   install with: ",
        needed[missing],
        collapse = "\n"
      ),
      "\n",
      call. = FALSE
    )
  }
})

library(shiny)

# 1. The application owns its pins + data pipeline ---------------------------
board <- pins::board_temp()

local({
  staging <- file.path(tempdir(), "example-flights.parquet")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(
    con,
    sprintf(
      "COPY (
       SELECT i AS flight_id,
              'CARRIER_' || (i %% 12) AS carrier,
              CASE WHEN i %% 11 = 0 THEN NULL
                   ELSE 'APT_' || (i %% 40) END AS origin,
              (i %% 3000)::INTEGER AS distance_mi,
              ((i * 17) %% 480 - 60)::DOUBLE AS delay_min,
              DATE '2026-01-01' + (i %% 365)::INTEGER AS flight_date,
              (i %% 4 = 0) AS is_international,
              [i %% 5, i %% 7] AS gate_history
       FROM range(100000) r(i)
     ) TO '%s' (FORMAT PARQUET)",
      staging
    )
  )
  pins::pin_upload(board, staging, "example-flights")
})

pin_path <- pins::pin_download(board, "example-flights")

# 2. Lazy duckplyr pipeline (stays in DuckDB; stingy refuses fallbacks) ------
source_data <- duckplyr::read_parquet_duckdb(pin_path, prudence = "stingy") |>
  dplyr::filter(distance_mi > 0) |>
  dplyr::mutate(
    delay_hr = delay_min / 60,
    long_haul = distance_mi > 1500
  ) |>
  dplyr::select(
    flight_id,
    carrier,
    origin,
    flight_date,
    distance_mi,
    delay_min,
    delay_hr,
    long_haul,
    is_international,
    gate_history
  )

# 3. The package exposes the lazy result through reactable ------------------
backend <- reactableduckdb::reactable_duckdb_backend(
  source_data,
  key = "flight_id"
)

ui <- bslib::page_sidebar(
  title = "reactableduckdb — 100,000 rows served lazily from DuckDB",
  sidebar = bslib::sidebar(
    p(
      "Filtering, sorting, counting, and pagination all run in DuckDB.",
      "R only ever holds the page you see."
    ),
    p(
      strong("Try:"),
      "type",
      code("CARRIER_3"),
      "in the carrier filter,",
      code(">=1500"),
      "in distance,",
      code("2026-06-01..2026-06-30"),
      "in the date column, or",
      code("true"),
      "in international."
    ),
    p(
      "The",
      code("gate_history"),
      "column is a DuckDB LIST: it renders,",
      "but is deliberately not filterable or sortable (display-only)."
    )
  ),
  bslib::card(reactable::reactableOutput("flights"))
)

server <- function(input, output, session) {
  output$flights <- reactable::renderReactable(
    reactableduckdb::reactable_duckdb(
      backend,
      filterable = TRUE,
      defaultPageSize = 50L,
      columns = list(
        gate_history = reactable::colDef(name = "Gate history (LIST)"),
        delay_hr = reactable::colDef(format = reactable::colFormat(digits = 2))
      )
    )
  )
}

shinyApp(ui, server)
