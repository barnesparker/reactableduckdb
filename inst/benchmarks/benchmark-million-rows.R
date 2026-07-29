# Development-only benchmark (plan section 16). Not run by tests or CI.
#
# Generates >= 1,000,000 rows of temporary Parquet (never committed) and
# measures the backend over three source shapes:
#   (a) flat scan,
#   (b) summarise(.by = ) aggregate  — group_by() is refused under stingy,
#   (c) a join,
# because for (b)/(c) every filtered count must re-run the aggregation and
# cannot be short-circuited.
#
# There is deliberately no fixed memory threshold: the structural proof that
# only bounded data is materialized is the row count of every result, plus
# the allocation column reported by bench (which should not scale with the
# source-size column). Outputs are written next to this script and are both
# .gitignore'd and .Rbuildignore'd.

stopifnot(
  requireNamespace("bench", quietly = TRUE),
  requireNamespace("duckplyr", quietly = TRUE),
  requireNamespace("reactableduckdb", quietly = TRUE)
)

n_rows <- 1200000L
out_dir <- tempfile("reactableduckdb-bench-")
dir.create(out_dir)
flights_pq <- file.path(out_dir, "flights.parquet")
carriers_pq <- file.path(out_dir, "carriers.parquet")

message("Generating ", format(n_rows, big.mark = ","), " rows of Parquet...")
con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, sprintf(
  "COPY (
     SELECT i AS flight_id,
            'CARRIER_' || (i %% 40) AS carrier,
            CASE WHEN i %% 11 = 0 THEN NULL ELSE 'APT_' || (i %% 300) END AS origin,
            (i %% 5000)::INTEGER AS distance_mi,
            ((i * 17) %% 480 - 60)::DOUBLE AS delay_min,
            DATE '2026-01-01' + (i %% 365)::INTEGER AS flight_date,
            (i %% 4 = 0) AS is_international
     FROM range(%d) r(i)
   ) TO '%s' (FORMAT PARQUET)",
  n_rows, flights_pq
))
DBI::dbExecute(con, sprintf(
  "COPY (
     SELECT 'CARRIER_' || i AS carrier, 'Carrier name %%' || i AS carrier_name
     FROM range(40) r(i)
   ) TO '%s' (FORMAT PARQUET)",
  carriers_pq
))
DBI::dbDisconnect(con, shutdown = TRUE)

flat <- duckplyr::read_parquet_duckdb(flights_pq, prudence = "stingy")

aggregated <- flat |>
  dplyr::filter(!is.na(origin)) |>
  dplyr::summarise(
    flights = dplyr::n(),
    mean_delay = mean(delay_min),
    max_distance = max(distance_mi),
    .by = c(carrier, origin, is_international)
  )

joined <- flat |>
  dplyr::inner_join(
    duckplyr::read_parquet_duckdb(carriers_pq, prudence = "stingy"),
    by = "carrier"
  ) |>
  dplyr::mutate(delay_hr = delay_min / 60)

shapes <- list(flat = flat, aggregated = aggregated, joined = joined)

filters_for <- function(shape) {
  switch(shape,
    flat = list(list(id = "carrier", value = "CARRIER_1")),
    aggregated = list(list(id = "flights", value = ">=1000")),
    joined = list(list(id = "carrier_name", value = "name %1"))
  )
}
sort_for <- function(shape) {
  switch(shape,
    flat = list(list(id = "delay_min", desc = TRUE)),
    aggregated = list(list(id = "mean_delay", desc = TRUE)),
    joined = list(list(id = "delay_hr", desc = TRUE))
  )
}

results <- list()
for (shape in names(shapes)) {
  message("Benchmarking shape: ", shape)
  key <- if (shape == "flat" || shape == "joined") "flight_id" else NULL
  backend <- NULL
  request <- function(...) {
    rd <- reactable::reactableServerData(backend, pageSize = 50, ...)
    stopifnot(nrow(rd$data) <= 50, length(rd$rowCount) == 1)
    rd
  }
  bm <- bench::mark(
    backend_creation = {
      backend <- reactableduckdb::reactable_duckdb_backend(shapes[[shape]], key = key)
    },
    count_and_first_page = request(pageIndex = 0),
    filtered_page = request(pageIndex = 0, filters = filters_for(shape)),
    sorted_page = request(pageIndex = 1, sortBy = sort_for(shape)),
    repeated_page_cached_count = request(pageIndex = 2),
    iterations = 5,
    check = FALSE,
    memory = TRUE
  )
  bm$shape <- shape
  results[[shape]] <- bm
  print(bm[, c("expression", "median", "mem_alloc", "n_gc")])
}

all_results <- do.call(rbind, results)
saveRDS(all_results, "benchmark-results.rds")
message("\nStructural proof: every request above returned <= 50 rows and a ",
        "scalar count, against a ", format(n_rows, big.mark = ","),
        "-row source. Results saved to benchmark-results.rds")
unlink(out_dir, recursive = TRUE)
