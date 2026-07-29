counters <- function(b) c(hits = b$state$hits, misses = b$state$misses)

test_that("count cache: miss on first request, hit on later pages", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000)
  server_data(b, pageIndex = 0)
  expect_identical(counters(b), c(hits = 0L, misses = 1L))
  server_data(b, pageIndex = 3)
  expect_identical(counters(b), c(hits = 1L, misses = 1L))
})

test_that("sort-only changes reuse the cached count", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000, key = "id")
  filters <- list(list(id = "qty", value = ">=50"))
  server_data(b, filters = filters)
  server_data(b, filters = filters, sortBy = list(list(id = "amount", desc = TRUE)))
  server_data(b, filters = filters, pageIndex = 2, sortBy = list(list(id = "grp", desc = FALSE)))
  expect_identical(counters(b), c(hits = 2L, misses = 1L))
})

test_that("changing filters is a different cache key", {
  skip_if_no_backend_api()
  b <- local_backend(n = 2000)
  server_data(b, filters = list(list(id = "qty", value = ">=50")))
  server_data(b, filters = list(list(id = "qty", value = ">=51")))
  expect_identical(counters(b), c(hits = 0L, misses = 2L))
  # Same filters expressed in a different order canonicalize to one key.
  server_data(b, filters = list(
    list(id = "qty", value = ">=50"),
    list(id = "name", value = "name_1")
  ))
  server_data(b, filters = list(
    list(id = "name", value = "name_1"),
    list(id = "qty", value = ">=50")
  ))
  expect_identical(counters(b), c(hits = 1L, misses = 3L))
})

test_that("separate backends have separate caches", {
  skip_if_no_backend_api()
  b1 <- local_backend(n = 500)
  b2 <- local_backend(n = 500)
  server_data(b1)
  expect_identical(counters(b2), c(hits = 0L, misses = 0L))
  server_data(b2)
  expect_identical(counters(b2), c(hits = 0L, misses = 1L))
})

test_that("cache_counts = FALSE bypasses the cache entirely", {
  skip_if_no_backend_api()
  tbl <- local_duckdb_tbl(n = 500)
  b <- reactable_duckdb_backend(tbl, cache_counts = FALSE)
  reactableduckdb:::enable_query_log(b)
  server_data(b, pageIndex = 0)
  server_data(b, pageIndex = 1)
  expect_identical(counters(b), c(hits = 0L, misses = 0L))
  expect_length(b$state$cache, 0)
  # Every request re-counts.
  count_queries <- grep("reactableduckdb_count", query_log(b), fixed = TRUE)
  expect_length(count_queries, 2)
})

test_that("the LRU cache is bounded and evicts the least recently used", {
  skip_if_no_backend_api()
  b <- local_backend(n = 500)
  cap <- reactableduckdb:::count_cache_capacity
  for (i in seq_len(cap + 5)) {
    server_data(b, filters = list(list(id = "qty", value = paste0(">=", i %% 100))))
  }
  expect_lte(length(b$state$cache), cap)
  expect_identical(length(b$state$order), length(b$state$cache))
})

test_that("warmed count from reactableServerInit makes construction a hit", {
  skip_if_no_backend_api()
  b <- local_backend(n = 500)
  reactable::reactableServerInit(b, pageIndex = 0, pageSize = 10)
  expect_identical(counters(b), c(hits = 0L, misses = 1L))
  server_data(b)
  expect_identical(counters(b), c(hits = 1L, misses = 1L))
})

test_that("the query log is a bounded ring with a dropped counter", {
  skip_if_no_backend_api()
  b <- local_backend(n = 200)
  reactableduckdb:::enable_query_log(b)
  cap <- reactableduckdb:::query_log_capacity
  for (i in seq_len(110)) {
    server_data(b, filters = list(list(id = "qty", value = paste0(">=", i %% 100))))
  }
  expect_lte(length(b$state$query_log), cap)
  expect_gt(b$state$query_log_dropped, 0L)
  # Disabled by default: a fresh backend logs nothing.
  b2 <- local_backend(n = 100)
  server_data(b2)
  expect_length(b2$state$query_log, 0)
})
