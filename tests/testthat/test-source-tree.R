# The working source tree is authoritative: the suite must never validate an
# installed copy of the package.
#
# This is not a hypothetical. `tryCatch(library(reactableduckdb), error =
# load_all(...))` in a spawned app process used to mean that whenever the
# package happened to be installed, `library()` succeeded and the test
# exercised the installed build instead of the code under test — passing or
# failing for the wrong reasons, with nothing to show for it.
#
# These tests fail loudly if that shape returns.

test_that("the suite runs against the source tree, not an installed copy", {
  # devtools::test() and testthat::test_local(".", load_package = "source")
  # both load the working tree with pkgload. R CMD check is the deliberate
  # exception: it installs a build of *this* source and runs tests/testthat.R
  # against it, which is the point of that check.
  skip_if(
    testthat::is_checking(),
    "under R CMD check, which tests an installed build of this source by design"
  )
  expect_identical(pkgload::is_dev_package("reactableduckdb"), TRUE)
})

test_that("no test file loads reactableduckdb itself", {
  files <- list.files(test_path(), pattern = "[.][Rr]$", full.names = TRUE)
  expect_gt(length(files), 0)

  # Each pattern loads or resolves an *installed* package. A test file that
  # needs the package in a spawned process must use
  # pkgload::load_all(<pkg dir>) instead, which loads the source tree.
  forbidden <- c(
    "library\\(\\s*reactableduckdb",
    "require\\(\\s*reactableduckdb",
    "requireNamespace\\(\\s*[\"']reactableduckdb",
    "loadNamespace\\(\\s*[\"']reactableduckdb",
    "test_package\\(\\s*[\"']reactableduckdb",
    "attachNamespace\\(\\s*[\"']reactableduckdb"
  )

  offenders <- character(0)
  for (file in files) {
    # Strip comments so this file's own prose does not trip the check.
    lines <- readLines(file, warn = FALSE)
    lines <- sub("#.*$", "", lines)
    for (pattern in forbidden) {
      hits <- grep(pattern, lines)
      if (length(hits) > 0) {
        offenders <- c(offenders, sprintf("%s:%d", basename(file), hits))
      }
    }
  }
  expect_identical(offenders, character(0))
})

test_that("an installed package directory does not read as a source tree", {
  # skip_without_source_tree() decides whether a spawned app process may
  # pkgload::load_all() a directory. An installed package root has a
  # DESCRIPTION, so DESCRIPTION alone cannot be the test -- covr runs the
  # suite from inside one. Any installed package is a fine stand-in.
  installed <- find.package("testthat")
  expect_true(file.exists(file.path(installed, "DESCRIPTION")))
  expect_false(has_source_tree(installed))

  # ...and the real tree still passes. Only meaningful when there *is* one:
  # under R CMD check and under covr the suite runs from an installed build,
  # and covr's layout in particular puts an installed package root exactly
  # where this looks -- which is the whole reason the check above exists.
  skip_if(
    testthat::is_checking(),
    "running from an installed build (R CMD check / covr), so there is no tree here"
  )
  source_tree <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  expect_true(has_source_tree(source_tree))
})

test_that("no test file falls back to load_all only when loading fails", {
  files <- list.files(test_path(), pattern = "[.][Rr]$", full.names = TRUE)
  offenders <- character(0)
  for (file in files) {
    text <- paste(
      sub("#.*$", "", readLines(file, warn = FALSE)),
      collapse = "\n"
    )
    # A conditional load is the exact shape of the original defect: the
    # source tree must be loaded unconditionally, never as a fallback.
    if (
      grepl("tryCatch[^)]*reactableduckdb[\\s\\S]*load_all", text, perl = TRUE)
    ) {
      offenders <- c(offenders, basename(file))
    }
  }
  expect_identical(offenders, character(0))
})
