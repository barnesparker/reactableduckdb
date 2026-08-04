# R CMD check's test entry point, and *only* that. R CMD check builds a
# tarball from the working tree, installs it into a temporary library, and
# runs this file against that build -- so the `library()` call below resolves
# to the code under test, not to a stale copy.
#
# Do not use this file to validate source changes during development: run
# `make test` (equivalently `devtools::test()`, or
# `testthat::test_local(".", load_package = "source")`), which loads the
# working tree with pkgload and never consults the installed library. See the
# "Validating against the source tree" section of CLAUDE.md.

library(testthat)
library(reactableduckdb)

test_check("reactableduckdb")
