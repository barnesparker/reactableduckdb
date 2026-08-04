# Canonical commands for reactableduckdb.
#
# The working source tree is authoritative. Every target below loads the
# package from source with pkgload; none consults the installed library, so
# none can validate a stale copy. See the "Validating against the source
# tree" section of CLAUDE.md.

.PHONY: test test-file check document install-dev clean

## test: run the full suite against the working source tree
test:
	Rscript -e 'testthat::test_local(".", load_package = "source")'

## test-file: run one file against the source tree, e.g. make test-file FILTER=selection
test-file:
	@test -n "$(FILTER)" || { echo "usage: make test-file FILTER=<pattern>"; exit 1; }
	Rscript -e 'testthat::test_local(".", load_package = "source", filter = "$(FILTER)")'

## document: regenerate NAMESPACE and man/ from roxygen comments
document:
	Rscript -e 'devtools::document()'

## check: full R CMD check -- builds a tarball from this tree and tests that
check:
	Rscript -e 'devtools::check(document = FALSE)'

## install-dev: install this tree into the user library (NOT needed to run tests)
install-dev:
	Rscript -e 'devtools::install(upgrade = "never")'

## clean: remove an installed copy, so nothing can shadow the source tree
clean:
	Rscript -e 'try(remove.packages("reactableduckdb"), silent = TRUE)'
