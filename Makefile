# Canonical commands for reactableduckdb.
#
# The working source tree is authoritative. Every target below builds from
# this tree and never from a pre-existing installed copy, so none of them can
# validate a stale build: `test`/`test-file` load the tree with pkgload, and
# `check`/`coverage` install a fresh build of it. See the "Validating against
# the source tree" section of CLAUDE.md.
#
# LANG: a non-UTF-8 locale (R's default "C") cannot represent the non-ASCII
# column name in local_weird_tbl(), so the non-syntactic column-name tests --
# the only end-to-end exercise of quote_ident() -- skip themselves. Default to
# a UTF-8 locale so they actually run; an existing LANG in the environment
# wins.
LANG ?= en_US.UTF-8
export LANG

# Pandoc is installed on this machine, but bundled inside Quarto rather than on
# PATH. R finds pandoc through RSTUDIO_PANDOC, which RStudio sets and a bare
# Rscript does not -- so without this `make check` fails to build the vignette
# and looks like pandoc is missing. Pick the first bundled binary that actually
# runs (Quarto ships both aarch64 and x86_64). An RSTUDIO_PANDOC already in the
# environment always wins, and on CI setup-pandoc supplies one, so this is a
# local convenience only.
ifndef RSTUDIO_PANDOC
RSTUDIO_PANDOC := $(shell for d in /Applications/quarto/bin/tools/*/ /usr/local/lib/quarto/bin/tools/*/ /opt/quarto/bin/tools/*/; do \
    [ -x "$$d/pandoc" ] && "$$d/pandoc" --version >/dev/null 2>&1 && { echo "$${d%/}"; break; }; \
  done 2>/dev/null)
endif
ifneq ($(RSTUDIO_PANDOC),)
export RSTUDIO_PANDOC
endif

.PHONY: test test-file coverage coverage-report check document readme install-dev clean

## test: run the full suite against the working source tree
test:
	Rscript -e 'testthat::test_local(".", load_package = "source")'

## test-file: run one file against the source tree, e.g. make test-file FILTER=selection
test-file:
	@test -n "$(FILTER)" || { echo "usage: make test-file FILTER=<pattern>"; exit 1; }
	Rscript -e 'testthat::test_local(".", load_package = "source", filter = "$(FILTER)")'

# covr runs the suite through tests/testthat.R rather than testthat::test_local(),
# so it does not set NOT_CRAN the way `make test` does. Without it every
# expect_snapshot() skips (they are `cran = FALSE` by default), which silently
# drops the selection error paths from the measurement. The browser tests stay
# skipped regardless: skip_without_source_tree() refuses covr's installed-build
# layout, which is exactly what it is there for.
COVR_ENV = NOT_CRAN=true

## coverage: per-file test coverage, printed. Installs a fresh build of this
## tree into a temp library (covr's mechanism) -- never a pre-existing copy.
## The browser tests cannot run under covr, so R/selection.R is understated;
## see the "Test coverage" section of CLAUDE.md before reading the number.
coverage:
	$(COVR_ENV) Rscript -e 'print(covr::package_coverage())'

## coverage-report: the same run, as an interactive HTML report (needs DT)
coverage-report:
	$(COVR_ENV) Rscript -e 'covr::report(covr::package_coverage())'

## document: regenerate NAMESPACE and man/ from roxygen comments
document:
	Rscript -e 'devtools::document()'

## readme: rebuild README.md from README.Rmd (needs pandoc; see RSTUDIO_PANDOC above)
readme:
	Rscript -e 'devtools::build_readme()'

## check: full R CMD check -- builds a tarball from this tree and tests that
check:
	Rscript -e 'devtools::check(document = FALSE)'

## install-dev: install this tree into the user library (NOT needed to run tests)
install-dev:
	Rscript -e 'devtools::install(upgrade = "never")'

## clean: remove an installed copy, so nothing can shadow the source tree
clean:
	Rscript -e 'try(remove.packages("reactableduckdb"), silent = TRUE)'
