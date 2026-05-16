# Common development commands for epiprocess.

set shell := ["bash", "-cu"]

# List available recipes.
default:
    @just --list

# Run the default data.table-backed test suite.
test-dt:
    Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_local(reporter="summary")'

# Run the test suite with unqualified as_epi_archive() calls switched to duckplyr.
test-duck:
    env EPIPROCESS_TEST_ARCHIVE_BACKEND=duck TESTTHAT_PARALLEL=false Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_local(reporter="summary")'

# Run both archive backend test passes.
test-backends: test-dt test-duck
