# List available commands
default:
    just --list

# Open a shell with the pinned R version and dev-only packages (devbox.json)
shell:
    devbox shell

# Restore the renv library (packages declared in renv.lock)
setup:
    devbox run -- Rscript -e 'renv::restore(prompt = FALSE)'

# Run the test suite
test: setup
    devbox run -- Rscript -e 'testthat::test_local()'

# Regenerate documentation (roxygen2) and NAMESPACE
document: setup
    devbox run -- Rscript -e 'roxygen2::roxygenise()'

# Load the package into an interactive R session for development
load: setup
    devbox run -- R -e 'pkgload::load_all()'

# Install the package locally
install: setup
    devbox run -- R CMD INSTALL .

# Build the source package tarball
build: setup
    devbox run -- R CMD build .

# Run full R CMD check
check: setup
    devbox run -- bash -c 'R CMD build . && R CMD check --as-cran $(ls -t *.tar.gz | head -n1)'

# Build the pkgdown site
site: setup
    devbox run -- Rscript -e 'pkgdown::build_site()'

# Remove build artifacts
clean:
    rm -rf ..Rcheck ..Rcheck.Rcheck docs *.tar.gz *.Rcheck
