PKG      := tripPlanner
R        ?= R
RSCRIPT  ?= Rscript
DEPS     := Rcpp R6 httr jsonlite testthat pkgload shiny

.PHONY: all setup deps install test check demo shiny ui clean help

## one-shot bootstrap: deps -> install -> tests
all: deps install test

help:
	@echo "Targets:"
	@echo "  make          - install deps, build package, run tests (default)"
	@echo "  make deps     - install required CRAN packages"
	@echo "  make install  - R CMD INSTALL $(PKG)"
	@echo "  make test     - run the testthat suite"
	@echo "  make check    - R CMD check (full package check)"
	@echo "  make demo     - run a small end-to-end planning example"
	@echo "  make shiny    - launch the Shiny demo UI (alias: make ui)"
	@echo "  make clean    - remove build artefacts"

deps:
	@$(RSCRIPT) -e 'pkgs <- c($(shell echo $(DEPS) | sed "s/[^ ]*/\"&\"/g" | tr " " ",")); \
	  miss <- setdiff(pkgs, rownames(installed.packages())); \
	  if (length(miss)) install.packages(miss, repos="https://cloud.r-project.org") \
	  else cat("All dependencies already installed.\n")'

install: deps
	$(R) CMD INSTALL $(PKG)

test: install
	$(RSCRIPT) -e 'testthat::test_local("$(PKG)")'

check: deps
	$(R) CMD build $(PKG)
	$(R) CMD check --no-manual $(PKG)_*.tar.gz

demo: install
	$(RSCRIPT) scripts/demo.R

shiny ui: install
	$(RSCRIPT) -e 'shiny::runApp("shiny", launch.browser = TRUE)'

clean:
	rm -rf $(PKG)/src/*.o $(PKG)/src/*.so $(PKG).Rcheck $(PKG)_*.tar.gz
