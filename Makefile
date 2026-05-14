PKG      := PolandTripPlanner
PKGDIR   := PolandTripPlanner
R        ?= R
RSCRIPT  ?= Rscript
DEPS     := Rcpp R6 httr jsonlite pkgload shiny leaflet

.PHONY: all setup deps install check demo shiny ui clean help

## one-shot bootstrap: deps -> install
all: deps install

help:
	@echo "Targets:"
	@echo "  make          - install deps and build package (default)"
	@echo "  make deps     - install required CRAN packages"
	@echo "  make install  - R CMD INSTALL $(PKGDIR)"
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
	$(R) CMD INSTALL $(PKGDIR)

check: deps
	$(R) CMD build $(PKGDIR)
	$(R) CMD check --no-manual $(PKG)_*.tar.gz

demo: install
	$(RSCRIPT) scripts/demo.R

shiny ui: install
	$(RSCRIPT) -e 'shiny::runApp("shiny", launch.browser = TRUE)'

clean:
	rm -rf $(PKGDIR)/src/*.o $(PKGDIR)/src/*.so $(PKG).Rcheck $(PKG)_*.tar.gz
