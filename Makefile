PKGNAME = `sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION`
PKGVERS = `sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION`

all: doc vignettes site install_deps build

doc:
	Rscript -e "devtools::document()"

vignettes: doc
	Rscript -e "devtools::build_vignettes()"

site: doc
	Rscript -e "devtools::build_readme()"
	Rscript -e "devtools::build_site()"

install_deps:
	Rscript \
	-e 'if (!requireNamespace("remotes")) install.packages("remotes")' \
	-e 'devtools::install_deps(dependencies = TRUE)'

build: doc install_deps
	Rscript -e "devtools::build()"

check: build
	Rscript -e "devtools::check()"

install: build
	Rscript -e "devtools::install()"

clean:
	@rm -rf $(PKGNAME)_$(PKGVERS).tar.gz $(PKGNAME).Rcheck
