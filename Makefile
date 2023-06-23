site:
	Rscript -e "rmarkdown::render('README.Rmd', output_file = 'README.md')"
	Rscript -e "pkgdown::build_site()"
doc:
	Rscript -e "devtools::document()"
build:
	Rscript -e "devtools::build()"
