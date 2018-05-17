
.PHONY: install test check build document readme

all: document readme

install: document
	R --vanilla --slave -e "devtools::install()"

test: document
	R --vanilla --slave -e "devtools::test()"

check: document
	R --vanilla --slave -e "devtools::check()"

build: document
	R --vanilla --slave -e "devtools::build()"

document:
	R --vanilla --slave -e "devtools::document()"

readme:
	R --vanilla --slave -e "library(rmarkdown);render('vignettes/introduction_to_reclin.Rmd', md_document(variant='markdown_github'), output_file = 'README.md', output_dir = './')"
