
.PHONY: install test check build document

all: build

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
