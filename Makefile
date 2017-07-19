
.PHONY: install test check build

all: build

install:
	R --vanilla --slave -e "devtools::install()"

test:
	R --vanilla --slave -e "devtools::test()"

check:
	R --vanilla --slave -e "devtools::check()"

build: 
	R --vanilla --slave -e "devtools::build()"

