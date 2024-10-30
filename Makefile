test:
	Rscript -e "devtools::test()"

check:
	Rscript -e "devtools::check()"

build_site:
	Rscript -e "pkgdown::build_site()"
	cp pkgdown/cheatsheet/survminer_cheatsheet.pdf docs/
	cp -r tools docs/tools