test:
	Rscript -e "devtools::test()"

check:
	Rscript -e "devtools::check()"
	Rscript -e "urlchecker::url_check()"

build_site:
	Rscript -e "pkgdown::build_site()"
	cp pkgdown/cheatsheet/survminer_cheatsheet.pdf docs/
	cp -r tools docs/tools
	rm -f docs/CLAUDE.html docs/CLAUDE.md
	rm -f docs/ISSUE_TEMPLATE.html docs/ISSUE_TEMPLATE.md