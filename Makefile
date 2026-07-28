test:
	Rscript -e "devtools::test()"

check:
	Rscript -e "devtools::check()"
	Rscript -e "urlchecker::url_check()"

# Never call pkgdown::build_site() directly -- it renders every root *.md into
# a public page and bakes its full text into search.json. build-site.sh builds,
# purges any CLAUDE/ISSUE_TEMPLATE artifact from pages, sitemap, search index
# and llms.txt, then FAILS if anything survives.
build_site:
	.github/scripts/build-site.sh

# Scrub + verify an existing docs/ tree without rebuilding.
scrub_site:
	.github/scripts/build-site.sh --scrub-only

.PHONY: test check build_site scrub_site