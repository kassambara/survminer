## Test environments
* local macOS (Darwin 24.6.0), R 4.4.2
* GitHub Actions: ubuntu-latest (release), windows-latest (release), macOS-latest (release)

## R CMD check results
There were no ERRORs or WARNINGs.

## Downstream dependencies

I have also run R CMD check on downstream dependencies of survminer.
All packages that I could install passed.

## Update

This is an update to version 0.5.2 (see NEWS.md). Key changes:
- Removed survMisc dependency by implementing weighted log-rank tests internally
- Fixed GeomConfint incompatibility with ggplot2 4.0.x
- Fixed surv_fit() error with list of formulas
- Updated deprecated ggplot2 API usage (size -> linewidth, is.ggplot -> is_ggplot)
