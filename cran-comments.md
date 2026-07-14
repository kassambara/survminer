## Test environments
* local macOS (Darwin 24.6.0), R 4.5.x
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1),
  windows-latest (release), macOS-latest (release)
* ggplot2-devel job (deprecation-as-error)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:
* "checking for future file timestamps ... unable to verify current time" —
  this is an environment issue (the check host could not reach the remote
  time server) and is unrelated to the package.

## Downstream dependencies
survminer has 72 reverse dependencies on CRAN. This release is additive: new
functionality is provided through new functions and opt-in arguments, and the
default output of existing functions is unchanged. I have checked the reverse
dependencies I could install and found no problems introduced by this update.

## Update

This is a major update (0.5.2 -> 1.0.0; see NEWS.md), consolidating a large body
of new functionality while keeping existing defaults backward compatible. Key
additions:

* One-call publication presets: `ggsurvplot(preset = "publication" | ...)` plus
  the `theme_surv_classic/minimal/bold()` companions.
* Modern effect measures: restricted mean survival time (`ggrmst()`,
  `ggrmst_difference()`), milestone survival (`ggmilestone()`), and landmark
  analysis (`gglandmark()`), all computed internally (no new run-time
  dependency).
* Cox-model tools: subgroup and multi-model forest plots
  (`ggforest_subgroup()`, `ggforest_models()`), a non-proportional-hazards
  diagnostic panel (`ggcoxnph()`), and `ggadjustedcurves()` gains a hazard-ratio
  annotation and a number-at-risk table.
* A composable single-ggplot return (`ggsurvplot(output = "ggplot")`),
  parametric-over-KM overlays (`ggsurvparametric()`), a CDISC ADTTE helper
  (`surv_adtte()`), a batch weighted-log-rank wrapper (`weighted_logrank()`),
  reverse-KM median follow-up (`surv_median_followup()`), and `plotmath`
  support in `legend.labs`.
* Modernized competing-risks visualization (`ggcompetingrisks()`).
* New teaching vignettes and various bug fixes and deprecation cleanups.
