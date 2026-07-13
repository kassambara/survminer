# survminer 0.5.2.999

## New features

- New `ggcoxnph()` draws a multi-panel diagnostic for the proportional-hazards
  assumption of a Cox model, for one covariate of interest: log-cumulative-hazard
  vs log-time by group, the scaled Schoenfeld residuals with a smoothed estimate
  of the time-varying coefficient (Grambsch and Therneau, 1994) and the
  Grambsch-Therneau test p-value, a hazard-ratio trend that shows the shape of a
  time-varying effect, and the restricted-mean-survival-time difference as a
  function of the follow-up horizon. Panels that do not apply to the chosen
  covariate are dropped with a message. `ggcoxnph(fit, "sex")`.

- `ggadjustedcurves()` gains `show.hr`: annotate the covariate-adjusted survival
  curves with the grouping variable's hazard ratio and 95% confidence interval
  taken from the Cox model (the effect adjusted for the model's other terms). A
  factor with more than two levels shows one line per non-reference level; a
  `strata()` term (which has no coefficient) is skipped with a warning. Works with
  the number-at-risk table too.

- New `weighted_logrank()` computes Fleming-Harrington `G(rho, gamma)` weighted
  log-rank tests for one or more `(rho, gamma)` pairs, returning a tidy table of
  `statistic`, `df` and `p.value`. `FH(0,0)` is the log-rank test, `FH(1,0)`
  emphasises early differences, `FH(0,1)` late differences. The same arbitrary
  weight is available via `surv_pvalue(method = "FH", rho =, gamma =)` and
  `ggsurvplot(log.rank.weights = "FH", rho =, gamma =)`, and `surv_pvalue()` now
  also returns the test `statistic` and `df`.

- `ggsurvplot()` gains a `preset` argument for one-call publication-ready figures.
  `preset = "publication"` bundles a full evidence panel (confidence bands, log-rank
  p-value, median-survival lines, a number-at-risk table, a colourblind-safe palette
  and a clean theme); `"minimal"`, `"classic"` and `"presentation"` offer airy,
  textbook and slide-friendly looks. A preset only sets arguments you did not pass
  yourself, so any explicit argument still wins, and `preset = "none"` (default)
  leaves every existing call unchanged. Three reusable companion themes --
  `theme_surv_classic()`, `theme_surv_minimal()` and `theme_surv_bold()`
  -- are also exported for use via `ggtheme`.

- New `ggforest_subgroup()` draws the subgroup forest plot used in clinical
  reporting: the hazard ratio of a treatment *within* each level of one or more
  subgroup variables, with the treatment-by-subgroup interaction test. Per-level
  hazard ratios come from refitting the Cox model on each subset; each subgroup
  variable gets one interaction p-value from a likelihood-ratio test (the test of
  effect modification, as opposed to the per-level estimates). Unstratifiable
  levels are dropped with a warning. Returns a themeable ggplot with an optional
  overall row, a "No. of patients (\%)" column, precision-weighted boxes and a
  "favours" annotation. Complements `ggforest()` (see #271 and #366).

- New `surv_median_followup()` returns the median follow-up time computed with the
  reverse Kaplan-Meier method (Schemper & Smith, 1996) -- the roles of the event and
  censoring indicators are swapped and the median of the resulting curve is reported,
  per group. This is the follow-up counterpart of `surv_median()` (median survival),
  a number routinely reported in clinical publications.
- New `ggrmst()` and `ggrmst_difference()` for restricted mean survival time (RMST) --
  the area under the Kaplan-Meier curve up to a truncation time `tau`, an absolute
  measure (in time units) that stays interpretable under non-proportional hazards.
  `ggrmst()` draws the Kaplan-Meier curves with the RMST region shaded: for two groups
  the area *between* the curves up to `tau` is highlighted (equal to the RMST difference
  when the curves do not cross) and the difference is annotated with its confidence
  interval and p-value;
  for three or more groups the area under each curve is shaded, per panel.
  `ggrmst_difference()` returns a tidy table of per-group RMST (with SE and CI) and the
  RMST difference. The estimate is computed internally from the Kaplan-Meier curve (no
  new run-time dependency; matches `survRM2::rmst2()`), and `tau` defaults to the
  largest time at which every group's curve is defined. See Royston & Parmar (2013).
- `ggsurvplot()` gains an `output` argument. The default `output = "classic"` returns
  the usual compound `ggsurvplot` object (unchanged). `output = "ggplot"` instead
  returns a single survival-curve `ggplot` -- one object that composes normally with
  `+ geom_*`, `ggsave()` and `facet_wrap()` (by strata; faceting an external covariate
  still needs `ggsurvplot_facet()`), so you can add your own layers (an RMST
  area, a cumulative-incidence overlay, a reference line) without extracting `$plot`
  or fighting the compound object's `+`. `pval`, `conf.int` and `surv.median.line`
  are kept on the curve. A risk / cumulative-events / cumulative-censor table or a
  separate censor barplot cannot live in a single ggplot, so those are dropped with a
  warning if requested (use the default and its `$table`).
- `ggadjustedcurves()` gains a `risk.table` argument (#286). When set, a
  number-at-risk table is drawn below the adjusted curves and the function returns a
  `ggsurvplot` object (printed with `print()`) instead of a plain `ggplot`. Because
  the adjusted curves are model-based expectations for covariate profiles with no
  literal number at risk, the table is the **Kaplan-Meier** number at risk *by the
  grouping* `variable` (the title says so), and it is computed on the rows the Cox
  model used -- its complete cases -- so the baseline count matches `fit$n` rather
  than over-counting rows dropped for missing covariates. A shared `xlim`/
  `break.time.by` keeps the curve and table time axes aligned. It is meaningful only
  when `variable` is a real subgroup; for `method = "marginal"`/`"conditional"` the
  curves are balanced while this count is the raw Kaplan-Meier number at risk. The
  default (`risk.table = FALSE`) is unchanged. The "Risk table under
  `ggadjustedcurves()`" vignette recipe is kept as the manual equivalent. Requested
  by @BenCorden and others.
- `ggsurvplot_facet()` gains two arguments contributed by @MikeWLloyd (#705):
  `risk.table` draws a number-at-risk table under the faceted curves, faceted with the
  same structure so each panel's table sits below its own curves, and `pval.in.label`
  appends the per-panel log-rank p-value to the strip label instead of drawing it inside
  the panel. With `risk.table = TRUE` the function returns an aligned `gtable` (printed
  with `print()`); the default (`risk.table = FALSE`) still returns the faceted ggplot
  unchanged. The table's y-axis shows the within-panel group taken from the structured
  survival summary (so a factor level containing `=`, `,` or `;` is labelled correctly,
  in row order matching the curves and in a single colour -- across facets the curve
  colours are not a reliable row identifier, so the text label carries the identity), and
  `pval.in.label` reuses the same p-value text as the on-panel label (so it also reflects
  `p.adjust.method`). This is the
  long-standing request to combine faceting with a risk table (#587, #330, #539; the
  single-variable case of #370 and #511). Both are for a single `facet.by` variable:
  `pval.in.label` with two
  variables warns and draws the p-value on the panels, and `risk.table` with two warns and
  returns the plot without a table (a two-way `facet_grid` shares one y axis per row, so
  the per-panel strata cannot be labelled correctly).
- New `customize_labels()` to set the font (size, style and color) of the main title,
  subtitle, caption, axis titles and axis tick labels across all components of a
  `ggsurvplot` object (survival curve, risk table and censor plot), or of an individual
  component. Labels are set with `ggtext::element_markdown()`, so the markdown-formatted
  risk-table strata labels are preserved (#520).
- `ggsave()` now works directly on a `ggsurvplot` object: a `grid.draw()` method for the `ggsurvplot` class is registered, so `ggsave(filename, plot = p)` saves the assembled plot (survival curve + risk table) without needing the `print(p, newpage = FALSE)` workaround, and no leading blank page is produced (#152). Same approach independently contributed by @mjakubczak (#522).

## Major changes

## Documentation

- New "Customization recipes" vignette collecting reusable recipes that solve common requests by composing on the objects survminer already returns (`ggsurvplot()`'s compound object and the `surv_summary()` data frame). Recipes so far: solid-to-dashed curves once the number at risk drops below a fraction of the initial cohort (#559); step-shaped legend keys that echo the Kaplan-Meier curve (#537); marking the median survival and its confidence interval (#345); plot math (expressions) in the legend (#350); one shared legend across a grid of survival plots (#340); a per-group risk table under Cox-adjusted survival curves (#231); a number-at-risk table under `ggadjustedcurves()` (#286); for a `cmprsk::cuminc()` competing-risks fit, plotting a single event of interest with a number-at-risk table (#223); a forest plot summarizing several univariate Cox models on one plot (#459, #310); smooth parametric `survreg()` / Weibull survival curves per covariate profile, overlaid on the Kaplan-Meier, via `surv.geom = geom_line` (#276); and a subgroup-analysis forest plot (the treatment effect within each subgroup, with the treatment-by-subgroup interaction p-value), which `ggforest()` cannot draw from a single model (#271, #366); and comparing several parametric distributions (`flexsurv::flexsurvreg()`) against the Kaplan-Meier, with AIC-based selection (#183).

## Minor changes

- `ggsurvplot()` now emits an informative message when `risk.table.y.text = TRUE` is explicitly set together with `risk.table.pos = "in"`, instead of silently ignoring it. An in-plot risk table is drawn over the survival panel's own y-axis, so its rows are coloured by strata (matching the curves) rather than labelled; the message explains this and points to `risk.table.pos = "out"` for text strata labels. It fires only when both arguments are explicitly passed, so default in-plot tables (which carry `risk.table.y.text = TRUE` by default) are unaffected. Reported by @Swechhya (#211).

- `surv_median()` no longer errors on a `survfit` stored without confidence limits (`conf.type = "none"`) or fitted at a non-default confidence level (e.g. `conf.int = 0.9`). It hard-coded the `0.95LCL`/`0.95UCL` columns of `summary()$table`; these are now detected by suffix, so a `0.9` fit returns its real limits and a no-CI fit returns the median with `NA` limits instead of failing. Default (0.95) output is unchanged. Found alongside #639 (#818).

- `ggsurvplot()` no longer errors with a cryptic `arguments imply differing number of rows` message on a `survfit` stored without confidence limits (e.g. fit with `conf.type = "none"`, or any fit that kept no CI). `surv_summary()` treated the absent `upper`/`lower` as length-0 columns; because it runs for every `ggsurvplot()`, such a fit could not be plotted at all -- even with `conf.int = FALSE`. The missing limits are now filled with `NA`, so the curve draws with no confidence band. Fits with confidence limits are unchanged. Reported by @pmpradhan (#639).

- `ggsurvtable()` (and `ggsurvplot()`'s risk table / cumulative event / cumulative censor tables) gains `obscure.less.than` for small-cell suppression, so small at-risk / event / censor counts can be hidden to avoid patient re-identification in sensitive data. When set to an integer, any count below it is shown as `"<n"` (e.g. `obscure.less.than = 5` renders `"<5"` for counts of 1--4); the combined `"nrisk_cumevents"` / `"nrisk_cumcensor"` cells mask each count independently, and comparison is numeric (so `138` is never masked at a threshold of 5). `obscure.zero` (default `FALSE`) controls whether a count of 0 is also hidden. Both default off, so existing tables are unchanged. Requested by @jwallib (#637).

- `ggforest()` now labels factor levels correctly when the model uses a non-default treatment contrast, e.g. `contrasts(x) <- contr.treatment(k, base = j)` to set a custom reference while keeping the original level order. `coxph()` then names such coefficients by contrast column (level index) -- `ph.ecog1`, `ph.ecog3`, `ph.ecog4` for `base = 2` on levels `0/1/2/3` -- whereas the factor branch matched them by `paste0(variable, level)`, so it tagged the wrong level(s) as the reference, placed the hazard ratios on the wrong rows, and silently dropped the last level's coefficient (a misleading plot, no error). Each fitted coefficient is now routed to the level it belongs to via the model's stored contrast matrix. This makes `contr.treatment(base = j)` the order-preserving way to pick a reference level in `ggforest()` (the original request). Default contrasts are byte-identical, and non-treatment contrasts (`contr.helmert`/`contr.sum`/`contr.poly`, for which a single per-level reference is not defined) fall back to the previous behaviour. Reported by @PeterStrom (#404).

- `ggcoxzph()` gains two optional, default-preserving arguments (#767, follow-up to #410, idea from @DanChaltiel): `add.beta.line` overlays a horizontal reference line at the model's estimated coefficient on each single-coefficient term's scaled-Schoenfeld panel (under proportional hazards the residuals scatter around beta; multi-level factor and spline terms have no single coefficient, so their panel is left without a line) -- it needs a `coxph` model as input and is styled with `beta.line.col`/`beta.line.type`/`beta.line.size`; and `var_pval` selects only the terms whose proportional-hazards test p-value is below a threshold, as an alternative to `var`. Both default off (`add.beta.line = FALSE`, `var_pval = NULL`), so existing calls are unchanged.

- `ggcompetingrisks()` now fails with a clear, actionable message instead of the opaque `Names repair functions can't return 'NA' values` error when given predicted curves from a multistate Cox model evaluated at several covariate profiles -- `survfit(coxph_multistate, newdata = <2+ rows>)`, whose `pstate` is a 3-D array with one column per profile-by-state combination, more than the number of states. The message points to supplying a single profile, using `cmprsk::cuminc()` / `survfit(Surv(., type = "mstate") ~ ...)`, or base `plot()` for the per-profile curves. The 2-D competing-risks path and a single-profile prediction (which already rendered) are unchanged. Reported by @adamssv (#625).

- `ggsurvplot()` on a `surv_summary()` data frame now supports `risk.table`, `cumevents` and `cumcensor`, building the number-at-risk / cumulative tables from the data frame (it already carries `n.risk`/`n.event`/`n.censor`) -- previously these were ignored and only the curve was drawn. The displayed tables match those from the corresponding survfit (for a weighted or left-truncated fit the internal `strata_size` may differ, but the shown numbers do not). A data-frame call without a table request still returns a bare ggplot (unchanged), and the survfit path is untouched. Requested by @wnilesanderson (#409).

- `ggsurvplot()` on a grouped fit from `surv_fit(..., group.by=)` now shows the correct per-subgroup log-rank p-value on each panel with `pval = TRUE`. Previously every panel displayed the same pooled p-value, because `ggsurvplot_list()` recomputed the test on the pooled `data=` instead of each subgroup. `surv_fit()` now carries the per-group data subsets, which `ggsurvplot_list()` uses; the curves/tables and same-data multi-fit lists are unchanged (#799).

- `ggsurvplot_combine()` now accepts `surv_summary()` data frames as list elements, not only survfit objects -- the data-frame analogue of `ggsurvplot_df()`. A data-frame element is drawn directly (and `data` is optional in that case). Survfit elements are unchanged. The number-at-risk/cumulative tables and median lines require a survfit and are refused with a clear message for a summary data frame. Requested by @HeidiSeibold (#323).

- `pairwise_survdiff()` gains a `ref.group` argument to compare every group against a single control/reference group only, instead of all pairwise comparisons. The p-values are then adjusted over just those k-1 comparisons (a smaller multiple-testing correction), which suits a many-treatments-vs-one-control design. The default `NULL` keeps the full pairwise comparison table unchanged. Requested by @th3minstr3l (#364).

- `ggsurvplot_facet()` gains a `p.adjust.label` argument to customise the prefix shown before an adjusted p-value (used with `p.adjust.method`). The default `"adj.p ="` is unchanged; set e.g. `p.adjust.label = "q ="` or `"p.adj ="` for publication styling (a trailing `"="` becomes `"<"` for very small p-values). Follow-up to #407.
- `ggsurvplot_facet()` gains a `p.adjust.method` argument to adjust the per-panel log-rank p-values for multiple comparisons across panels (passed to `stats::p.adjust()`, e.g. `"BH"`, `"bonferroni"`, `"holm"`), mirroring `pairwise_survdiff()`. The default `"none"` shows the raw per-panel p-values (unchanged); when a method is set the displayed text is prefixed with `"adj.p ="`. Panels with an undefined p-value (e.g. a single group) are left out of the adjustment. Requested by @choc2000 (#407).

- `ggsurvplot_facet()` now fails with a clear, actionable message instead of a cryptic `grDevices::col2rgb()` / "Unknown colour name" error at draw time when the fit's grouping term is not a plain data column -- e.g. a fit built from `Surv(...) ~ I(sex)`, `~ strata(sex)`, `~ cut(age, 3)`, or a formula assembled with `eval(as.name(...))` inside a loop. Such a term is not a column and cannot be used to colour the panels; the message points to using a plain variable / `reformulate()`. Fits whose grouping variable is a real column are unchanged. Reported by @Yatros (#380).

- `ggsurvplot(..., risk.table.pos = "in")` now keeps the number-at-risk figures aligned with the survival curve's x-axis when the visible range changes. The in-plot table was previously embedded as a single grob stretched across the panel with `annotation_custom()`, so with `xlim` or `axes.offset = FALSE` the numbers drifted off their axis ticks. Each number (and the table title) is now drawn as its own text placed at its real `(time, y)` position, so ggplot's own coordinate system keeps it locked to the tick under any `xlim`/`axes.offset`, and numbers beyond `xlim` are clipped like the "out" table. Because the labels are drawn in data coordinates without going through the y-scale, the survival curve, its coordinate range (x and y) and its out-of-bounds clipping are unchanged, and the default `risk.table.pos = "out"` is untouched. One consequence: the in-plot numbers/title are now drawn in a fixed style rather than through `tables.theme`, so `tables.theme` no longer restyles the inset (e.g. `tables.theme = theme_void()` no longer hides the in-plot title). Reported by @coenvandam, with a `coord_cartesian()` workaround from @gunthergl and a detailed reproducible example from @Doktertje (#302).

- New exported `build_ggsurvplot()` returns the assembled `gtable` grob of a `ggsurvplot` object (survival curve + risk table + cumulative events/censor tables) -- the same object `print.ggsurvplot()` draws -- for use with `grid::grid.draw()`, `gridExtra`, or `ggplot2::ggsave()` without going through the print method. It wraps the internal `.build_ggsurvplot()`, giving a documented, stable public entry point (#569).

- `pairwise_survdiff()` gains a `method` argument matching `surv_pvalue()`, so pairwise comparisons can use a weighted log-rank test (`"n"`/Gehan-Breslow, `"sqrtN"`/Tarone-Ware, `"S1"`/Peto-Peto, `"S2"`/modified Peto-Peto, `"FH_p=1_q=1"`/Fleming-Harrington), not only `survival::survdiff()` + `rho`. The method name/alias resolution is now shared between the two functions. Default `method = "survdiff"` reproduces the previous result exactly (`rho` still applies). Weighted methods do not support `strata()` terms and error clearly if combined. Addresses the inconsistency reported in #433.

- `cumevents` and `cumcensor` now accept a character value ("absolute", "percentage", "abs_pct"), like `risk.table`, to show the cumulative events/censoring table as a percentage of the stratum size (or "count (percent)"), not only the absolute count. `TRUE`/`FALSE` are unchanged (absolute). The default table title reflects the type. Requested by @anarpkpd (#499).

- `break.time.by` (and its alias `break.x.by`) now accepts a numeric vector of custom break positions, e.g. `break.time.by = c(0, 100, 300, 600, 1000)`, in addition to a single step. Passing a vector previously errored (`'by' must be of length 1`). The same breaks drive the survival curve and the number-at-risk/censor tables, so they stay aligned. A single value is unchanged. Requested in #435 and by @AjayKumar-O (#695).

- `ggforest()` gains a `ggtheme` argument to customize the plot's theme (e.g. `ggtheme = theme(text = element_text(size = 14))`). Because `ggforest()` returns a rasterised gtable, a theme added to the returned object was silently ignored; passing it via `ggtheme` applies it to the plot before rasterisation. Default `NULL` is unchanged. Reported by @manuSrep (#530).

- `ggforest()` gains a `var.labels` argument to relabel the variable names shown in the plot, e.g. `var.labels = c(age = "Age (years)", sex = "Sex")`. Only names matching a model term are relabelled; unmatched terms keep their original name. Default `NULL` is unchanged. Requested by @PeterStrom (#405).

- `ggsurvplot()` gains a `pval.parse` argument. Set `pval.parse = TRUE` to render the p-value text as a plotmath expression, allowing italic/superscript formatting such as `pval = "italic(P)==1.4~x~10^-6"`. Default `FALSE` draws the text literally (unchanged). Requested by @KBalazs1987 (#605) and @arossevold (#679).

- `ggcoxzph()` now accepts a `coxph` model directly (running `survival::cox.zph()` on it automatically), instead of only a pre-computed `cox.zph` object — `ggcoxzph(coxph_fit)` previously errored with "Can't handle an object of class coxph". Passing a `cox.zph` object is unchanged. Contributed by @DanChaltiel (#410).

- `ggforest()` gains a `ref.display` argument. Set `ref.display = FALSE` to omit the rows that have no hazard ratio — factor baselines and any non-estimable/aliased or `strata()` terms (the rows labelled "reference") — from both the plot and the table, keeping only the estimated levels; useful for compact panels. Default is `TRUE` (every row shown, unchanged). Contributed by @trentleslie (#563).

- `pairwise_survdiff()` now uses `anyNA()` for its internal missing-grouping-value check instead of a row-wise `NA %in% .row`, which is faster and clearer. Results are unchanged for the usual factor/character grouping variables; the only difference is a purely numeric grouping column containing `NaN`, whose row is now dropped as missing (previously it formed a spurious `"NaN"` group). Contributed by @MichaelChirico (#635).

- `ggsurvplot_facet()` gains a `labeller` argument (forwarded to `ggplot2::facet_wrap()`/`facet_grid()`) to control how the panel strip labels are formatted — e.g. `labeller = ggplot2::label_both` or `labeller = ggplot2::as_labeller(c("1" = "Obs", "2" = "Lev", "3" = "Lev+5FU"))`. Default `NULL` keeps the current labels (unchanged); when supplied it takes precedence over `short.panel.labs` and composes with `panel.labs`. Idea contributed by @B0ydT (#667, #668).

- `surv_adjustedcurves()` gains a `fun` argument (matching `ggadjustedcurves()`) to transform the returned survival column — e.g. `fun = "event"` for cumulative events, `"cumhaz"`, or `"pct"`. Default `NULL` leaves the survival probabilities unchanged (#630).

- `arrange_ggsurvplots()` gains a `layout_matrix` argument (forwarded to `gridExtra::marrangeGrob()`) to control the position/order of the plots — e.g. `layout_matrix = matrix(seq_along(splots), nrow = nrow, byrow = TRUE)` orders them by row. Default is unchanged (plots fill column-wise) (#300).

- `ggforest()` gains a `global.stats` argument. Set `global.stats = FALSE` to omit the bottom caption reporting the global statistics (number of events, global log-rank p-value, AIC, concordance index) — useful when arranging several forest plots in a panel. Default is `TRUE` (unchanged) (#392).

- `surv_pvalue()` gains a `pval.digits` argument controlling the number of significant digits used to format the p-value in `pval.txt` (default `2`, unchanged; e.g. `pval.digits = 3` gives `"p = 0.00131"`). Requested for journals that report p-values to 3 digits (#343).

- `ggsurvtable()` gains an `hjust` argument to control the horizontal justification of the table text (passed to `geom_text`). Default is `0.5` (centered, unchanged); use e.g. `hjust = 0` for left-aligned counts (#629).

- `ggsurvplot()` gains a `linejoin` argument controlling the line join of the survival curve. Default is `"round"` (unchanged); use `linejoin = "mitre"` for sharp corners that mark event times precisely (requires a `ggplot2` version that passes `linejoin` through `geom_step()`). Previously `linejoin` passed via `...` was silently dropped (#653).

## Bug fixes

- The log-scale time axes (`ggsurvplot(fun = "cloglog")`, log-x risk/event tables
  and the number-at-risk censor plot) no longer use the ggplot2 `trans` scale
  argument that ggplot2 3.5.0 renamed to `transform`; the axis is unchanged.

- `ggsurvplot()` and `ggflexsurvplot()` no longer emit a ggplot2 deprecation warning when you set the curve line width with `size`. The width was already applied through `linewidth`, but `size` was also passed on to the line geom, where it is a deprecated aesthetic (as of ggplot2 3.4.0). It is no longer forwarded; the drawn line width is unchanged.

- `ggflexsurvplot()` now honors a user-supplied `summary.flexsurv` and the `fun` argument for a single-stratum fit. The single-stratum branch of the internal summary helper recomputed `summary(fit)` with the default time grid and type, so a `summary.flexsurv` meant to extend the fitted curve to a wider `xlim` was ignored (the curve stopped at the last observed time), and `fun = "cumhaz"` fell back to the survival curve. It now uses the already-selected summary. Multi-stratum fits and the default single-stratum survival curve are unchanged (#400).

- `ggsurvplot()` now accepts a per-strata `linetype` vector that contains a hex dash pattern, e.g. `linetype = c("solid", "F1")`. Only all-base-name or all-numeric vectors were mapped to a manual per-strata scale; a vector with a hex pattern fell through and crashed with "the condition has length > 1". Any length-1 value (including a single hex pattern or `"strata"`) is unchanged (#344).

- `ggforest()` no longer draws spurious "reference" rows for an interaction-only model. With a term such as `Surv(time, status) ~ sex:ph.ecog` (an interaction without the corresponding `sex`/`ph.ecog` main effects), the main-effect factor levels were still listed as reference rows, implying main effects that were never fit. A variable that appears only inside an interaction (absent from `names(model$assign)`) is now skipped; its coefficients are still drawn as interaction rows. Models with genuine main effects (including splines/transformations) are unchanged. Reported by @anaveda (#594).

- `pairwise_survdiff()` now recognises a `survival::strata()` term in the formula, not only bare `strata()`. `pairwise_survdiff()` parses the formula's term labels itself before rebuilding the model call, and its strata detector keyed on `^strata(`; a `survival::`-qualified stratification term therefore failed to match and was mistaken for a grouping variable, erroring with "undefined columns selected" (this was survminer-side string parsing, independent of the survival version). The `survival::` prefix is now stripped so bare `strata()` is passed to `survival::survdiff()`. Grouping and p-values for every existing formula are unchanged. The separate internal use of `survival::strata()` to combine several grouping columns is a direct call on a data frame (not a formula term), so it is unaffected by how formulas resolve specials; it partitions the data the same way as `interaction()`, and the pairwise p-values match `survival::survdiff()` exactly. Flagged by Terry Therneau (#672).

- Fix the risk table's `strata_size` and `pct.risk` for id-based counting-process fits (`survfit(Surv(start, stop, event) ~ g, id = subject)`, with time-varying covariates / multiple rows per subject). `fit$n` counts intervals (rows), not subjects, so the stratum size was overstated and the at-risk percentage did not start at 100%. `strata_size` now uses `fit$n.id` (the per-stratum unique-subject count) when the fit provides it; ordinary right-censored, weighted and plain left-truncated fits (which have no `fit$n.id`) are unchanged. The number-at-risk counts themselves are computed by `survival` and were already correct on current versions. Reported by @wiedenhoeft (#592).

- Fix `ggforest()` silently dropping interaction terms. Only main-effect variables were iterated (`attr(model$terms, "dataClasses")`), so a model with an interaction (e.g. `Surv(time, status) ~ sex * ph.ecog`) omitted the interaction coefficients (`sexfemale:ph.ecog1`, ...) from both the plot and the table. Each interaction coefficient is now drawn as its own row, mapped to the fitted coefficients via `model$assign`. Models without interactions are unchanged. Reported by @Generalized (#536).

- `ggsurvplot()` now gives an actionable message instead of the cryptic "object of type 'symbol' is not subsettable" when a `survfit` was built from a formula stored in a variable in a non-global scope (e.g. `survfit(form, data)` inside a function, `lapply()`/`nest_by()`, or a `{targets}` pipeline), whose formula object is out of scope at plot time. The message points to `surv_fit()`, which retains the formula and data. Fits whose formula is recoverable are unchanged. Reported by @skent259 (#533).

- `surv_fit()` now accepts data-column arguments passed by a bare name, matching `survfit()`. `surv_fit(f, data = d, weights = w)` and `surv_fit(f, data = d, id = subject)` previously errored with "object '<col>' not found", because `surv_fit()` evaluated `...` in the calling frame instead of inside `data`. When the standard call fails for this reason, `surv_fit()` now retries with the survfit data-column arguments (`weights`, `subset`, `id`, `istate`, `etype`) evaluated inside `data`. The standard path is unchanged, so every call that already worked is unaffected. Reported by @raffaelemancuso (#644) and @wiedenhoeft (#571).

- `ggadjustedcurves()` now relabels the y-axis to match `fun` (e.g. "Cumulative hazard" for `fun = "cumhaz"`, "Cumulative event" for `fun = "event"`), like `ggsurvplot()` already does. Previously the axis kept the default "Survival rate" label even when the curve was transformed, so a cumulative-hazard curve was mislabelled. Only the default label is changed: `fun = NULL` (the default) and a user-supplied `ylab` other than the default are unchanged. Reported by @dangvantri (#555).

- Fix overlapping y-axis numbers in the `ncensor.plot` ("Number of censoring") panel. The panel drew one y-axis break per distinct censoring count, so a dataset with many distinct counts crowded the short panel and the integer labels overlapped. It now keeps one break per count when there are few (<= 5) distinct counts (unchanged), and falls back to ~5 evenly spaced integer breaks when there are more. Reported by @ZihaoXingUP (#542).

- Fix `ggsurvplot(fit, facet.by = ...)` silently dropping the fit's case `weights` when the `facet.by` variables are not all in the survfit formula. In that case the curves are refit to add the faceting variables, and the refit ignored `weights`, so the faceted curves and the `surv.median.line` were computed **unweighted** — silently wrong. The recovered weight vector is now forwarded to the refit, so weighted faceting draws the correct (weighted) curves and medians. Unweighted fits are unchanged. Two caveats, each now made explicit with a warning: the per-panel p-value uses `survival::survdiff()`, which has no case-weights argument, so it remains unweighted; and only weights supplied as a bare column of `data` (`weights = w`) are recovered — a column is guaranteed row-aligned to the refit data; weights given any other way (e.g. `weights = df$w`) fall back to unweighted (with a warning) rather than risk a silent misweighting. Reported by @goncosta (#556).

- `ggsurvplot()` / `surv_summary()` now fail early with a clear, actionable message when given a multi-state / competing-risks fit (a `survfitms` object) instead of crashing with a cryptic "arguments imply differing number of rows". This happens when the status passed to `survival::Surv()` is a factor or has more than two levels, which makes `survfit()` return a multi-state model. The message directs users to a numeric 0/1 (or logical) status for a Kaplan-Meier curve, or to `ggcompetingrisks()` for competing risks / multi-state. Ordinary `survfit` inputs are unchanged (the negative-time trigger of the same error was already fixed) (#373).

- Fix the number-at-risk table's first (t = 0) column not lining up with the survival curve's start when `axes.offset = FALSE`. The table panel starts exactly at `x = 0` like the curve, but the t = 0 numbers were nudged right to `max(xlim)/30`, so they sat to the right of the curve's origin. They are now kept at `x = 0` and only the t = 0 column is left-aligned (a centred number at `x = 0` would be clipped by the panel edge); interior columns are unchanged. The default `axes.offset = TRUE` output, the log-axis path (which keeps the historical relocation, since `x = 0` can't be shown on a log scale), and the inset `risk.table.pos = "in"` layout are all unchanged. `cumevents`/`cumcensor` tables get the same t = 0 alignment. Contributed by @nolankm (#645, #448). (The separate `risk.table.pos = "in"` inset misalignment in #302 is not addressed here and remains open.)

- Improve the error message when `ggsurvplot(fit)` is called without `data =` for a model fitted in a non-global environment (e.g. inside a function or a `{targets}` pipeline). survminer re-derives the data from the fit's stored call, but the referenced object is then out of scope and a survfit stores no reference to its creation environment, so the data cannot be recovered. Instead of a cryptic `object '<name>' not found`, survminer now raises an actionable message telling the user to pass `data =` explicitly (e.g. `ggsurvplot(fit, data = mydata)`), while preserving the original error as context. Working inputs (data supplied, or a fit created in the global scope) are unchanged (#521).

- Fix the "large dash" size not being changeable when `tables.y.text = FALSE`: `p$table <- p$table + theme(axis.text.y = ggtext::element_markdown(size = ...))` was silently reset to the default (50) because the dash styling is re-applied when the plot is printed. The re-application now keeps a user-set size; the default is unchanged (#642).

- Fix `ggsurvplot(..., facet.by = , pval = TRUE)` erroring with "variable lengths differ" when the model was built from a `Surv` object created *outside* the data and used as the formula left-hand side (e.g. `Survival <- Surv(time, status); survfit(Survival ~ x, data = D)`, rather than `Surv(time, status) ~ x`). The per-panel p-value refit row-subset the data while the global response kept its full length; the response is now materialised once on the full data so it stays aligned under subsetting. In-formula fits are unchanged (#467).

- Improve `ggforest()` handling of a non-converged Cox model (complete / quasi-complete separation), whose coefficient has a hazard ratio or confidence limit that overflows to `Inf` (or underflows to 0). Such a row can't be placed on the log axis; it previously produced a misleading full-width interval and a cryptic "log-10 transformation introduced infinite values" warning. `ggforest()` now emits a clear message naming the affected term(s), omits those rows from the drawn point/interval (they still appear in the table with their numeric labels), and sizes the axis from the finite rows. Converged models are unchanged (#406). The infinite/near-zero CI case was also reported by @rschauner (#582).

- Fix customizing the risk table failing under `ggplot2 >= 4.0` with "Can't merge the `axis.text.y` theme element" (e.g. `p$table + theme(axis.text.y = element_text(...))`, or `ggsave()`ing a customized table). In ggplot2 4.x theme elements are S7 objects and `ggtext::element_markdown()` (used to colour the risk-table y labels per strata) can no longer be merged with a user `element_text()`. On ggplot2 >= 4.0 the per-strata label colours are now applied with `element_text(colour = <vector>)` (native and mergeable); on older ggplot2 `element_markdown()` is kept (it merges fine there). The coloured labels are unchanged (#557).

- Fix a factor **level** with a genuine trailing or leading space (e.g. `"Obs, "`) being parsed as `NA` in the grouping/facet columns. `survfit` right-pads level names in strata labels, so survminer must trim them; the trimmed value is now matched against the (also trimmed) factor levels while keeping the original levels, so such labels are recovered instead of dropped. Completes the strata special-character fixes (#616).

- Fix `ggsurvplot(..., risk.table = TRUE)` erroring with "gridtext has encountered a tag that isn't supported yet: &lt;blockquote&gt;" when a strata label / `legend.labs` entry contains `<`, `>` or `&` (e.g. `"> 1 Risk factor"`). The risk-table y-axis labels are drawn with `ggtext::element_markdown()` (to colour them per strata), which parsed those characters as HTML tags; they are now HTML-escaped so they render literally. Ordinary labels are unchanged, and the plain-text path (`risk.table.y.text.col = FALSE`) is untouched (#532).

- Fix strata parsing when a factor **level** contains special characters (`=`, `>`, `$`, etc.). survminer previously split each `survfit` stratum name on every `=`/`,` and mis-detected `$`, so such levels broke faceting, colouring, median lines, and legends. The parser now scopes the split to the known formula variable names (obtained from the fit), so levels may safely contain these characters. This fixes: `ggsurvplot(..., facet.by=)` erroring with "Unknown colour name" when a level has `=`/`>=` (#291, #430); a `>=`/`<` level used as `facet.by` erroring with "missing faceting variable" (#616); missing `surv.median.line` when a level has `=` (#599); and, for a level containing `$`, mis-parsed legend labels with the other variable's groups silently reordered (#680). Ordinary data (no special characters) is unchanged.

- Fix `ggcompetingrisks()` (multi-state / `survfitms` competing-risks fits) sorting its facet panels alphabetically by strata name instead of following the model's strata order. The internal `strata` column is now a factor with levels in model order, so panels keep `names(fit$strata)` order. Fits whose strata order already is alphabetical are unchanged. Contributed by @muschellij2 (#470).

- Fix `ggforest()` mislabeling the global p-value in the bottom caption as "Log-Rank": the value shown (`broom::glance()$p.value.log`) is the p-value of the *likelihood ratio test* (`survival` stores it in `logtest`), not the score/log-rank test (`p.value.sc`/`sctest`). The caption now reads "Global p-value (Likelihood ratio test)". The value itself is unchanged — only the label is corrected (#640).

- Fix `ggsurvplot()` mis-rendering negative survival times: the x-axis was clipped at 0 (hiding the negative-time region) and a spurious vertical drop was drawn at `x = 0`. The plot now behaves like base `plot.survfit()` — the panel spans the true time range and each curve starts at survival = 1 from its first observed time, with no injected `(0, 1)` point. For all non-negative data (the common case) the output is byte-identical (the x-origin still resolves to exactly 0). The `#523` warning that negative survival times are likely invalid is retained (#389).

- `ggadjustedcurves()`/`surv_adjustedcurves()` now warn when the default `method = "conditional"` is used with a grouping `variable` that is not in the Cox model: in that case every group's curve is identical (a single visible line), which silently confused users. The warning points to `method = "average"`/`"marginal"` (which are designed for a variable absent from the model). The computed curves are unchanged (message only), and no warning fires for the other methods or when the variable is in the model (#623).

- Fix `ggforest()` clipping the bottom global-statistics caption (number of events, global p-value, AIC, concordance index) in short plots (small figure heights): too little space was reserved below the last row. Space is now reserved so the caption always renders; it is only added when the caption is drawn, so `global.stats = FALSE` is unchanged (#696).

- Fix `ggforest()` reporting a sample size that includes subjects with missing values: `coxph()` drops rows with a missing value in any model variable (`na.action = na.omit`), but `ggforest()` counted all rows of `data`, overstating the per-term/level `N`. The reported `N` now reflects the complete cases the model actually used (`model$n`). Models fit on data without missing values are unaffected (#597).

- Fix `ggsurvplot(fit, facet.by = "X")` erroring with "subscript out of bounds" when every variable of the survival formula is also a `facet.by` variable, e.g. a null model `Surv(...) ~ 1` faceted by `X`, or `Surv(...) ~ X` faceted by `X`. Each panel then shows a single curve with no within-panel grouping, so there is no extra strata to build; this case no longer calls the strata builder with zero variables (#304).

- Fix the percentage at risk (`pct.risk`, used in `risk.table = "percentage"`/`"abs_pct"`) exceeding 100% for a weighted `survfit()`: it was computed as `n.risk * 100 / fit$n`, but `fit$n` is the unweighted subject count while `n.risk` is weighted. When the weighting makes `n.risk` exceed `fit$n`, the denominator now falls back to the weighted number at risk at the origin. Unweighted fits (for which `n.risk` never exceeds `fit$n`) keep `fit$n`, so their output is unchanged (#561).

- Fix `break.y.by` producing wrong axis breaks for transformed survival curves (`fun = "cloglog"`, `"event"`, `"cumhaz"`) or a custom `ylim` outside [0, 1]: the y breaks were computed as `seq(0, 1, by = break.y.by)`, so values outside [0, 1] had no breaks. They are now derived from the displayed y-range (the default survival plot is unchanged) (#378, #442).

- Fix `ggcoxzph()` with `cox.zph(..., transform = "log")` drawing the fitted line on a different x-scale than the residual points (the line was squeezed to the far left): the fit line was drawn at `log(pred.x)` while the points and confidence bands were on the original time scale. The fit line now uses the same scale and the x-axis is log-scaled, matching `survival::plot.cox.zph(log = "x")` (#454, #588).

- Fix `pairwise_survdiff()` erroring with "undefined columns selected" when the formula contains a `strata()` term (e.g. `~ rx + strata(sex)`): `strata(sex)` is not a data column, so it could not be used to group/subset. `strata()` terms are now separated from the grouping variable and kept in the `survdiff` formula, giving a stratified pairwise test (#648).

- Fix `ggsurvplot(..., add.all = TRUE, pval = TRUE, pval.method = TRUE)` drawing the p-value method (test name) as an empty string: the p-value is computed on the original fit and forwarded as text, so `ggsurvplot_core()` re-derived the method from the "all"-augmented fit and got `""`. The method is now drawn by `ggsurvplot_add_all()` (#673).

- Fix `ggsurvplot_facet(..., pval = "<string>")` erroring with "argument is not interpretable as logical", and clarify the documentation: `ggsurvplot_facet()` computes a p-value for each panel, so (unlike `ggsurvplot()`) a numeric or character `pval` cannot be substituted. Such a value is now ignored with a warning instead of crashing (#636).

- Fix `ggsurvplot_facet(..., panel.labs = ...)` failing with "cannot xtfrm data frames" when the data is a tibble: the panel-label code did `as.factor(data[, var])`, and `tibble[, var]` returns a one-column tibble rather than a vector. The data is now coerced to a plain data.frame at entry (#591).

- Fix `ggadjustedcurves(..., fun = ...)` not transforming the curve: `fun` (e.g. `"event"`, `"cumhaz"`, `"pct"`) only changed the y-axis limits, while the plotted curve stayed the raw survival probability. The transformation is now applied to the curve (a no-op when `fun = NULL`, the default) (#287, #498, #660).

- Fix `ggforest()` crashing (`axisTicks(): '_LARGE_ range'`) for a Cox model with complete/quasi-complete separation, where a coefficient is near-infinite: the x-axis range is now clamped to a finite window (with a warning) so the plot still renders (#570, #590).

- Fix `ggforest()` producing duplicated/crossed rows for prefix-colliding non-factor term names (e.g. logical covariates `add11` and `add17`): coefficients were matched to terms with a regex (`"^var*."`) that treated trailing digits as a quantifier, so each name matched the other's coefficient. Coefficients are now mapped to their term via `model$assign` (#689).

- Fix `ggforest()` reference level inheriting the statistics of a similarly-named non-reference level (e.g. a reference level `Bar` showing the hazard ratio of `Barb`): term rows were matched by character row indexing, which partial-matches. They are now matched exactly, so the reference level is correctly shown as the reference (#312).

- Fix `ggforest()` erroring with "undefined columns selected" when the Cox formula contains an in-formula factor transformation such as `as.factor(x)`: the term is now evaluated rather than looked up as a column name (a plain column name is unaffected) (#240).

- Fix the cumulative number-of-events and number-censored columns showing decimals for a weighted `survfit()` (in the risk/cumevents/cumcensor tables): the cumulative sums of the fractional weighted counts were not rounded, unlike the per-interval columns. They are now rounded to the same precision (#560, #554).

- Fix `ggsurvplot_combine()` ignoring the risk-table type: `risk.table = "nrisk_cumcensor"` (and other types) is now honoured instead of always showing the absolute number at risk (#641).

- Fix `ggsurvplot_combine()` ignoring `risk.table.fontsize`: the risk-table text size can now be set with `risk.table.fontsize` (as in `ggsurvplot()`), not only with `fontsize` (#514).

- Fix `surv_categorize()` returning the raw numeric values (instead of `"high"`/`"low"`) for variables whose names contain characters that `make.names()` alters, such as a hyphen (e.g. gene names like `"A1BG-AS1"`): `summary.surv_cutpoint()` now builds its row names with `check.names = FALSE` so the name still matches (#609). The hyphenated-gene-name case was also reported by @hmkim (#502).

- Remove an unused, undefined `alpha` argument passed by `surv_cutpoint()` to `maxstat::maxstat.test()` (it only worked by lazy evaluation); no change to computed cut points (#598).

- Fix `ggsurvplot()` clipping events that occur after the last x-axis break: the default upper x-limit was the largest "nice" axis break, which can fall below the largest event/censoring time, so those events were invisible unless `xlim` was set manually. The upper x-limit now extends to cover the maximum time; the axis tick breaks (and risk-table columns) are unchanged, and a user-supplied `xlim` is still honoured (#655).

- `ggsurvplot()` (and the related builders such as `ggsurvplot_combine()`) now warn when the survival data contain negative times, which make the Kaplan-Meier curve appear to increase (not meaningful for a survival estimate). Previously such times were plotted silently. The plot itself is unchanged (#523).

- Fix `ggcoxfunctional()` clipping most of its own points: the default y-axis limits were set to the range of the lowess smoother, which is much narrower than the martingale residuals plotted as points, so points with large residuals fell outside the visible panel. The y-axis now auto-scales to include all points by default; a user-supplied `ylim` is still honoured (#465).

- Fix `ggcoxfunctional()` erroring with "'x' and 'y' lengths differ" when the Cox formula contains a covariate with missing values: `model.matrix()` drops rows with missing terms, but the null-model martingale residuals were computed from the full data. The data is now restricted to the complete-case (model-matrix) rows so the lengths match (#248).

- Fix `ggcoxfunctional()` erroring with a cryptic "'x' and 'y' lengths differ" when the Cox formula contains a factor/character covariate (or a `strata()` term): such terms are renamed/expanded in the model matrix and cannot be checked for functional form. They are now dropped with a warning, and only continuous covariates are plotted (#357). Dropping non-continuous terms was also proposed by @DanChaltiel (#410).

- Fix `ggsurvplot_combine(..., surv.median.line = "hv")` (or `"h"`/`"v"`) not drawing the median survival lines: `surv.median.line` was forwarded to `ggsurvplot_df()` (which does not handle it) and silently ignored. The median lines are now computed from the combined fits and drawn on the plot (#316).

- Fix `ggsurvplot_facet(..., pval = TRUE, pval.size = <n>)` ignoring `pval.size`: the per-facet p-value (and `pval.method`) text was drawn with ggplot2's default size and `pval.size` was silently routed into `...`. `pval.size` is now an explicit argument applied to the text; its default is `5`, consistent with `ggsurvplot()` (previously the faceted p-value text rendered at ggplot2's default size ~3.88) (#338).

- Fix `ggcompetingrisks(..., multiple_panels = FALSE, conf.int = TRUE)` drawing incorrect confidence bands that jumped between groups of the same event: the ribbon was grouped only by `event`; it is now grouped by `interaction(event, group)` (#490).

- Fix `ggsurvplot(..., add.all = TRUE)` (and `ggsurvplot_add_all()`) erroring with "argument matches multiple formal arguments" when a `legend` position was supplied: `legend` partial-matched `legend.title`/`legend.labs`; it is now an explicit forwarded argument (#566).

- Fix `ggflexsurvplot()` collapsing a grouped Kaplan-Meier curve to a single "All" stratum when the grouping covariate is a factor: `is_factor_or_character()` called ggplot2's `is.facet()` (a Facet-object test, always `FALSE` for a data column) instead of `is.factor()` (#408).
- Fix `surv_group_by()` (and downstream `ggsurvplot_facet()` / grouped `surv_pvalue()`) failing with "cannot xtfrm data frame" when the input is a tibble: extract the grouping column with `data[[var]]` (a vector) instead of `data[, var]` (a one-column tibble) (#548, #670). The same one-line fix was earlier contributed by @yonicd (#549).
- Fix `ggflexsurvplot()` erroring with "object '<name>' not found" when the model's data object is out of scope at plot time (even with `data =` supplied): the already-resolved `data` is now forwarded to the internal `.extract.survfit()` instead of being re-derived from `fit$call$data` (#436).
- Fix `ggsurvplot(..., ncensor.plot = TRUE)` erroring at draw time with "Unknown colour name: strata" for a single-group fit (`~ 1`): the default `color = "strata"` was passed to the censoring bar plot as a literal colour when the data has no `strata` column. The bars now use the survival curve's own colour for that case only (falling back to black if it cannot be resolved), leaving grouped fits and explicit colours unchanged (#298).
- Fix `ggadjustedcurves()` / `surv_adjustedcurves()` failing with "cannot xtfrm data frame" when a tibble is passed as `data` (or as `reference` for the marginal method): both are coerced to a plain data.frame so the internal `data[, variable]` extractions return a vector (#501, #628).
- Fix `ggsurvplot()` erroring with "'names' attribute [n] must be the same length as the vector [m]" when the `palette` contains a duplicated colour (or a default palette rendered a repeated hue) together with `risk.table` / `ncensor.plot`: the internal colour extractor used `unique()`, collapsing duplicate colours; it now returns one colour per group (#397, #519, #595, #691).
- Fix `ggsurvplot()` and `surv_pvalue()` erroring with "object of type 'symbol' is not subsettable" for a `survfit` built from a formula stored in a variable (e.g. `frm <- Surv(time, status) ~ sex; survfit(frm, data)`): the stored formula (a symbol) is now resolved with `stats::formula(fit)` instead of `as.formula(fit$call$formula)` (#324, #341, #602).

# survminer 0.5.2

## New features

## Major changes

- Remove `survMisc` dependency: weighted log-rank tests (Gehan-Breslow, Tarone-Ware, Peto-Peto, modified Peto-Peto, Fleming-Harrington, test-for-trend) are now computed internally using base R, avoiding the at-risk `survMisc` package. All existing `method` arguments in `surv_pvalue()` and `log.rank.weights` in `ggsurvplot()` continue to work identically.

## Minor changes

- Update ggplot2 API usage: replace deprecated `size` with `linewidth` for line geoms, and `is.ggplot()` with `is_ggplot()` (#692, #693)

## Bug fixes

- Fix `GeomConfint` incompatibility with ggplot2 4.0.x: normalize `linewidth` and `linetype` after stairstep transformation to prevent "Aesthetics can not vary along a ribbon" error (#694)
- Fix `surv_fit()` error when using list of formulas with list of data sets (`match.fd = FALSE`): replace defunct `dplyr::combine()` with `unlist(recursive = FALSE)` (#697, #699)

# survminer 0.5.1

## Bug fixes

- Fix chart/risk table misalignment with ggplot2 >= 3.5.0 by using dynamic panel detection instead of hardcoded grob indices (#649, #675)
- Fix R CMD check global variable binding warnings in `.add_surv_median()`
- Fix `ggcoxdiagnostics()` x-axis scaling when using `ox.scale = "time"` with Schoenfeld residuals (#608)
- Fix ggplot2 3.5.0 aesthetic length warning when using `surv.median.line = "hv"` or `"h"` with multiple survival curves (#643)
- Fix compatibility with ggplot2 development version (#681): Remove manual class assignment in `theme_survminer()` to ensure proper theme object construction
- Fix test suite compatibility with ggplot2 development version (#681): Update layer access syntax in tests to support both stable (`$layers`) and development (`@layers`) versions
- Fix "Ignoring unknown labels" warnings by conditionally setting legend titles only for aesthetics that are actually used in plots
- Update documentation to recommend `%++%` operator instead of `+` for adding themes to ggsurv objects with ggplot2 v >= 3.5.2
- Fix documentation examples to consistently use `%++%` operator in `theme_survminer()` help

# Survminer 0.5.0

## Minor changes

- R-ADDICT website is no longer live, so updating links in README (#622)
- ggplot2 minimum version is now 3.4.0
- Fixing tidyverse deprecated functions to minimize warnings (@tbaer-c7ks7s #579, and #665):
  - dplyr::select_() -> dplyr::select()
  - tidyr::gather_() -> tidyr::pivot_longer()
  - ggplot2::aes_string() -> ggplot2::aes()

## Bug fixes

- Fixing Rd cross-references issue in the documentation (@HenningLorenzen-ext-bayer, #663). RD `\link{}` targets missing package added in the following Rd files:
  -  ggadjustedcurves.Rd: coxph.object, coxph
  -  ggcompetingrisks.Rd: survfit
  -  ggcoxdiagnostics.Rd: coxph, coxph.object, residuals.coxph, geom_hline, geom_smooth
  -  ggcoxfunctional.Rd: coxph, Surv, coxph.object, arrangeGrob, grid.arrange
  -  ggcoxzph.Rd: plot.cox.zph, cox.zph, arrangeGrob, grid.arrange
  -  ggsurvevents.Rd: Surv, survfit
  -  ggsurvplot.Rd: grid.arrange
  -  surv_cutpoint.Rd: theme_classic, grid.arrange
- Fixing issue in the scaling factor for sd in `ggcoxzph()` (#534 and #535)
   
# Survminer 0.4.9

## Minor changes

- A new vignette added to show how to display interaction using ggforest() (#496).
- Since ggplot2 v3.3.0, the function `element_text()` issues a warning when vectorized arguments are provided, as in colour = c("red", "green", "blue"). This is a breaking change affecting the function `ggsurvtable()`. To fix this, the function `ggtext::element_markdown()` is now used in place of `element_text()` to handle vectorized colors (issue #455 fixed by pull #503).

## Bug fixes

- The Gehan-Breslow p-value is now correctly computed when the option `log.rank.weights = "n"` is specified in the function `ggsurvplot()` (#453)
- In `ggsurvplot()` examples, the function `gridExtra::rbind.gtable()` is now replaced by `gridExtra::gtable_rbind()` (@jan-imbi, pull #493).

# survminer 0.4.8

## Minor changes

- Maintenance update due to new broom 0.7.0 version by explicitly setting conf.int = TRUE in the call to tidy.coxph from `ggforest()` (pull 485).

# Survminer 0.4.7

## Minor changes
   
- In older versions of the survival package, the function `survfit(res.cox)` returned an object of class survfit.cox. The class has been changed to `survfitcox` in the current survival package version. The survminer package has been now updated to take this change into account ([@edvbb, #441](https://github.com/kassambara/survminer/issues/441)).

Fixes to adapt to dplyr 1.0.0 ([@romainfrancois, #460](https://github.com/kassambara/survminer/pull/460)): 
    
- Using group_by() instead of group_by_() which is deprecated
- Putting the extra "surv_group_by" class first where it is supposed to be instead of last, which messes up with some internal processing from vctrs.
     
     
## Bug fxes
   
- When the group size is small (i.e. n = 1), NAs are introduced during the computation of the confidence interval leading to a failure when specifying the option `conf.int` in the `ggsurvplot()` function. To fix this issue, Now, NAs are removed by default when drawing the confidence interval (#443 and #315). 

# Survminer 0.4.6
    
## New features
   
- A new function `surv_adjustedcurves` is extracted from `ggadjustedcurves`. This function calculates adjusted survival curves but do not plot them. Its results may be useful for calculation of median survival or some other statistics. ([@pbiecek, #423](https://github.com/kassambara/survminer/pull/423)). 

     
## Minor changes 
   
- Adapted to tidyr 1.0.0 (#424)
   
# Survminer 0.4.5
   
## Minor changes

- Adding variable with a group-agnostic approach ([@jennybc, #414](https://github.com/kassambara/survminer/pull/414)
- `cmprsk` is no longer needed for survminer installation. The package has been moved from Imports to Suggests. It's only used in documentations ([@massimofagg, #394](https://github.com/kassambara/survminer/issues/394).

## Bug fixes
   
- Now, in `ggflexsurvplot()`, the grouping variable can be factor or character vector ([@andersbergren , #393](https://github.com/kassambara/survminer/issues/408)
- Bug fixed for plotting confidence intervals for coxph using ggsurvplot ([@kharknes, #393](https://github.com/kassambara/survminer/issues/393)
   
   
# Survminer 0.4.4
   
## Minor changes
   
- ggforest updated to take into account interactions and polynomial or spline terms ([@fabian-s, #306](https://github.com/kassambara/survminer/issues/306), [@fabian-s, #388](https://github.com/kassambara/survminer/pull/388)
- Removed unnecessary call to `anova()` as requested ([@pbiecek, #391](https://github.com/kassambara/survminer/issues/391)
   
## Bug fixes
    
- When a factor variable name is the same as one of its level, `ggsurvplot()` failed ([@KohSoonho, #387](https://github.com/kassambara/survminer/issues/387)). Fixed now.
- `ggsurvplot()` can now create correctly faceted survival curves ([@uraniborg, #254](https://github.com/kassambara/survminer/pull/254), [@BingxinS, #363](https://github.com/kassambara/survminer/pull/363))

- A typo fixed in the formula for weightened log-rank test ([@MarcinKosinski, #336](https://github.com/kassambara/survminer/pull/336).
    
- `surv_summary()` can now handle the output of `survfit(cox.model, newdata)` when the option `conf.type = "none"` is specified by users ([@HeidiSeibold, #335](https://github.com/kassambara/survminer/pull/335).

- `ggadjustedcurves()` has now flipped labels for `conditional`/`marginal` to mach names from ’Adjusted Survival Curves’ by Terry Therneau, Cynthia Crowson, Elizabeth Atkinson (2015) ([@pbiecek, #335](https://github.com/kassambara/survminer/pull/358).

# Survminer 0.4.3

## New features
   
- Now `ggsurvplot()` can be used to plot survreg model ([@HeidiSeibold, #276](https://github.com/kassambara/survminer/issues/276), #325 ).
   
   
 
## Minor changes

- Now, `ggforest()` simply returns a ggplot instead of drawing automatically the plot ([@grvsinghal, #267](https://github.com/kassambara/survminer/issues/321)).

## Bug fixes

- Now, hiding strata names in risk table work when combining survfits ([@krassowski, #317](https://github.com/kassambara/survminer/issues/317)).
- Now, `axes.offset` argument is also applied to risk table ([@dmartinffm, #243](https://github.com/kassambara/survminer/issues/243)).
- It is now possible to add `ggsurvplot` to powerpoint document using ReporteRs even if there is no risk table ([@DrRZ, #314](https://github.com/kassambara/survminer/issues/314)).

# Survminer 0.4.2
  
   
## Minor changes

- New argument `size` added in `ggadjustedcurves()` to change the curve size ([@MaximilianTscharre, #267](https://github.com/kassambara/survminer/issues/267)).

## Bug fixes

- Now, confidence interval ribbon works properly ([@wp07, #275](https://github.com/kassambara/survminer/issues/275)). 
- Now, the argument `ggtheme` is supported when combining a list of survfit objects in `ggsurvplot()` ([@PhonePong, #278](https://github.com/kassambara/survminer/issues/278)). 

# survminer 0.4.1
   
## New features
  
- New function `ggflexsurvplot()` to create ggplot2-based graphs for flexible survival models.

- The function `ggadjustedcurves()` handles now argument `method` that defines how adjusted curves shall be calculated. With `method='conditional'|'marginal'` subpopulations are balanced with respect to variables present in the model formula. With `method='single'|'average'` the curve represents just the expected survival curves.
  
  

## Major changes

- The function `ggcoxadjustedcurves()` is replaced by `ggadjustedcurves()` (#229). 
   
## Minor changes

- The grouping variable to the  `ggadjustedcurves()` function is now passed as a name (character) of grouping variable not as a vector with values of grouping variable.

- New argument `font.family` in `ggsurvtable()` to change the font family in the survival tables - such as risk, cummulative events and censoring tables. For example font.family = "Courier New" ([@Swechhya, #245](https://github.com/kassambara/survminer/issues/245)).

- Now, in `ggsurvplot()` the data argument should be strictly provided ([@dnzmarcio, #235](https://github.com/kassambara/survminer/issues/235))
  
  
## Bug fixes
    
- `ggforest()` no longer tries to bolt a table full of text to the coefficient plot ([@mmoisse, #241](https://github.com/kassambara/survminer/issues/241)), instead the annotations are done via ggplot2::annotate, see example at: [@fabian-s, #264](https://github.com/kassambara/survminer/pull/264)   

    
# survminer 0.4.0

## New features
   
   
### New options in ggsurvplot()
   
   
- New argument `test.for.trend` added in `ggsurvplot()` to perform a log-rank test for trend. logical value. Default is FALSE. If TRUE, returns the test for trend p-values. Tests for trend are designed to detect ordered differences in survival curves. That is, for at least one group. The test for trend can be only performed when the number of groups is > 2 ([#188](https://github.com/kassambara/survminer/issues/188)).
   
- New argument `add.all` added now in `ggsurvplot()` to add he survival curves of (all) pooled patients onto the main survival plot stratified by grouping variables. Alias of the `ggsurvplot_add_all()` function ([#194](https://github.com/kassambara/survminer/issues/194)).
    
- New argument `combine = TRUE` is now available in the `ggsurvplot()` function to combine a list of survfit objects on the same plot. Alias of the *ggsurvplot_combine*() function ([#195](https://github.com/kassambara/survminer/issues/195)).

-  The standard convention of ggplot2 is to have the axes offset from the origin. This can be annoying with Kaplan-Meier plots. New argument `axes.offset` added non in `ggsurvplot()`.  logical value. Default is TRUE. If FALSE, set the plot axes to start at the origin (c(0,0)) ([#196](https://github.com/kassambara/survminer/issues/196)). 
      
- The function `ggsurvplot()` can take a list of survfit objects and produces a list of ggsurvplots (#204).
   
- New argument `facet.by` added now in `ggsurvplot()` to draw multi-panel survival curves of a data set grouped by one or two variables. Alias of the `ggsurvplot_facet()` function (#205).
  
- New argument `group.by` added now in `ggsurvplot()` to create survival curves of grouped data sets. Alias of the `ggsurvplot_group_by()` function.
   

- In `ggsurvplot()`, one can specify pval = TRUE/FALSE as a logical value. Now, it's also possible to specify the argument `pval` as a numeric value (e.g.: pval = 0.002), that will be passed to the plot, so that user can pass any custom p-value to the final plot ([@MarcinKosinski, #189](https://github.com/kassambara/survminer/issues/189)) or one can specify it as a character string (e.g.: pval = "p < 0001") ([@MarcinKosinski, #193](https://github.com/kassambara/survminer/issues/193)).
   
   
- New argument `xscale` in `ggsurvplot()`: numeric or character value specifying x-axis scale.
    - If numeric, the value is used to divide the labels on the x axis. For example, a value of 365.25 will give labels in years instead of the original days.
    - If character, allowed options include one of c("d_m", "d_y", "m_d", "m_y", "y_d", "y_m"), where d = days, m = months and y = years. For example, xscale = "d_m" will transform labels from days to months; xscale = "m_y", will transform labels from months to years ([#166](https://github.com/kassambara/survminer/issues/166)). 
    
- New arguments `censor.shape` and `censor.size` to change the shape and the shape of censors ([#186](https://github.com/kassambara/survminer/issues/186) & [#187](https://github.com/kassambara/survminer/issues/187)).
     
     
- New argument `conf.int.alpha` added in `ggsurvplot()`. Numeric value specifying fill color transparency. Value should be in [0, 1], where 0 is full transparency and 1 is no transparency.
    
    
### New functions
  
- New function `surv_group_by()` added to create a grouped data set for survival analysis.
   
- New function `ggsurvplot_df()` added. An extension to ggsurvplot() to plot survival curves from any data frame containing the summary of survival curves as returned the surv_summary() function. Might be useful for a user who wants to use ggsurvplot for visualizing survival curves computed by another method than the standard survfit.formula function. In this case, the user has just to provide the data frame containing the summary of the survival analysis.
   
- New function `surv_median()` added to easily extract median survivals from one or a list of survfit objects (#207).
   
   
- New function `surv_pvalue`() added to compute p-value from survfit objects or parse it when provided by the user. Survival curves are compared using the log-rank test (default). Other methods can be specified using the argument method.
   
- New function `surv_fit`() added to handle complex situation when computing survival curves (Read more in the doc: *?surv_fit*). Wrapper arround the standard `survfit`() [*survival*] function to create survival curves. Compared to the standard survfit() function, it supports also:  
    - a list of data sets and/or a list of formulas,
    - a grouped data sets as generated by the function surv_group_by,
    - group.by option
   

    
## Major changes

- The `ggforest()` function has changed a lot. Now presents much more statistics for each level of each variable (extracted with `broom::tidy`) and also some statistics for the `coxph` model, like AIC, p.value, concordance (extracted with `broom::glance`) ([#178](https://github.com/kassambara/survminer/issues/178))
   
## Minor changes
     
- Now, `ggcompetingrisks()` supports the `conf.int` argument. If `conf.int=TRUE` and `fit` is an object of class `cuminc` then confidence intervals are plotted with `geom_ribbon`.
  
- Now, `ggsurvplot()` supports the `survfit()` outputs when used with the argument `start.time`.
  
- Now, the default behaviour of `ggsurvplot()` is to round the number at risk using the option `digits = 0` (#214).

- `pairwise_survdiff()` has been improved to handle a formula with multiple variables (#213).

- The argument `color` are updated allowing to assign the same color for same groups accross facets (#99 & [#185](https://github.com/kassambara/survminer/issues/185)).
    - If the number of strata/group (n.strata) = 1, the expected value is the color name. For example color = "blue".
    - If n.strata > 1, the expected value is the grouping variable name. By default, survival curves are colored by strata using the argument color = "strata", but you can also color survival curves by any other grouping variables used to fit the survival curves.
    
For example, in the following script, survival curves are colored by the grouping variable `sex` in all facets:  
   
```r
library(survminer)
library(survival)
fit <- survfit( Surv(time, status) ~ sex + rx + adhere,
                 data = colon )
ggsurv <- ggsurvplot(fit, data = colon,
               color = "sex",
               legend.title = "Sex",
               palette = "jco")
ggsurv$plot + facet_grid(rx ~ adhere)
```

   
- Now, the function `pairwise_survdiff()` checks whether the grouping variable is a factor. If this is not the case, the grouping variable is automatically converted into a factor.
- `ggsurvplot()`: Now, log scale is used for x-axis when plotting the complementary log−log function (argument `fun = "cloglog") ([#171](https://github.com/kassambara/survminer/issues/171)).

- Now, the argument `palette` in `ggsurvplot()` ccan be also a numeric vector of length(strata); in this case a basic color palette is created using the function `grDevices::palette()`.
   
- The `%+%` function in `survminer` has been replaced by `%++%` to avoid breaking the `ggplot2::%+%` function behavior when using survminer (#199 and #200). 
   
- New argument `fun` added in `ggcoxadjustedcurves()` ([@meganli, #202](https://github.com/kassambara/survminer/issues/202)).

- The function `theme_classic2()` removed.

## Bug fixes

- Columns/Rows are now correctly labeled in `pairwise_survdiff`() display ([@mriffle, #212](https://github.com/kassambara/survminer/issues/212)).

- Now, the `pairwise_survdiff()` function works when the data contain NAs ([@emilelatour , #184](https://github.com/kassambara/survminer/issues/184)).
   
- Now, `ggsurvplot()` fully supports different methods, in the *survMisc* package, for comparing survival curves ([#191](https://github.com/kassambara/survminer/issues/191)).

# survminer 0.3.1

## Minor changes

- The example section of the `ggcoxdiagnostics()` function and the vignette file `Informative_Survival_Plots.Rmd` have been updated so that `survminer` can pass CRAN check under R-oldrelease.
- New example dataset `BMT` added for competing risk analysis.
- New data set `BRCAOV.survInfo` added, used in vignette files

## Bug fixes
   
- Now, `palette` argument works in `ggcoxadjustedcurves() ([#174](https://github.com/kassambara/survminer/issues/174))
- Now `ggsurvplot()` works when the `fun` argument is an arbitrary function ([#176](https://github.com/kassambara/survminer/issues/176)).

# survminer 0.3.0
   
## New features
    
### New options in ggsurvplot()
     
- Additional `data` argument added to the `ggsurvplot()` function ([\@kassambara, #142](https://github.com/kassambara/survminer/issues/142)). Now, it's recommended to pass to the function, the data used to fit survival curves. This will avoid the error generated when trying to use the `ggsurvplot()` function inside another functions ([\@zzawadz, #125](https://github.com/kassambara/survminer/issues/125)).
   
   
- New argument `risk.table.pos`, for placing risk table inside survival curves (#69). Allowed options are one of c("out", "in") indicating 'outside' or 'inside' the main plot, respectively. Default value is "out".  

- New arguments `tables.height, tables.y.text, tables.theme, tables.col`: for customizing tables under the main survival plot:  ([#156](https://github.com/kassambara/survminer/issues/156)). 
   
- New arguments `cumevents` and `cumcensor`: logical value for displaying the cumulative number of events table ([#117](https://github.com/kassambara/survminer/issues/117)) and the cumulative number of censored subject ([#155](https://github.com/kassambara/survminer/issues/155)), respectively.
   

- Now, `ggsurvplot()` can display both the number at risk and the cumulative number of censored in the same table using the option `risk.table = 'nrisk_cumcenor'` (#96). It's also possible to display the number at risk and the cumulative number of events using the option `risk.table = 'nrisk_cumevents'`.
    
- New arguments `pval.method` and `log.rank.weights`: New possibilities to compare survival curves. Functionality based on `survMisc::comp`.
   
- New arguments `break.x.by` and `break.y.by`, numeric value controlling x and y axis breaks, respectively. 
   
- Now, `ggsurvplot()` returns an object of class ggsurvplot which is list containing the following components ([#158](https://github.com/kassambara/survminer/issues/158)):
    - **plot**: the survival plot (ggplot object)
    - **table**: the number of subjects at risk table per time (ggplot object). Returned only when risk.table = TRUE.
    - **cumevents**: the cumulative number of events table (ggplot object). Returned only when cumevents = TRUE.
    - **ncensor.plot**: the number of censoring (ggplot object). Returned only when ncensor.plot = TRUE or cumcensor = TRUE.
    - **data.survplot**: the data used to plot the survival curves (data.frame).
    - **data.survtable**: the data used to plot the tables under the main survival curves (data.frame).
   
    
### Themes
   
 
- New function `theme_survminer()` to change easily the graphical parameters of plots generated with survminer ([#151](https://github.com/kassambara/survminer/issues/151)). A theme similar to theme_classic() with large font size. Used as default theme in survminer functions.
  
- New function `theme_cleantable()` to draw a clean risk table and cumulative number of events table. Remove axis lines, x axis ticks and title ([#117](https://github.com/kassambara/survminer/issues/117) & [#156](https://github.com/kassambara/survminer/issues/156)).
    
    
```r
# Fit survival curves
require("survival")
fit<- survfit(Surv(time, status) ~ sex, data = lung)

# Survival curves
require("survminer")
ggsurvplot(fit, data = lung, risk.table = TRUE,
    tables.theme = theme_cleantable()
    )
```
    
### New functions

- New function `+.ggsurv()` to add ggplot components - `theme()`, `labs()` -  to an object of class ggsurv, which is a list of ggplots. ([#151](https://github.com/kassambara/survminer/issues/151)). For example:

```r
# Fit survival curves
require("survival")
fit<- survfit(Surv(time, status) ~ sex, data = lung)

# Basic survival curves
require("survminer")
p <- ggsurvplot(fit, data = lung, risk.table = TRUE)
p

# Customizing the plots
p %+% theme_survminer(
     font.main = c(16, "bold", "darkblue"),
     font.submain = c(15, "bold.italic", "purple"),
     font.caption = c(14, "plain", "orange"),
     font.x = c(14, "bold.italic", "red"),
     font.y = c(14, "bold.italic", "darkred"),
     font.tickslab = c(12, "plain", "darkgreen")
)

```
  
- New function `arrange_ggsurvplots()` to arrange multiple ggsurvplots on the same page (#66).
  
- New function `ggsurvevents()` to calculate and plot the distribution for events (both status = 0 and status = 1); with `type` parameter one can plot cumulative distribution of locally smooth density; with normalised, distributions are normalised. This function helps to notice when censorings are more common ([\@pbiecek, #116](https://github.com/kassambara/survminer/issues/116)). 
    
- New function `ggcoxadjustedcurves()` to plot adjusted survival curves for Cox proportional hazards model ([\@pbiecek, #133](https://github.com/kassambara/survminer/issues/133) & \@markdanese, #67).
   
- New function `ggforest()` for drawing forest plot for the Cox model.   
    
- New function `pairwise_survdiff()` for multiple comparisons of survival Curves (#97).
     
- New function `ggcompetingrisks()` to plot the cumulative incidence curves for competing risks ([\@pbiecek, #168](https://github.com/kassambara/survminer/issues/168).
    
### Helper functions
    
New heper functions `ggrisktable()`, `ggcumevents()`, `ggcumcensor()`. Normally, users don't need to use these function directly. Internally used by the function `ggsurvplot()`.
     
- `ggrisktable()` for plotting number of subjects at risk by time. ([#154](https://github.com/kassambara/survminer/issues/154)).
- `ggcumevents()` for plotting the cumulative number of events table ([#117](https://github.com/kassambara/survminer/issues/117)).
- `ggcumcensor()` for plotting the cumulative number of censored subjects table ([#155](https://github.com/kassambara/survminer/issues/155)).
    
   

## Major changes
     
- New argument `sline` in the `ggcoxdiagnostics()` function for adding loess smoothed trend on the residual plots. This will make it easier to spot some problems with residuals (like quadratic relation). ([\@pbiecek, #119](https://github.com/kassambara/survminer/issues/119)). 
   

- The design of `ggcoxfunctional()` has been changed to be consistent with the other functions in the survminer package. Now, `ggcoxfunctional()` works with coxph objects not formulas. The arguments formula is now deprecated ([\@pbiecek, #115](https://github.com/kassambara/survminer/issues/115)).
   
- In the `ggcoxdiagnostics()` function, it's now possible to plot Time in the OX axis ([\@pbiecek, #124](https://github.com/kassambara/survminer/issues/124)). This is convenient for some residuals like Schoenfeld. The `linear.predictions` parameter has been replaced with `ox.scale = c("linear.predictions", "time", "observation.id")`.
     
     
## Minor changes
  
- New argument `tables.height` in `ggsurvplot()` to apply the same height to all the tables under the main survival plots ([#157](https://github.com/kassambara/survminer/issues/157)).

- It is possible to specify `title` and `caption` for `ggcoxfunctional` ([\@MarcinKosinski, #138](https://github.com/kassambara/survminer/issues/138)) (`font.main` was removed as it was unused.)

- It is possible to specify `title`, `subtitle` and `caption` for `ggcoxdiagnostics` ([\@MarcinKosinski, #139](https://github.com/kassambara/survminer/issues/139)) and `fonts` for them.

- It is possible to specify global `caption` for `ggcoxzph` ([\@MarcinKosinski, #140](https://github.com/kassambara/survminer/issues/140)).

- In `ggsurvplot()`, more information, about color palettes, have been added in the details section of the documentation ([#100](https://github.com/kassambara/survminer/issues/100)).  

- The R package `maxstat` doesn't support very well an object of class `tbl_df`. To fix this issue, now, in the `surv_cutpoint()` function, the input data is systematically transformed into a standard data.frame format ([\@MarcinKosinski, #104](https://github.com/kassambara/survminer/issues/104)).

- It's now possible to print the output of the survminer packages in a powerpoint created with the ReporteRs package. You should use the argument *newpage = FALSE* in the `print()` function when printing the output in the powerpoint. Thanks to ([\@abossenbroek, #110](https://github.com/kassambara/survminer/issues/110)) and ([\@zzawadz, #111](https://github.com/kassambara/survminer/issues/111)). For instance:   
    
    
```r
require(survival)
require(ReporteRs)
require(survminer)

fit <- survfit(Surv(time, status) ~ rx + adhere, data =colon)
survplot <- ggsurvplot(fit, pval = TRUE,
                       break.time.by = 400,
                       risk.table = TRUE,
                       risk.table.col = "strata",
                       risk.table.height = 0.5, # Useful when you have multiple groups
                       palette = "Dark2")

require(ReporteRs)
doc = pptx(title = "Survival plots")
doc = addSlide(doc, slide.layout = "Title and Content")
doc = addTitle(doc, "First try")
doc = addPlot(doc, function() print(survplot, newpage = FALSE), vector.graphic = TRUE)
writeDoc(doc, "test.pptx")
```
    
    
- Now, in `ggcoxdiagnostics()`, the option `ncol = 1` is removed from the function `facet_wrap()`. By default, `ncol = NULL`. In this case, the number of columns and rows in the plot panels is defined automatically based on the number of covariates included in the cox model.
    
## Bug fixes
    
- Now, risk table align with survival plots when legend = "right" ([\@jonlehrer, #102](https://github.com/kassambara/survminer/issues/102)).

- Now, `ggcoxzph()` works for univariate Cox analysis ([#103](https://github.com/kassambara/survminer/issues/103)). 
   
- Now, `ggcoxdiagnostics()` works properly for schoenfeld residuals ([\@pbiecek, #119](https://github.com/kassambara/survminer/issues/122)).  
   
- Now, `ggsurvplot()` works properly in the situation where `strata()` is included in the cox formula ([#109](https://github.com/kassambara/survminer/issues/109)). 
   
## Vignettes and examples

- A new vignette and a `ggsurvplot` example was added to present new functionalities of possible texts and fonts customizations. 
  
- A new vignette and a `ggsurvplot` example was added to present new functionalities of possible weights specification in a Log-rank test.    

# survminer 0.2.4
     
## Bug fixes
     
- `surv_summary()` (v0.2.3) generated an error when the name of the variable used in `survfit()` can be found multiple times in the levels of the same variable. For example, variable = therapy; levels(therapy) --> "therapy" and "hormone therapy" (#86). This has been now fixed.

- To extract variable names used in `survival::survfit()`, the R code `strsplit(strata, "=|,\\s+", perl=TRUE)` was used in the `surv_summary()` function [survminer v0.2.3]. The splitting was done at any "=" symbol in the string, causing an error when special characters (=, <=, >=) are used for the levels of a categorical variable (#91). This has been now fixed.

- Now, `ggsurvplot()` draws correctly the risk.table (#93).
   
   
# survminer 0.2.3
    
    
## New features
   
- New function `surv_summary()` for creating data frame containing a nice summary of a survival curve (#64).
- It's possible now to facet the output of `ggsurvplot()` by one or more factors (#64):

```
# Fit complexe survival curves
require("survival")
fit3 <- survfit( Surv(time, status) ~ sex + rx + adhere,
                data = colon )
                
# Visualize by faceting
# Plots are survival curves by sex faceted by rx and adhere factors.
require("survminer")  
ggsurv$plot +theme_bw() + facet_grid(rx ~ adhere)
```
   
- Now, `ggsurvplot()` can be used to plot cox model (#67).
- New 'myeloma' data sets added.
- New functions added for determining and visualizing the optimal cutpoint of continuous variables for survival analyses:   
   - `surv_cutpoint()`: Determine the optimal cutpoint for each variable using 'maxstat'. Methods defined for surv_cutpoint object are summary(), print() and plot().
   - `surv_categorize()`: Divide each variable values based on the cutpoint returned by `surv_cutpoint()` (#41).
- New argument 'ncensor.plot' added to `ggsurvplot()`. A logical value. If TRUE, the number of censored subjects at time t is plotted. Default is FALSE ([#18](https://github.com/kassambara/survminer/issues/18)).
  
  
## Minor changes
   
- New argument 'conf.int.style' added in `ggsurvplot()` for changing the style of confidence interval bands.
- Now, `ggsurvplot()` plots a stepped confidence interval when conf.int = TRUE (#65).
- `ggsurvplot()` updated for compatibility with the future version of ggplot2 (v2.2.0) (#68)
- ylab is now automatically adapted according to the value of the argument `fun`. For example, if fun = "event", then ylab will be "Cumulative event".
- In `ggsurvplot()`, linetypes can now be adjusted by variables used to fit survival curves (#46)
- In `ggsurvplot()`, the argument risk.table can be either a logical value (TRUE|FALSE) or a string ("absolute", "percentage"). If risk.table = "absolute", `ggsurvplot()` displays the absolute number of subjects at risk. If risk.table = "percentage", the percentage at risk is displayed. Use "abs_pct" to show both the absolute number and the percentage of subjects at risk (#70).
- New argument surv.median.line in `ggsurvplot()`: character vector for drawing a horizontal/vertical line at median (50%) survival. Allowed values include one of c("none", "hv", "h", "v"). v: vertical, h:horizontal (#61).
- Now, default theme of ggcoxdiagnostics() is ggplot2::theme_bw().
   
   
## Bug fixes
    
- `ggcoxdiagnostics()` can now handle a multivariate Cox model (#62)
- `ggcoxfunctional()` now displays graphs of continuous variable against martingale residuals of null cox proportional hazards model (#63).
- When subset is specified in the survfit() model, it's now considered in `ggsurvplot()` to report the right p-value on the subset of the data and not on the whole data sets ([@jseoane, #71](https://github.com/kassambara/survminer/issues/71)).
- `ggcoxzph()` can now produce plots only for specified subset of varibles ([@MarcinKosinski, #75](https://github.com/kassambara/survminer/issues/75))   
   
# survminer 0.2.2
    
    
## New features
   
- New `ggcoxdiagnostics` function that plots diagnostic graphs for Cox Proportional Hazards model ([@MarcinKosinski, #16](https://github.com/kassambara/survminer/issues/16)).
- Vignette added: `Survival plots have never been so informative` ([@MarcinKosinski, #39](https://github.com/kassambara/survminer/issues/39))
- New argument linetype in 'ggsurvplot' ([@MarcinKosinski, #45](https://github.com/kassambara/survminer/issues/45)). Allowed values includes i) "strata" for changing linetypes by strata (i.e. groups); ii) a numeric vector (e.g., c(1, 2)) or a character vector c("solid", "dashed").
   
## Bug fixes
    
- lienetype argument changed to linetype in `ggsurvplot()` documentation. ([@ViniciusBRodrigues, #43](https://github.com/kassambara/survminer/issues/43))
    
# survminer 0.2.1

## New features

- New `ggcoxzph` function that displays a graph of the scaled Schoenfeld residuals, along with a smooth curve using 'ggplot2'. Wrapper around \link{plot.cox.zph}. ([@MarcinKosinski, #13](https://github.com/kassambara/survminer/issues/13))

- New `ggcoxfunctional` function that displays graphs of continuous explanatory variable against martingale residuals of null
 cox proportional hazards model, for each term in of the right side of input formula. This might help to properly choose the functional form of continuous variable in cox model, since fitted lines with `lowess` function should be linear to satisfy cox proportional hazards model assumptions. ([@MarcinKosinski, #14](https://github.com/kassambara/survminer/issues/14))
 
- New function `theme_classic2`: ggplot2 classic theme with axis line. This function replaces ggplot2::theme_classic, which does no longer display axis lines (since ggplot2 v2.1.0)
   
## Minor changes

- post-customization of color and fill no longer shows warnings like "Scale for 'fill' is already present. Adding another scale for 'fill', which will replace the existing scale" ([@MarcinKosinski, #11](https://github.com/kassambara/survminer/issues/11)).
- now, post-customization of survival curve colors will automatically affect the risk table y axis text colors ([@MarcinKosinski, #11](https://github.com/kassambara/survminer/issues/12)).
- Default value for the argument `risk.table.y.text.col` is now TRUE.
- New argument risk.table.y.text for the function `ggsurvplot`. logical argument. Default is TRUE. If FALSE, risk table y axis tick labels will be hidden (@MarcinKosinski, #28).
   
   
## Bug fixes
   
- Black dots removed from risk table ([@Feli-Anna, #25](https://github.com/kassambara/survminer/issues/25))

# survminer 0.2.0

## New features
   
- New arguments in ggsurvplot for changing font style, size and color of main title, axis labels, axis tick labels and legend labels: *font.main, font.x, font.y, font.tickslab, font.legend*.
- New arguments *risk.table.title, risk.table.fontsize* in ggsurvplot
- New argument *risk.table.y.text.col*: logical value. Default value is FALSE. If TRUE, risk table tick labels will be colored by strata ([@MarcinKosinski, #8](https://github.com/kassambara/survminer/issues/8)).

- ```print.ggsurvplot()``` function added: S3 method for class 'ggsurvplot'. 
  
- ggsurvplot returns an object of class ggsurvplot which is list containing two ggplot objects: 
    - *plot*: the survival plot
    - *table*: the number at risk table per time
    
    
- It's now possible to customize the output survival *plot* and the *risk table* returned by ggsurvplot, and to print again the final plot.  ([@MarcinKosinski, #2](https://github.com/kassambara/survminer/issues/2)):
  
```
# Fit survival curves
require("survival")
fit<- survfit(Surv(time, status) ~ sex, data = lung)

# visualize
require(survminer)
ggsurvplot(fit, pval = TRUE, conf.int = TRUE,
          risk.table = TRUE)

# Customize the output and then print
res <- ggsurvplot(fit, pval = TRUE, conf.int = TRUE,
           risk.table = TRUE)
res$table <- res$table + theme(axis.line = element_blank())
res$plot <- res$plot + labs(title = "Survival Curves")
print(res)
```
 
   
## Minor changes
   
- p < 0.0001 is used (when pvalue < 0.0001).

## Bug fixes
  
- ggtheme now affects risk.table ([@MarcinKosinski, #1](https://github.com/kassambara/survminer/issues/1))

- xlim changed to cartesian coordinates mode ([@MarcinKosinski, #4](https://github.com/kassambara/survminer/issues/4)).  The Cartesian coordinate system is the most common type of coordinate system. It will zoom the plot (like you’re looking at it with a magnifying glass), without clipping the data.

- Risk table and survival curves have now the same color and the same order

- Plot width is no longer too small when legend position = "left" ([@MarcinKosinski, #7](https://github.com/kassambara/survminer/issues/7)).
    

# survminer 0.1.1

## New features
    
- **ggsurvplot()**: Drawing survival curves using ggplot2
