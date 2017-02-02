# survminer 0.2.4.999
   
## New features
   
- New function `ggcoxadjustedcurves()` added to plot adjusted survival curves for Cox proportional hazards model ([@pbiecek, #133](https://github.com/kassambara/survminer/issues/133) & [@markdanese, #67](https://github.com/kassambara/survminer/issues/67)).

- New possibilities to compare survival curves with `pval.method` and `log.rank.weights` parameters in `ggsurvplot()`, by specifing weights in Log-rank test. Functionality based on `survMisc::comp`.
- New function `ggforest()` added for drawing forest plot for the Cox model.
- New argument `sline` in the `ggcoxdiagnostics()` function for adding loess smoothed trend on the residual plots. This will make it easier to spot some problems with residuals (like quadratic relation). ([@pbiecek, #119](https://github.com/kassambara/survminer/issues/119)). 
- New function `pairwise_survdiff()` for multiple comparisons of survival Curves ([#97](https://github.com/kassambara/survminer/issues/97)).
   

## Major changes
   
- Additional `data` argument added to the `ggsurvplot()` function ([@kassambara, #142](https://github.com/kassambara/survminer/issues/142)). Now, it's recommended to pass to the function, the data used to fit survival curves. This will avoid the error generated when trying to use the `ggsurvplot()` function inside another functions ([@zzawadz, #125](https://github.com/kassambara/survminer/issues/125)).

- New fonts and texts customization features for `ggsurvplot` ([@MarcinKosinski, #105](https://github.com/kassambara/survminer/issues/105)) ([@MarcinKosinski, #106](https://github.com/kassambara/survminer/issues/106))
    - It is possible to specify subtitle (param `submain`) and caption (param `caption`) for the `curve` part of the `ggsurvplot`.
    - It is possible to specify subtitle (param `risk.table.subtitle`) and caption (param `risk.table.caption`) for the `table` part of the `ggsurvplot`.
    - It is possible to specify title (param `ncens.plot.title`), subtitle (param `ncens.plot.subtitle`) and caption (param `ncens.plot.caption`) for the `ncens` part of the `ggsurvplot`.
    - ncens.plot part of the `ggsurvplot` has no integer values on y axis (not real as before), as it only can take integer values.
    - Fonts can be specified directly for `curve`, `risk.table` and `ncensor.plot` elements ([@MarcinKosinski, #106](https://github.com/kassambara/survminer/issues/106)).
    - README was extended with `uber platinium (premium) customization` examples. 
    
- The design of `ggcoxfunctional()` has been changed to be consistent with the other functions in the survminer package. Now, `ggcoxfunctional()` works with coxph objects not formulas. The arguments formula is now deprecated ([@pbiecek, #115](https://github.com/kassambara/survminer/issues/115)).
   
- In the `ggcoxdiagnostics()` function, it's now possible to plot Time in the OX axis ([@pbiecek, #124](https://github.com/kassambara/survminer/issues/124)). This is convenient for some residuals like Schoenfeld. The `linear.predictions` parameter has been replaced with `ox.scale = c("linear.predictions", "time", "observation.id")`.
     
     
## Minor changes

- It is possible to secify `title` and `caption` for `ggcoxfunctional` ([@MarcinKosinski, #138](https://github.com/kassambara/survminer/issues/138)) (`font.main` was removed as it was unused.)

- It is possible to secify `title`, `subtitle` and `caption` for `ggcoxdiagnostics` ([@MarcinKosinski, #139](https://github.com/kassambara/survminer/issues/139)) and `font`s for them.

- It is possible to secify global `caption` for `ggcoxzph` ([@MarcinKosinski, #140](https://github.com/kassambara/survminer/issues/140)).

- In `ggsurvplot()`, more information, about color palettes, have been added in the details section of the documentation ([#100](https://github.com/kassambara/survminer/issues/100)).  

- The R package `maxstat` doesn't support very well an object of class `tbl_df`. To fix this issue, now, in the `surv_cutpoint()` function, the input data is systematically transformed into a standard data.frame format ([@MarcinKosinski, #104](https://github.com/kassambara/survminer/issues/104)).

- It's now possible to print the output the survminer packages in a powerpoint created with the ReporteRs package. Thanks to ([@abossenbroek, #110](https://github.com/kassambara/survminer/issues/110)) and ([@zzawadz, #111](https://github.com/kassambara/survminer/issues/110)). You should use the argument *newpage = FALSE* in the `print()` function when printing the output in the powerpoint. For instance:   
    
    
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
    
- Now, `gggcoxzph()` works for univariate Cox analysis ([#103](https://github.com/kassambara/survminer/issues/103)). 
   
- Now, `ggcoxdiagnostics()` works properly for schoenfeld residuals ([@pbiecek, #119](https://github.com/kassambara/survminer/issues/122)).  
   
- Now, `ggsurvplot()` works properly in the situation where `strata()` is included in the cox formula ([#109](https://github.com/kassambara/survminer/issues/109)). 
   
## Vignettes and examples

- A new vignette and a `ggsurvplot` example was added to present new functionalities of possible texts and fonts customizations. 
- A new vignette and a `ggsurvplot` example was added to present new functionalities of possible weights specification in a Log-rank test.    

# survminer 0.2.4
     
## Bug fixes
     
- `surv_summary()` (v0.2.3) generated an error when the name of the variable used in `survfit()` can be found multiple times in the levels of the same variable. For example, variable = therapy; levels(therapy) --> "therapy" and "hormone therapy" ([#86](https://github.com/kassambara/survminer/issues/86)). This has been now fixed.

- To extract variable names used in `survival::survfit()`, the R code `strsplit(strata, "=|,\\s+", perl=TRUE)` was used in the `surv_summary()` function [survminer v0.2.3]. The splitting was done at any "=" symbol in the string, causing an error when special characters (=, <=, >=) are used for the levels of a categorical variable ([#91](https://github.com/kassambara/survminer/issues/91)). This has been now fixed.

- Now, `ggsurvplot()` draws correctly the risk.table ([#93](https://github.com/kassambara/survminer/issues/93)).
   
   
# survminer 0.2.3
    
    
## New features
   
- New function `surv_summary()` for creating data frame containing a nice summary of a survival curve ([#64](https://github.com/kassambara/survminer/issues/64)).
- It's possible now to facet the output of `ggsurvplot()` by one or more factors ([#64](https://github.com/kassambara/survminer/issues/64)):

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
   
- Now, `ggsurvplot()` can be used to plot cox model ([#67](https://github.com/kassambara/survminer/issues/67)).
- New 'myeloma' data sets added.
- New functions added for determining and visualizing the optimal cutpoint of continuous variables for survival analyses:   
   - `surv_cutpoint()`: Determine the optimal cutpoint for each variable using 'maxstat'. Methods defined for surv_cutpoint object are summary(), print() and plot().
   - `surv_categorize()`: Divide each variable values based on the cutpoint returned by `surv_cutpoint()` ([#41](https://github.com/kassambara/survminer/issues/41)).
- New argument 'ncensor.plot' added to `ggsurvplot()`. A logical value. If TRUE, the number of censored subjects at time t is plotted. Default is FALSE ([#18](https://github.com/kassambara/survminer/issues/18)).
  
  
## Minor changes
   
- New argument 'conf.int.style' added in `ggsurvplot()` for changing the style of confidence interval bands.
- Now, `ggsurvplot()` plots a stepped confidence interval when conf.int = TRUE ([#65](https://github.com/kassambara/survminer/issues/65)).
- `ggsurvplot()` updated for compatibility with the future version of ggplot2 (v2.2.0) ([#68](https://github.com/kassambara/survminer/issues/68))
- ylab is now automatically adapted according to the value of the argument `fun`. For example, if fun = "event", then ylab will be "Cumulative event".
- In `ggsurvplot()`, linetypes can now be adjusted by variables used to fit survival curves ([#46](https://github.com/kassambara/survminer/issues/46))
- In `ggsurvplot()`, the argument risk.table can be either a logical value (TRUE|FALSE) or a string ("absolute", "percentage"). If risk.table = "absolute", `ggsurvplot()` displays the absolute number of subjects at risk. If risk.table = "percentage", the percentage at risk is displayed. Use "abs_pct" to show both the absolute number and the percentage of subjects at risk. ([#70](https://github.com/kassambara/survminer/issues/70)).
- New argument surv.median.line in `ggsurvplot()`: character vector for drawing a horizontal/vertical line at median (50%) survival. Allowed values include one of c("none", "hv", "h", "v"). v: vertical, h:horizontal ([#61](https://github.com/kassambara/survminer/issues/61)).
- Now, default theme of ggcoxdiagnostics() is ggplot2::theme_bw().
   
   
## Bug fixes
    
- `ggcoxdiagnostics()` can now handle a multivariate Cox model ([#62](https://github.com/kassambara/survminer/issues/62))
- `ggcoxfunctional()` now displays graphs of continuous variable against martingale residuals of null cox proportional hazards model ([#63](https://github.com/kassambara/survminer/issues/63)).
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
- New argument risk.table.y.text for the function `ggsurvplot`. logical argument. Default is TRUE. If FALSE, risk table y axis tick labels will be hidden ([@MarcinKosinski, #28](https://github.com/kassambara/survminer/issues/28)).
   
   
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
