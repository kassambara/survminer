[![Build Status](https://api.travis-ci.org/kassambara/survminer.png)](https://travis-ci.org/kassambara/survminer) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/survminer)](http://cran.r-project.org/package=survminer) [![Downloads](http://cranlogs.r-pkg.org/badges/survminer)](https://cran.r-project.org/package=survminer) [![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/survminer?color=orange)](http://cranlogs.r-pkg.org/badges/grand-total/survminer) [![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![Pending Pull-Requests](http://githubbadges.herokuapp.com/kassambara/survminer/pulls.svg?style=flat)](https://github.com/kassambara/survminer/pulls) [![Github Issues](http://githubbadges.herokuapp.com/kassambara/survminer/issues.svg)](https://github.com/kassambara/survminer/issues)

survminer: Survival Analysis and Visualization
==============================================

The **survminer** R package provides functions for facilitating **survival analysis** and **visualization**. The current version contains three main functions including:

-   **ggsurvplot**(): Draws survival curves with the 'number at risk' table.

-   **ggcoxzph**(): Graphical test of proportional hazards.

-   **ggcoxfunctional**(): Displays graphs of continuous explanatory variable against martingale residuals of null cox proportional hazards model. It helps to properly choose the functional form of continuous variable in cox model.

Find out more at <http://www.sthda.com/english/rpkgs/survminer/>, and check out the documentation and usage examples of each of the functions in survminer package.

Installation and loading
------------------------

Install from [CRAN](https://cran.r-project.org/package=survminer) as follow:

``` r
install.packages("survminer")
```

Or, install the latest version from [GitHub](https://github.com/kassambara/survminer):

``` r
# Install
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/survminer", build_vignettes = TRUE)
```

Load survminer:

``` r
library("survminer")
# Loading required package: ggplot2
```

ggsurvplot: Drawing survival curves
-----------------------------------

1.  **Fitting survival curves**

``` r
require("survival")
# Loading required package: survival
fit <- survfit(Surv(time, status) ~ sex, data = lung)
```

1.  **Basic plots**

``` r
ggsurvplot(fit)
```

![](README-ggplot2-basic-survival-plot-1.png)

1.  **Customized survival curves**

``` r
ggsurvplot(fit,  size = 1,  # change line size
           palette = c("#E7B800", "#2E9FDF"), # custom color palettes
           conf.int = TRUE, # Add confidence interval
           pval = TRUE, # Add p-value
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Risk table color by groups
           legend.labs = c("Male", "Female"), # Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw() # Change ggplot2 theme
           )
```

![](README-ggplot2-customized-survival-plot-1.png)

Note that, additional arguments are available to customize the main title, axis labels, the font style, axis limits, legends and the number at risk table.

Blog posts
----------

-   M. Kosiński. R-ADDICT May 2016. [Survival plots have never been so informative](http://r-addict.com/2016/05/23/Informative-Survival-Plots.html)

-   A. Kassambara. STHDA January 2016. [survminer R package: Survival Data Analysis and Visualization](http://www.sthda.com/english/wiki/survminer-r-package-survival-data-analysis-and-visualization).
