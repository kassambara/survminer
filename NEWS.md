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
  
```{r}
# Fit survival curves
#++++++++++++++++++++++++++++++++++++
require("survival")
fit<- survfit(Surv(time, status) ~ sex, data = lung)

# visualize
#++++++++++++++++++++++++++++++++++++
require(survminer)
ggsurvplot(fit, pval = TRUE, conf.int = TRUE,
          risk.table = TRUE)

# Customize the output and then print
#++++++++++++++++++++++++++++++++++++
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
