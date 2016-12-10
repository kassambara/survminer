## Test environments
* local OS X install, R 3.2.4
* win-builder 
* Travis

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## win-builder check results

There was 1 warning affecting particularly the vignette files: Informative_Survival_Plots.Rmd.

Before submitting the update version (v0.2.3) of survminer, I did a
check on winbulder that generated the following error:
"there is no package called 'RTCGA.clinical". (RTCGA.clinical is a bioconductor package.)
Could you, please, make sure that RTCGA.clinical is installed on winbulder.


## Downstream dependencies
There are currently no downstream dependencies for this package

## Update
This is an update version 0.2.4 (see NEWS.md). After v0.2.3 released on CRAN, users detected substantial bugs in the main function of the survminer package (ggsurvplot()). Would you be so kind to release version 0.2.4?
