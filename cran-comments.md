## Test environments
* local OS X install, R 3.3.2
* win-builder 
* Travis

## R CMD check results
There were no ERRORs or WARNINGs.

There were two notes:

1) one note concerning the subdirectory "doc", which contains vignette files.  
   
checking installed package size ... NOTE
  installed size is  5.5Mb
  sub-directories of 1Mb or more:
    doc   5.4Mb
    
2)  one note concerning Examples with CPU or elapsed time > 5s:
  
                     user system elapsed
ggcoxadjustedcurves 6.815  0.501   7.479
ggcoxdiagnostics    4.562  0.306   5.042
   
   
Would you be so kind to ignore these notes.
    
    
## win-builder check results
   
As in the R CMD check results, there were two notes:

- concerning the subdirectory "doc", which contains vignette files.
- and the example sections of ggcoxadjustedcurves and ggcoxdiagnostics functions.
  
## Downstream dependencies
There are currently no downstream dependencies for this package

## Update
This is an update version 0.3.0 (see NEWS.md).
