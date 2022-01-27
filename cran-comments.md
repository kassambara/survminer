## Test environments
* local OS X install, R 3.6.3
* win-builder 
* Github Action

## R CMD check results
There were no ERRORs or WARNINGs.

There was one note concerning the subdirectory "doc", which contains vignette files.  
   
checking installed package size ... NOTE
  installed size is  5.1Mb
  sub-directories of 1Mb or more:
    doc   5.1Mb
   
Would you be so kind to ignore this note.
    
    
## Downstream dependencies
  
I have also run R CMD check on downstream dependencies of survminer. 
All packages that I could install passed.

## Update

This is an update version 0.4.9 (see NEWS.md)

## Resubmission

We fixed broken web links in the doc.



