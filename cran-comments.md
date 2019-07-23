## Resubmission
This is a resubmission In this version I have:

* Added reference describing the methods in the package to the description field of the DESCRIPTION file. The paper is currently in review; I will update the reference with the doi when it becomes available.

## Release summary
First submission of the package eSDM to CRAN

## Test environments
* Windows 10, R 3.6.1 (local)
* OS X, R 3.6.1 (local)
* ubuntu 14.04.5 (on travis-ci, R devel and release)

## R CMD check results
There were no ERRORs or WARNINGs

There was 1 NOTE (only when testing on local OS X):

* checking installed package size ... NOTE
    installed size is  9.2Mb
    sub-directories of 1Mb or more:
      doc     5.9Mb
      shiny   1.7Mb

  The size of the shiny sub-directory is primarily because of the (compressed) PDF manual for the shiny app. The vignette demonstrates an example analysis (case study) in our paper describing this package (Woodman et al., in review). 

## Downstream dependencies
N/A - first submission of eSDM
