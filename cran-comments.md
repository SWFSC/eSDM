## Release summary
* Updated citation with doi, etc. due to paper being published
* Updated vignette with proper citation information
* Several bug fixes (documented in NEWS.md)
* Improved documentation and messages in the GUI (shiny app)

## Test environments
* Windows 10, R 3.6.1 (local)
* OS X, R 3.6.1 (local)
* ubuntu 14.04.5 (on travis-ci, R devel and release)

## R CMD check results
There were no ERRORs or WARNINGs

There was 1 NOTE (only when testing on local OS X):

* checking installed package size ... NOTE
    installed size is  9.4Mb
    sub-directories of 1Mb or more:
      doc     5.9Mb
      shiny   1.9Mb

  The size of the shiny sub-directory is primarily because of the PDF manual for the shiny app. The vignette demonstrates an example analysis (case study) in our paper describing this package (Woodman et al. 2019). 

## Downstream dependencies
No downstream dependencies
