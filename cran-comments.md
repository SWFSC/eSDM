## Resubmission
This is a resubmission. In this version I have:
* Addressed the errors identified in the CRAN pre-tests, including fixing the evaluation metrics test
* Responded to the notes/warnings on the check page by moving packages only used by the shiny app from 'Imports' to 'Suggests'
  
  
## Release summary
* Import st_make_valid from sf rather than lwgeom, as per message from sf author
* Fixed the shiny app as necessary due to other package updates

## Test environments
* Windows 10, R 3.6.3 (local)
* OS X, R 3.6.3 (local)
* ubuntu 14.04.5 (on travis-ci.com, R devel and release)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTES

## Downstream dependencies
No downstream dependencies
