# eSDM

[![Build Status](https://travis-ci.org/smwoodman/eSDM.svg?branch=master)](https://travis-ci.org/smwoodman/eSDM)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/smwoodman/eSDM?branch=master&svg=true)](https://ci.appveyor.com/project/smwoodman/eSDM)


**NOTE: eSDM is currently under development. Please check with Sam (sam.woodman@noaa.gov) about the status of eSDM before using it.**

## Description

`eSDM` is an R package designed to allow users to create ensembles from predictions from species distribution models (SDMs) made at different spatial scales or with different prediction units. Included in the package is the eSDM GUI, an R Shiny tool that provides the user with a graphical user interface that they can use to import, overlay, and create ensembles from SDM predictions.

Although the best effort has been made to make the GitHub version of `eSDM` stable, the package is still under development and may contain errors. Please report any errors as issues.

## Install R package

To install the latest version from GitHub:

```r
if (!require('devtools')) install.packages('devtools')
# install from GitHub
devtools::install_github('smwoodman/eSDM')
```

## `eSDM` GUI

To access the GUI online without installing it locally, go to https://swoodman.shinyapps.io/eSDM/

To run the GUI locally, install `eSDM` as specified above and then run the following code in your RStudio console:

```r
eSDM::eSDM_GUI()
```

If text or images overlap within the GUI, please make the browser window full screen or adjust the text size in your browser (e.g. Ctrl - minus ('-') on Windows systems)
