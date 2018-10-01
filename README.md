# eSDM

[![Build Status](https://travis-ci.org/smwoodman/eSDM.svg?branch=master)](https://travis-ci.org/smwoodman/eSDM)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/smwoodman/eSDM?branch=master&svg=true)](https://ci.appveyor.com/project/smwoodman/eSDM)


## Description

`eSDM` is an R package designed to allow users to create ensembles predictions using predictions from species distribution models (SDMs) made at different spatial scales or with different units. Included in the package is the eSDM GUI, an R Shiny tool that provides the user with a graphical user interface that they can use to load, overlay, and create ensembles from SDM predictions.

## Installation

To install the latest version from GitHub:

```r
if (!require('devtools')) install.packages('devtools')
# install from GitHub
devtools::install_github('smwoodman/eSDM')
```

## How to use

To access the GUI online, go to https://swoodman.shinyapps.io/eSDM/

To run the eSDM GUI locally, after installing the eSDM package run the following code in your RStudio console:

```r
eSDM::eSDM_GUI()
```
