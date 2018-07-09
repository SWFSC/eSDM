# eSDM

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

To run the eSDM GUI locally:

```r
eSDM::eSDM_GUI()
```
