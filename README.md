# eSDM

## Description

`eSDM` is a R package designed to allow users to create ensembles from species distribution model (SDM) predictions made at different spatial scales or with different units. Included in the package is a R Shiny tool that provides the user with a user interface where they can load, overlay, and create ensembles from their SDM predictions.

## Installation

To install the latest version from GitHub:

```r
if (!require('devtools')) install.packages('devtools')
# install from GitHub
devtools::install_github('smwoodman/eSDM')
```

## How to use

To run the eSDM R Shiny tool locally:

```r
eSDM::eSDM_GUI()
```
