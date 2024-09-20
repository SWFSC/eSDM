# eSDM

<!-- badges: start -->
[![CRAN version](https://www.r-pkg.org/badges/version/eSDM)](https://cran.r-project.org/package=eSDM)
[![R-CMD-check](https://github.com/SWFSC/eSDM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SWFSC/eSDM/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`eSDM` is an R package designed to allow users to create ensembles of predictions from species distribution models (SDMs) made at different spatial scales or with different prediction units. Included in the package is the eSDM GUI, an R Shiny tool that provides the user with a graphical user interface that they can use to import, overlay, and create ensembles of SDM predictions.

## Installation

You can install the released version of eSDM from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages('eSDM')
```

To install the latest version from [GitHub](https://github.com):

``` r
# install.packages('devtools')
devtools::install_github('smwoodman/eSDM', build_vignettes = TRUE)
```

## eSDM GUI

You can access the GUI online at <https://connect.fisheries.noaa.gov/eSDM>. You do not need to have R or RStudio installed to access the GUI online.

To run the GUI locally: install `eSDM` as described above, and then run the following code in your RStudio console:

``` r
eSDM::eSDM_GUI()
```

Depending on your internet connection, running the GUI locally may be faster than running it online. If text or images overlap within the GUI, please make the browser window full screen or adjust the text size in your browser (e.g., Ctrl - minus (‘-’) on Windows systems)

## Vignettes

``` r
# To see the list of available vignettes
browseVignettes("eSDM") 

# To open a specific vignette
vignette("example-analysis")
```

## Manuscript reference

The paper can be obtained [here](https://doi.org/10.1111/2041-210X.13283), and is cited as (preferred):

Woodman, S.M., Forney, K.A., Becker, E.A., DeAngelis, M.L., Hazen, E.L., Palacios, D.M., Redfern, J.V. (2019). *eSDM*: A tool for creating and exploring ensembles of predictions from species distribution and abundance models. *Methods Ecol Evol*. 2019;10:1923-1933. <doi:10.1111/2041-210X.13283>

For data used in the example analysis, see <https://github.com/smwoodman/eSDM-data>

For code used to create applicable figures from the manuscript: [Figure 2](https://github.com/swfsc/eSDM/blob/master/data-raw/figure2_overlay.R), [Figure 3](https://github.com/swfsc/eSDM/blob/master/data-raw/figure3.R), [Figure 4](https://github.com/swfsc/eSDM/blob/master/data-raw/figure4.R), [Figure 5](https://github.com/swfsc/eSDM/blob/master/data-raw/figure5.R)

## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

<img src="https://raw.githubusercontent.com/nmfs-fish-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" alt="NOAA Fisheries Logo" width="200" style="height: 75px !important;"/>

[U.S. Department of Commerce](https://www.commerce.gov/) \| [National Oceanographic and Atmospheric Administration](https://www.noaa.gov) \| [NOAA Fisheries](https://www.fisheries.noaa.gov/)
