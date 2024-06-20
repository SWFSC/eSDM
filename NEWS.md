# eSDM 0.4.2.9000

* Moved repo to https://github.com/swfsc, and updated all associated documentation

* Corrected license files (still GPL3 + [addendum](https://github.com/SWFSC/eSDM/blob/main/LICENSE)), based on https://nmfs-opensci.github.io/GitHub-Guide



# eSDM 0.4.1

* Updated shiny app URL to https://connect.fisheries.noaa.gov/eSDM/


# eSDM 0.4.0

* Removed `maptools` as a dependency (#13). This Import was a relic as `eSDM` did not actually use `maptools` functions, so no functionality should change.

* eSDM depends on R >= 4.0.0


# eSDM 0.3.7

* Updated `overlay_sdm` to stay current with `sf` 1.0 (#12)


# eSDM 0.3.6

* Fixed a bug to stay current with the `shiny` package (not using the `%OR%` function) - no changes to user functionality


# eSDM 0.3.5

* Use `==` rather than `identical` to check CRS equivalence (#10)

* Fixed bug in the GUI where static previews downloaded with "Dimensions of 'Static Preview' window" did not have exact same dimensions as the window


# eSDM 0.3.4

* Fixed bug in eSDM GUI - invalid error message printed if packages not installed


# eSDM 0.3.3

* Import `st_make_valid` from `sf` rather than `lwgeom` (#9)

* Use `shinybusy` rather than `shinycssloaders` in eSDM GUI because of unknown error

* Move packages only used in the GUI (shiny app) to Suggests


# eSDM 0.3.2

* Updated citation details with MEE issue and page numbers

* Updated vignette formatting 


# eSDM 0.3.1

* Fixed bug in eSDM GUI 'update map range' button

* Fixed bug in evaluation_metrics when validation data contains NA values

* When subsetting for a single column, replaced `[,]` with `[[]]` so both data frames and tibbles return a vector

* Improved documentation and messages in GUI

* Added eSDM paper citation details (doi, etc) where applicable


# eSDM 0.3.0

* Updated citation to 'in press' in Methods in Ecology and Evolution

* Added button to download manuscript example analysis data through the GUI

* Added ability to have tick marks on high quality maps (`tmap` update)

* Explicitly call `dplyr::select`, in case it is masked by `raster::select`

* Fixed bug when exporting predictions from the GUI as a shapefile (now use `zip` package)

* Fixed bug when downloading evaluation metrics from the GUI

* Now call all `addMouseCoordinates` from `leafem` rather than `mapview`


# eSDM 0.2.1

* Initial release
