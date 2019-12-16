## eSDM 0.3.2
* Updated citation details with MEE issue and page numbers
* Updated vignette formatting 

## eSDM 0.3.1
* Fixed bug in eSDM GUI 'update map range' button
* Fixed bug in evaluation_metrics when validation data contains NA values
* When subsetting for a single column, replaced `[,]` with `[[]]` so both data frames and tibbles return a vector
* Improved documentation and messages in GUI
* Added eSDM paper citation details (doi, etc) where applicable

## eSDM 0.3.0
* Updated citation to 'in press' in Methods in Ecology and Evolution
* Added button to download manuscript example analysis data through the GUI
* Added ability to have tick marks on high quality maps (`tmap` update)
* Explicitly call `dplyr::select`, in case it is masked by `raster::select`
* Fixed bug when exporting predictions from the GUI as a shapefile (now use `zip` package)
* Fixed bug when downloading evaluation metrics from the GUI
* Now call all `addMouseCoordinates` from `leafem` rather than `mapview`

## eSDM 0.2.1
* Initial release
