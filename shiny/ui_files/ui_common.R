### ui_common
## Lists and descriptions used multiple times in ui functions
# By Sam Woodman


###############################################################################
# Commonly used objects

#######################################
### File type lists for use in radioButton()'s and selectInupt()'s
file.type.list1 <- list("Excel csv file" = 1, "GIS shapefile" = 2, 
                        "GIS file geodatabase (.gdb) file" = 3)
file.type.list2 <- list("Excel csv file" = 1, "GIS raster" = 2, 
                        "GIS shapefile" = 3, 
                        "GIS file geodatabase (.gdb) file" = 4)


###############################################################################
# helpText() code used multiple times

#######################################
ui.new.line <- function() helpText(HTML("<br/>")) # Just under 2x br() width


#######################################
### Instructions for loading certain file types
ui.csv.instructions <- function() {
  helpText("CSV model prediction data must be geographic coordinates",
           "(latitude/longitude points) that are equally spaced", 
           "in decimal degrees.", br(), 
           "Select the names of the longitude, latitude, and other applicable", 
           "columns after uploading the .csv file,", 
           "and then click the button to load model predictions")
}

ui.gis.raster.instructions <- function() {
  helpText("For a raster, load the file that has the extension '.tif", 
           "and thus is a TIFF file.", br(), 
           "The raster can be in any projection, but the raster coordinates", 
           "must be between the equivalent of -180 and 180 decimal degrees")
}

ui.gis.shp.intructions <- function() 
  helpText("Select all files of desired GIS shapefile")

ui.gis.gdb.intructions <- function() {
  helpText("Enter the full file path up to and including .gdb folder.", 
           "The name of the file should be as it appears in ArcCatalog.", br(), 
           "This app does not currently support loading files from an ", 
           "ESRI personal geodatabase")
}

ui.load.data.instructions <- function() {
  helpText("Please ensure that missing prediction values are one of the",
           "following: 'NA', 'NaN', 'N/A', 'n/a', 'na', blank,", 
           "or a negative number.", br(), 
           "For 'Prediction value type',",
           "select 'Relative' if predictions are probabilities of occurrence")
}

ui.csv.poly.instructions <- function() {
  helpText("The .csv file must have headers, the first column must be the", 
           "longitude values, and the second column must be the", 
           "latitude values. The longitudes and latitudes must be in", 
           "geographic coordinates. Multiple polygons can be demarcated", 
           "using blank cells or cells with 'NA'.", br(), 
           "If the provided points don't form a closed polygon,", 
           "then the last point and first point are connected, which",
           "could create an invalid polygon.")
}


#######################################
### Message displayed if tab functionality can't be used yet
ui.no.model.pred.loaded1 <- function() {
  box(width = 4, 
      h4("No model predictions are loaded"), 
      h5("Please load model predictions to use this section of the app")
  )
}

ui.no.model.pred.loaded2 <- function(box.width = 4) {
  box(width = box.width, 
      h4("Overlaid model predictions have not been created"), 
      h5("Please create overlaid model predictions", 
         "to use this section of the app")
  )
}

###############################################################################
