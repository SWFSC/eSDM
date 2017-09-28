### Lists and descriptions used multiple times in ui functions


###############################################################################
### File type lists for use in radioButton()'s and selectInupt()'s
file.type.list1 <- list("Excel .csv" = 1, "GIS shapefile" = 2, 
                        "GIS file geodatabase feature class" = 3)
file.type.list2 <- list("Excel .csv" = 1, "GIS raster" = 2, 
                        "GIS shapefile" = 3, 
                        "GIS file geodatabase feature class" = 4)


###############################################################################
# Labels of widgets used in loading file geodatabase feature classes

### Excel csv
label.csv.upload <- h5("Upload Excel .csv file (.csv extension)")

### GIS raster
label.raster.upload <- h5("Upload raster file (.tif extension)")

### GIS shp
label.shp.upload <- h5("Upload GIS shapefile files")

### GIS gdb
label.gdb.path <- h5("Full path to file geodatabase")
label.gdb.name <- h5("Name of file geodatabase feature class")
label.gdb.upload <- "Upload file geodatabase feature class"


###############################################################################
# Instructions

###########################################################
### Instructions for uploading certain file and object types
ui.instructions.upload.csv <- function() {
  helpText("Browse to and open the desired file with the extension '.csv'.", 
           "This file must have headers.")
}

ui.instructions.upload.raster <- function() {
  helpText("Browse to and open the desired TIFF file that has the extension '.tif.", 
           "The raster can be in any projection, but the raster coordinates", 
           "must be between the equivalent of -180 and 180 decimal degrees.")
}

ui.instructions.upload.shp <- function() {
  helpText("Browse to and open all files of the desired GIS shapefile.")
}

ui.instructions.upload.gdb <- function() {
  helpText("Enter the full file path of the file geodatabase that contains", 
           "the desired file geodatabase feature class. The path and the", 
           "name of the feature class should be", 
           "exactly as they appear in ArcCatalog.", 
           "This app does not currently support loading a file geodatabse", 
           "raster dataset or data from an ESRI personal geodatabase.")
}


###########################################################
# Instructions for loading specific type of files

#######################################
### Loading model predictions
ui.instructions.pred.csv <- function() {
  helpText(em("Column with longitude data"), "and", 
           em("Column with latitude data:"), "Longitude and latitude", 
           "points must be geographic coordinates that are equally spaced", 
           "in decimal degrees.", 
           em("Column with prediciton data:"), "Please ensure that missing",
           "prediction values are one of the following: 'NA', 'NaN', 'N/A',", 
           "'n/a', 'na', blank, or a negative number.", br(), 
           em("Prediction value type:"), "Select \"Relative density\" if", 
           "the predictions are probabilities of occurrence.", br(), 
           em("Column with error/weight data:"), "Select \"N/A\" if the", 
           "data does not have error/weight data.")
}

ui.instructions.pred.raster <- function() {
  helpText("Please ensure that missing",
           "prediction values are one of the following: 'NA', 'NaN', 'N/A',", 
           "'n/a', 'na', blank, or a negative number.", br(), 
           em("Prediction value type:"), "select \"Relative density\" if", 
           "the predictions are probabilities of occurrence.")
}

ui.instructions.pred.shp.gdb <- function() {
  helpText(em("Column with prediciton data:"), "Please ensure that missing",
           "prediction values are one of the following: 'NA', 'NaN', 'N/A',", 
           "'n/a', 'na', blank, or a negative number.", br(), 
           em("Prediction value type:"), "Select \"Relative density\" if", 
           "the predictions are probabilities of occurrence.", br(), 
           em("Column with error/weight data:"), "Select \"N/A\" if the", 
           "data does not have error/weight data.")
}


#######################################
### Loading csv polygons
ui.instructions.poly.csv <- function() {
  helpText("The first column must contain the longitude values,", 
           "and the second column must contain the latitude values.", 
           "The longitudes and latitudes must be in geographic coordinates", 
           "in the range [-180, 180].", 
           "Multiple polygons may be demarcated", 
           "using blank cells or cells with 'NA' entries.", 
           "If the provided points do not form a closed polygon,", 
           "then the last point is connected to the first point.", 
           "Please be aware that this could create an invalid polygon.")
}


###############################################################################
# Error/notification messages

###########################################################
# File upload error messages

### CSV
ui.error.upload.csv <- strong("Please choose a file that has a .csv file extension")

### Raster
ui.error.upload.raster <- strong("Could not load GIS raster using the provided file and band number")

### Shapefile
ui.error.upload.shp <- strong("Could not load GIS shapefile using the provided files")

### File gdb
ui.error.upload.gdb <- strong("Could not load GIS file using the provided path and filename")


###########################################################
# Message displayed if tab functionality can't be used yet

### No original model predicitons have been loaded
# ui.no.model.pred.loaded1
ui.notice.no.pred.original <- function() {
  box(width = 4, 
      h4("No model predictions are loaded"), 
      h5("Please load model predictions to use this section of the app")
  )
}

### No overlaid model predictions have been created
# ui.no.model.pred.loaded2
ui.notice.no.pred.overlaid <- function(box.width = 4) {
  box(width = box.width, 
      h4("No overlaid model predictions have been created"), 
      h5("Please create overlaid model predictions", 
         "to use this section of the app")
  )
}


###############################################################################
### Other
ui.new.line <- function() helpText(HTML("<br/>")) # Just under 2x br() height

###############################################################################
