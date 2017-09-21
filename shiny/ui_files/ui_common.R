### Lists and descriptions used multiple times in ui functions


###############################################################################
# Commonly used objects

###########################################################
### File type lists for use in radioButton()'s and selectInupt()'s
file.type.list1 <- list("Excel csv file" = 1, "GIS shapefile" = 2, 
                        "GIS file geodatabase feature class" = 3)
file.type.list2 <- list("Excel csv file" = 1, "GIS raster" = 2, 
                        "GIS shapefile" = 3, 
                        "GIS file geodatabase feature class" = 4)


###########################################################
# Labels of widgets used in loading file geodatabase feature classes

### 
label.gdb.path <- h5("Full path to file geodatabase")
label.gdb.name <- h5("Name of file geodatabase feature class")
label.gdb.upload <- "Upload file geodatabase feature class"


###############################################################################
# Text displayed multiple times in the app

###########################################################
ui.new.line <- function() helpText(HTML("<br/>")) # Just under 2x br() height


###########################################################
# Instructions

#######################################
### Instructions for loading model predictions
ui.load.data.instructions.raster <- function() {
  helpText("Please ensure that missing",
           "prediction values are one of the following: 'NA', 'NaN', 'N/A',", 
           "'n/a', 'na', blank, or a negative number.", br(), 
           em("Prediction value type:"), "select \"Relative density\" if", 
           "the predictions are probabilities of occurrence.")
}

ui.load.data.instructions <- function() {
  helpText(em("Column with prediciton data:"), "please ensure that missing",
           "prediction values are one of the following: 'NA', 'NaN', 'N/A',", 
           "'n/a', 'na', blank, or a negative number.", br(), 
           em("Prediction value type:"), "select \"Relative density\" if", 
           "the predictions are probabilities of occurrence.", br(), 
           em("Column with error/weight data:"), "select \"N/A\" if the", 
           "data does not have error/weight data.")
}


#######################################
### Instructions for loading certain file and object types
ui.csv.instructions <- function() {
  helpText("Browse to and load the file with the .csv extension that contains", 
           "the model prediction data. The longitude and latitude points", 
           "must be geographic coordinates that are equally spaced", 
           "in decimal degrees.")
}

ui.gis.raster.instructions <- function() {
  helpText("Browse to and load the TIFF file that has the extension '.tif.", 
           "The raster can be in any projection, but the raster coordinates", 
           "must be between the equivalent of -180 and 180 decimal degrees.")
}

ui.gis.shp.intructions <- function() {
  helpText("Browse to and select all files of the desired GIS shapefile.")
}

ui.gis.gdb.intructions <- function() {
  helpText("Enter the full file path of the file geodatabase that contains", 
           "the desired file geodatabase feature class. The path and the", 
           "name of the feature class should be", 
           "exactly as they appear in ArcCatalog.", 
           "This app does not currently support loading data from an", 
           "ESRI personal geodatabase.")
}

ui.csv.poly.instructions <- function() {
  helpText("Browse to and load the file with the .csv extension that", 
           "contains the desired polygon(s). The file must have headers,", 
           "the first column must contain the longitude values,", 
           "and the second column must contain the latitude values.", 
           "The longitudes and latitudes must be in geographic coordinates.", 
           "Multiple polygons may be demarcated", 
           "using blank cells or cells with 'NA' entries.", 
           "If the provided points don't form a closed polygon,", 
           "then the last point and first point are connected.", 
           "Please be aware that this could create an invalid polygon.")
}


###########################################################
### Message displayed if tab functionality can't be used yet
ui.no.model.pred.loaded1 <- function() {
  box(width = 4, 
      h4("No model predictions are loaded"), 
      h5("Please load model predictions to use this section of the app")
  )
}

ui.no.model.pred.loaded2 <- function(box.width = 4) {
  box(width = box.width, 
      h4("No overlaid model predictions have been created"), 
      h5("Please create overlaid model predictions", 
         "to use this section of the app")
  )
}

###############################################################################
