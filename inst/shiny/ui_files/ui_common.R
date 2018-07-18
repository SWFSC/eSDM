### Lists, descriptions, and other text used multiple times in ui functions


###############################################################################
### File type lists for use in radioButton()'s and selectInupt()'s
file.type.list1 <- list(
  "Excel .csv" = 1, "GIS shapefile" = 2,
  "GIS file geodatabase feature class" = 3
)
file.type.list2 <- list(
  "Excel .csv" = 1, "GIS raster (GeoTIFF)" = 2, "GIS shapefile" = 3,
  "GIS file geodatabase feature class" = 4
)


###############################################################################
# Labels of widgets used in loading file geodatabase feature classes

### Excel csv
label.csv.upload <- tags$h5("Upload Excel .csv file (.csv extension)")

### GIS raster
label.raster.upload <- tags$h5("Upload raster GeoTIFF file (.tif extension)")

### GIS shp
label.shp.upload <- tags$h5("Upload GIS shapefile files")

### GIS gdb
label.gdb.path <- tags$h5("Full path to file geodatabase")
label.gdb.name <- tags$h5("Name of file geodatabase feature class")
label.gdb.upload <- "Upload file geodatabase feature class"


###############################################################################
# Instructions

###########################################################
### Instructions for uploading certain file and object types
ui.instructions.upload.csv <- function() {
  helpText("Browse to and open the desired file with the extension '.csv'.",
           "This file must have headers. Note that larger files will take longer to load.")
}

ui.instructions.upload.raster <- function() {
  helpText("Browse to and open the desired GeoTIFF file, which has the extension '.tif'.",
           "The raster can be in any coordinate system, but the raster coordinates must be",
           "between the equivalent of -180 and 180 decimal degrees.")
}

ui.instructions.upload.shp <- function() {
  helpText("Browse to and open all files (.shp, .dbf, etc.) associated with",
           "the GIS shapefile. Note that larger files will take longer to load.")
}

ui.instructions.upload.gdb <- function() {
  tags$h5(
    tags$span("Note that you can only load SDM predictions from a GIS file geodatabase feature class while",
              "running the eSDM locally through RStudio.",
              style = "color: red"),
    tags$br(),
    tags$br(),
    helpText("Enter the full file path of the file geodatabase that contains the desired file geodatabase feature class.",
             "The path and the name of the feature class should be exactly as they appear in ArcCatalog.",
             "The GUI does not currently support loading a file geodatabse raster dataset or data from an ESRI personal geodatabase.",
             "Note that larger files will take longer to load.")
  )
}


###########################################################
# Instructions for loading specific type of files

#######################################
### Loading model predictions
ui.instructions.pred.csv <- function() {
  helpText(tags$em("Column with longitude data"), "and", tags$em("Column with latitude data:"),
           "Longitude and latitude points must be in WGS 84 geographic coordinates and equally spaced in decimal degrees.",
           tags$br(),
           tags$em("Column with prediciton data:"), "Please ensure that missing prediction values are one of the following:",
           "'NA', 'NaN', 'N/A', 'n/a', 'na', 'Null', blank, or a negative number.",
           tags$br(),
           tags$em("Prediction value type:"), "Select \"Relative density\" if the predictions are probabilities of occurrence.",
           tags$br(),
           tags$em("Column with weight data:"), "Select \"N/A\" if the data does not have weight data.")
}

ui.instructions.pred.raster <- function() {
  helpText("Please ensure that missing prediction values are one of the following:",
           "'NA', 'NaN', 'N/A', 'n/a', 'na', 'NULL', blank, or a negative number.",
           tags$br(),
           tags$em("Prediction value type:"), "select \"Relative density\" if the predictions are probabilities of occurrence.")
}

ui.instructions.pred.shp.gdb <- function() {
  helpText(tags$em("Column with prediciton data:"), "Please ensure that missing prediction values are one of the following:",
           "'NA', 'NaN', 'N/A', 'n/a', 'na', 'NULL', blank, or a negative number.",
           tags$br(),
           tags$em("Prediction value type:"), "Select \"Relative density\" if the predictions are probabilities of occurrence.",
           tags$br(),
           tags$em("Column with weight data:"), "Select \"N/A\" if the data does not have weight data.")
}


#######################################
### Loading csv polygons
ui.instructions.poly.csv <- function() {
  helpText("The first column must contain the longitude values,  and the second column must contain the latitude values.",
           "The longitudes and latitudes must be in WGS 84 geographic coordinates and in the range [-180, 180].",
           "Multiple polygons may be demarcated rows with blank cells or cells with 'NA' entries.")
}


###########################################################
ui.click.row.a1a <- paste(
  "Click on a row in the table below to select or deselect model predictions."
)
ui.click.row.a2a <- paste(
  "Click on row(s) in the table below to select or deselect model predictions."
)
ui.click.row.a1b <- paste(
  "Click on a row in the table(s) below to select or deselect model predictions."
)
ui.click.row.a2b <- paste(
  "Click on row(s) in the table(s) below to select or deselect model predictions."
)
### Instructions for selecting model(s) from one table
ui.instructions.table.select <- function(text.pre, text.in, sel.num = 2, text.other = NULL) {
  tags$h5(tags$strong("Select", text.pre, "model predictions", text.in),
          switch(sel.num, ui.click.row.a1a, ui.click.row.a2a),
          HTML(text.other))
}

### Instructions for selecting model(s )from the o-o-e trio of tables
ui.instructions.multipletables.select <- function(text.in, sel.num = 2, text.other = NULL) {
  tags$h5(tags$strong("Select model predictions to", text.in),
          switch(sel.num, ui.click.row.a1b, ui.click.row.a2b),
          HTML(text.other))
}


###############################################################################
# Error/notification messages

###########################################################
# File upload error messages

### CSV
ui.error.upload.csv <- tags$strong(
  "Error: Please choose a file that has a .csv file extension",
  style = "color: red"
)

### Raster
ui.error.upload.raster <- tags$strong(
  "Error: Could not load GIS raster using the uploaded file and band number",
  style = "color: red"
)

### Shapefile
ui.error.upload.shp <- tags$strong(
  "Error: Could not load GIS shapefile using the uploaded files",
  style = "color: red"
)

### File gdb
ui.error.upload.gdb <- tags$strong(
  "Error: Could not load GIS file geodatabase feature class using the provided path and filename",
  style = "color: red"
)


###########################################################
# Message displayed if tab functionality can't be used yet

### No original model predicitons have been loaded
# ui.no.model.pred.loaded1
ui.notice.no.pred.original <- function() {
  box(
    width = 4,
    tags$h4("No model predictions are loaded"),
    tags$h5("Please load model predictions to use this section of the GUI")
  )
}

### No overlaid model predictions have been created
# ui.no.model.pred.loaded2
ui.notice.no.pred.overlaid <- function(box.width = 4) {
  box(
    width = box.width,
    tags$h4("No overlaid model predictions have been created"),
    tags$h5("Please create overlaid model predictions to use this section of the GUI")
  )
}


###############################################################################
ui.instructions.ens.weightpoly0 <- function() {
  helpText("A weight entry of '0' means that predictions within the weight polygon will be set to NA",
           "and thus will not be included in the ensemble")
}

###############################################################################
