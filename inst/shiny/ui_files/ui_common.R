# Lists, descriptions, and other text used multiple times in ui functions


###############################################################################
# https://github.com/daattali/advanced-shiny/blob/master/close-window/app.R
jscode <- "shinyjs.closeWindow = function() { window.close(); }"


###############################################################################
### File type lists for use in radioButton()'s and selectInupt()'s
#------------------------------------------------------------------------------
file.type.list1 <- list(
  "CSV (comma-separated value) file" = 1, "GIS shapefile" = 2,
  "GIS file geodatabase feature class" = 3
)
file.type.list2 <- list(
  "CSV (comma-separated value) file" = 1, "GIS raster" = 2,
  "GIS shapefile" = 3, "GIS file geodatabase feature class" = 4
)
file.type.list3 <- list(
  "CSV (comma-separated value) file" = 1, "GIS shapefile" = 2,
  "KML or KMZ file" = 3
)

preview.static.perc <- list("Percentages" = 1, "Values (numeric)" = 2)
preview.static.var <- list(
  "Plot predictions only" = 1, "Plot predictions and uncertainty" = 2
)
preview.download.dim <- list(
  "8 inches by 8 inches" = 1, "Dimensions of 'Static Preview' window" = 2
)


###############################################################################
# Widget labels used in importing

### CSV
label.csv.upload <- tags$h5("Upload CSV file (.csv extension)")

### GIS raster
label.raster.upload <- tags$h5("Upload raster file (.img or .tif extension)")

### GIS shp
label.shp.upload <- tags$h5("Upload GIS shapefile files")

### GIS gdb
label.gdb.path <- tags$h5("Full path to file geodatabase")
label.gdb.name <- tags$h5("Name of file geodatabase feature class")
label.gdb.upload <- "Upload feature class"


###############################################################################
# Instructions

###########################################################
### Instructions for uploading certain file and object types
ui.instructions.upload.csv <- function() {
  helpText(
    "Browse to and open the desired file with the extension '.csv'.",
    "This file must have headers. Note that larger files will take longer to upload."
  )
}

ui.instructions.upload.raster <- function() {
  helpText(
    "Browse to and open the desired file, which should have the extension '.img' or '.tif'.",
    "The raster can be in any coordinate system, but the raster coordinates must be",
    "between the equivalent of -180 and 180 decimal degrees.",
    "Note that larger files will take longer to upload."
  )
}

ui.instructions.upload.shp <- function() {
  helpText(
    "Browse to and open all files (.shp, .dbf, etc.) associated with",
    "the GIS shapefile. Note that larger files will take longer to upload."
  )
}

ui.instructions.upload.gdb <- function() {
  helpText(
    tags$h5(
      "Note that you can only import SDM predictions from a GIS file geodatabase feature class while",
      "running the GUI locally through RStudio.",
      style = "color: red"
    ),
    helpText(
      "Enter the full file path of the file geodatabase that contains the desired file geodatabase feature class.",
      "The path and the name of the feature class should be exactly as they appear in ArcCatalog.",
      "The GUI does not currently support importing a file geodatabase raster dataset or data from an ESRI personal geodatabase.",
      "Note that larger files will take longer to upload."
    )
  )
}


###########################################################
# Instructions for importing specific type of files

#######################################
### Importing predictions
ui.instructions.pred.csv <- function() {
  helpText(
    tags$em("Column with longitude data"), "and", tags$em("Column with latitude data:"),
    "Longitude and latitude points must be in WGS 84 geographic coordinates and equally spaced in decimal degrees.",
    tags$br(),
    tags$em("Data/value columns:"), "Please ensure that missing data values are one of the following:",
    "'NA', 'NaN', 'N/A', 'n/a', 'na', 'Null', blank, or a negative number.",
    tags$br(),
    tags$em("Prediction unit:"), "Select \"Relative density\" if the predictions are habitat likelihood",
    "(i.e. probabilities of occurrence).",
    tags$br(),
    tags$em("Column with uncertainty values:"), "Select \"N/A - No uncertainty values\" if the data",
    "does not have associated uncertainty values.",
    tags$br(),
    tags$em("Column with weight data:"), "Select \"N/A - No pixel-level spatial weight data\" if the data",
    "does not have weight data."
  )
}

ui.instructions.pred.raster <- function() {
  helpText(
    "Please ensure that missing prediction values are one of the following:",
    "'NA', 'NaN', 'N/A', 'n/a', 'na', 'NULL', blank, or a negative number.",
    tags$br(),
    tags$em("Prediction unit:"), "select \"Relative density\" if the predictions are probabilities of occurrence."
  )
}

ui.instructions.pred.shp.gdb <- function() {
  helpText(
    tags$em("Column with prediction data:"), "Please ensure that missing prediction values are one of the following:",
    "'NA', 'NaN', 'N/A', 'n/a', 'na', 'NULL', blank, or a negative number.",
    tags$br(),
    tags$em("Prediction unit:"), "Select \"Relative density\" if the predictions are probabilities of occurrence.",
    tags$br(),
    tags$em("Column with weight data:"), "Select \"N/A - No pixel-level spatial weight data\" if the data",
    "does not have weight data."
  )
}


#######################################
### Importing csv polygons
ui.instructions.poly.csv.single <- function() {
  helpText(
    "The first column must contain the longitude values, and the second column must contain the latitude values.",
    "The longitudes and latitudes must be in WGS 84 geographic coordinates and in the range [-180, 180]."
  )
}
ui.instructions.poly.csv <- function() {
  helpText(
    "The first column must contain the longitude values, and the second column must contain the latitude values.",
    "The longitudes and latitudes must be in WGS 84 geographic coordinates and in the range [-180, 180].",
    "Multiple polygons may be demarcated rows with blank cells or cells with 'NA' entries."
  )
}


###########################################################
ui.click.row.a1a <- paste(
  "Click on a row in the table below to select or deselect predictions."
)
ui.click.row.a2a <- paste(
  "Click on row(s) in the table below to select or deselect predictions."
)
ui.click.row.a1b <- paste(
  "Click on a row in the table(s) below to select or deselect predictions."
)
ui.click.row.a2b <- paste(
  "Click on row(s) in the table(s) below to select or deselect predictions."
)
### Instructions for selecting predictions from one table
ui.instructions.table.select <- function(text.pre, text.in, sel.num = 2, text.other = NULL) {
  tags$h5(tags$strong("Select", text.pre, "predictions", text.in),
          switch(sel.num, ui.click.row.a1a, ui.click.row.a2a),
          HTML(text.other))
}

### Instructions for selecting predictions from the o-o-e trio of tables
ui.instructions.multipletables.select <- function(text.in, sel.num = 2, text.other = NULL) {
  tags$h5(tags$strong("Select predictions to", text.in),
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
  "Error: Could not read GIS raster using the uploaded file and band number",
  style = "color: red"
)

### Shapefile
ui.error.upload.shp <- tags$strong(
  "Error: Could not read GIS shapefile using the uploaded files",
  style = "color: red"
)

### File gdb
ui.error.upload.gdb <- tags$strong(
  "Error: Could not read GIS file geodatabase feature class using the provided path and filename",
  style = "color: red"
)


###########################################################
# Message displayed if tab can't be used yet

### No original predictions have been imported
# ui.no.model.pred.loaded1
ui.notice.no.pred.original <- function() {
  box(
    width = 4,
    tags$h4("No original or overlaid predictions are available to use"),
    tags$h5("Please either import original predictions or load a saved workspace",
            "with original or overlaid predictions to use this section of the GUI")
  )
}

### No overlaid predictions have been created
# ui.no.model.pred.loaded2
ui.notice.no.pred.overlaid <- function(box.width = 4) {
  box(
    width = box.width,
    tags$h4("No overlaid predictions have been created"),
    tags$h5("Please either create overlaid predictions or load a saved workspace",
            "with overlaid predictions to use this section of the GUI")
  )
}

### No predictions have been imported/loaded/created
# ui.no.model.pred.loaded2
ui.notice.no.pred.general <- function(box.width = 4) {
  box(
    width = box.width,
    tags$h4("No predictions are available to use"),
    tags$h5("Please either import predictions or load a saved workspace",
            "with predictions to use this section of the GUI")
  )
}


###############################################################################
ui.instructions.ens.weightpolyNA <- function() {
  helpText(
    "The predictions that intersect with the uploaded polygon,",
    "with at least the specified overlap percentage,",
    "will be given values of NA and thus excluded from the ensemble."
  )
}

###############################################################################
