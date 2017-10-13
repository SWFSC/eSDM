### UI code for the 'Export Predictions' tab

ui.export <- function() {
  tabItem(
    tabName = "export", 
    conditionalPanel("output.export_flag == false", ui.notice.no.pred.original()), 
    conditionalPanel(
      condition = "output.export_flag", 
      fluidRow(
        #############################################################
        box(
          title = "Select Predictions to Export", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE, 
          ui.instructions.multipletables.select(text.in = "export:", sel.num = 1), 
          DT::dataTableOutput("export_table_orig_out"), 
          tags$br(), 
          DT::dataTableOutput("export_table_over_out"), 
          tags$br(), 
          DT::dataTableOutput("export_table_ens_out")
        ), 
        #############################################################
        box(
          title = "Export Predictions", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE, 
          conditionalPanel(
            condition = "output.export_tables_oneselected_flag == false", 
            tags$span(tags$strong("Please select exactly one set of predictions to export"), style = "color: red")
          ), 
          conditionalPanel(
            condition = "output.export_tables_oneselected_flag", 
            fluidRow(
              ########################################## Exported file format
              column(
                width = 6, 
                selectInput("export_format", tags$h5("Format in which to export predictions"), 
                            choices = list("Excel .csv file" = 1, "GIS shapefile" = 2, "KML or KMZ file" = 3), 
                            selected = 1), 
                # conditionalPanel(
                #   condition = "input.export_format == 3", 
                #   radioButtons("export_format_kml", tags$h5("File type"), 
                #                choices = list("KML" = 1, "KMZ" = 2), 
                #                selected = 2)
                # ), 
                box(
                  width = 12, 
                  conditionalPanel(
                    condition = "input.export_format == 1", 
                    helpText(tags$u("Description:"), 
                             "For predictions to be exported as an Excel .csv file, the centroid is determined for", 
                             "each prediction polygon. The exported .csv file consists of", 
                             "columns with the longitude and latitudes of these centroids,", 
                             "as well as the prediction, and weight values for each of those points.", 
                             tags$br(), 
                             tags$u("Filename:"), "Extension must be '.csv'.")
                    
                  ), 
                  conditionalPanel(
                    condition = "input.export_format == 2", 
                    helpText(tags$u("Description:"), 
                             "Predictions will be exported as polygons with the prediction,", 
                             "and weight value for each polygon. The eSDM will produce a zip file with the name", 
                             "‘eSDM_shp_Export.zip’ that will contain the various shapefile files.", 
                             tags$br(), 
                             tags$u("Filename:"), "Extension must be '.shp'.")
                    
                  ), 
                  conditionalPanel(
                    condition = "input.export_format == 3", 
                    radioButtons("export_format_kml", tags$h5("File type"), 
                                 choices = list("KML" = 1, "KMZ" = 2), 
                                 selected = 2), 
                    helpText(tags$u("Description:"), 
                             "Within the kml or kmz file, predictions will be represented as polygons with a red outline.", 
                             "Currently you cannot color-code the polygons by density value. The polygons will have their respective", 
                             "prediction and weight values as decriptions.", 
                             tags$br(), 
                             tags$u("Filename:"), "Extension must be '.kml' or '.kmz', depending on the", 
                             tags$em("File type"), "selection.")
                  )
                )
              ), 
              ########################################## Coordinate system
              column(
                width = 6, offset = 0, 
                checkboxInput("export_proj_ll", "Export predictions in WGS 84 geographic coordinates", value = TRUE), 
                conditionalPanel(
                  condition = "input.export_proj_ll", 
                  helpText("Predictions will be exported in WGS 84 geographic coordinates")
                ), 
                conditionalPanel(
                  condition = "input.export_proj_ll == false", 
                  column(12, uiOutput("export_proj_uiOut_select"))
                ), 
                box(
                  width = 12,
                  helpText(tags$u("Description:"),
                           "Predictions can be exported in WGS 84 geographic coordinates (lat/long, default), ",
                           "or in the projection of one of the loaded sets of model predictions")
                )
              )
            ), 
            ########################################## Filename and export
            fluidRow(
              column(6, uiOutput("export_filename_uiOut_text")), 
              column(
                width = 6, offset = 0, 
                tags$br(), 
                tags$br(), 
                conditionalPanel(
                  condition = "output.export_filename_flag == false", 
                  tags$span(tags$strong("Error: The file extension in", tags$em("Filename"), 
                                        "must match the file extension specified in", 
                                        tags$em("Format in which to export predictions"), "to download the predictions"), 
                            style = "color: red")
                ), 
                conditionalPanel(
                  condition = "output.export_filename_flag", 
                  downloadButton("export_out", "Export predictions")
                  # conditionalPanel(
                  #   condition = "input.export_format == 2", 
                  #   textInput("export_out_shp_dsn", tags$h5("Path (including name) of shapefile files"), value = "")
                  #   actionButton("export_out_shp_execute", "Export predictions to shapefile")
                  # ), 
                  # actionButton("export_out_shp_execute", "Export predictions"), 
                  # textOutput("export_out_shp_text")
                )
              )
            )
          )
        )
      )
    )
  )
}