### UI code for the 'Export Predictions' tab

ui.export <- function() {
  tabItem(tabName = "export",
          conditionalPanel(condition = "output.export_flag == false", ui.no.model.pred.loaded1()), 
          conditionalPanel(
            condition = "output.export_flag",
            fluidRow(
              box(
                title = "Select Predictions to Export", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE, 
                textOutput("export_text_none_loaded"),
                DT::dataTableOutput("export_table_orig_out"), br(),
                DT::dataTableOutput("export_table_over_out"), br(),
                DT::dataTableOutput("export_table_ens_out"),  br(), 
                helpText("Click on a row to select or deselect it")
              ), 
              box(
                title = "Export Predictions", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE, 
                conditionalPanel(
                  condition = "output.export_tables_oneselected_flag == false", 
                  strong("Please select exactly one set of predictions to export")
                ),
                conditionalPanel(
                  condition = "output.export_tables_oneselected_flag == true", 
                  fluidRow(
                    column(6, 
                           selectInput("export_format", h5("Format in which to export predictions"), 
                                       choices = list("Excel .csv file" = 1, "GIS shapefile" = 2, "KML file" = 3), 
                                       selected = 1), 
                           conditionalPanel(
                             condition = "input.export_format == 1", 
                             box(width = 12, 
                                 helpText("For predictions to be exported as an Excel .csv file, the centroid is determined for each polygon", 
                                          "that contains a prediction. The .csv file that is exported consists of", 
                                          "columns with the longitude and latitudes of these centroids,", 
                                          "as well as the prediction, error, and weight values for each of those points.")
                             )
                           ), 
                           conditionalPanel(
                             condition = "input.export_format == 2", 
                             box(width = 12, 
                                 helpText("Predictions will be exported as polygons with the prediction, error, and weight value for each polygon.", 
                                          br(), 
                                          "Predictions cannot be exported as GIS file or personal geodatabase files.")
                             )
                           ), 
                           conditionalPanel(
                             condition = "input.export_format == 3", 
                             checkboxInput("export_format_kml", "KML vs KMZ?", value = TRUE), 
                             box(width = 12, helpText("Info about exporting predictions in .kml"))
                           )
                    ), 
                    column(6, offset = 0, 
                           uiOutput("export_proj_uiOut_select"), 
                           box(width = 12, 
                               helpText("Predictions can be exported in WGS 84 geographic coordinates (lat/long, default),", 
                                        "or in the projection of one of the loaded sets of model predictions")
                           )
                    )
                  ), 
                  fluidRow(
                    column(6, uiOutput("export_filename_uiOut_text")), 
                    column(6, offset = 0, 
                           br(), br(), 
                           actionButton("export_out_execute", "Export predictions"), 
                           textOutput("export_out_text")
                    )
                  )
                )
              )
            )
          )
  )
}