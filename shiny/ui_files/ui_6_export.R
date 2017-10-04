### UI code for the 'Export Predictions' tab

ui.export <- function() {
  tabItem(
    tabName = "export", 
    conditionalPanel("output.export_flag == false", ui.notice.no.pred.original()), 
    conditionalPanel(
      condition = "output.export_flag", 
      fluidRow(
        box(
          title = "Select Predictions to Export", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE, 
          ui.instructions.multipletables.select(text.in = "export:", sel.num = 1), 
          DT::dataTableOutput("export_table_orig_out"), 
          tags$br(), 
          DT::dataTableOutput("export_table_over_out"), 
          tags$br(), 
          DT::dataTableOutput("export_table_ens_out")
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
              column(
                width = 6, 
                selectInput("export_format", tags$h5("Format in which to export predictions"), 
                            choices = list("Excel .csv file" = 1, "GIS shapefile" = 2, "KML file" = 3), 
                            selected = 1), 
                conditionalPanel(
                  condition = "input.export_format == 1", 
                  box(
                    width = 12, 
                    helpText("For predictions to be exported as an Excel .csv file, the centroid is determined for each polygon", 
                             "that contains a prediction. The .csv file that is exported consists of", 
                             "columns with the longitude and latitudes of these centroids,", 
                             "as well as the prediction, and weight values for each of those points.")
                  )
                ), 
                conditionalPanel(
                  condition = "input.export_format == 2", 
                  box(
                    width = 12, 
                    helpText("Predictions will be exported as polygons with the prediction,", 
                             "and weight value for each polygon.", tags$br(), 
                             "Predictions cannot be exported to a GIS file or personal geodatabase.")
                  )
                ), 
                conditionalPanel(
                  condition = "input.export_format == 3", 
                  checkboxInput("export_format_kml", "KML vs KMZ?", value = TRUE), 
                  box(width = 12, helpText("Info about exporting predictions in .kml"))
                )
              ), 
              column(
                width = 6, offset = 0, 
                uiOutput("export_proj_uiOut_select"), 
                box(
                  width = 12, 
                  helpText("Predictions can be exported in WGS 84 geographic coordinates (lat/long, default), ", 
                           "or in the projection of one of the loaded sets of model predictions")
                )
              )
            ), 
            fluidRow(
              column(6, uiOutput("export_filename_uiOut_text")), 
              column(
                width = 6, offset = 0, 
                tags$br(), 
                tags$br(), 
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