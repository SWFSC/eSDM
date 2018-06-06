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
              box(
                width = 6,
                tags$strong("1) Export options: file format"),
                selectInput("export_format", tags$h5("Format in which to export predictions"),
                            choices = list("Excel .csv file" = 1, "GIS shapefile" = 2, "KML or KMZ file" = 3),
                            selected = 1),
                column(
                  width = 12,
                  conditionalPanel(
                    condition = "input.export_format == 1",
                    helpText(tags$u("Description:"),
                             "The centroid is determined for each prediction polygon, and thus the exported .csv file consists of",
                             "columns with the coordinates of these centroids,",
                             "as well as the prediction (density), and weight values for each of those points.",
                             tags$br(), tags$br(),
                             tags$u("Filename:"), "Extension must be '.csv'.")
                  ),
                  conditionalPanel(
                    condition = "input.export_format == 2",
                    helpText(tags$u("Description:"),
                             "Predictions will be exported as polygons with the prediction (density),",
                             "and weight value for each polygon. The eSDM will produce a zip file with the name",
                             "'eSDM_shp_Export.zip' that will contain the various shapefile files.",
                             tags$br(), tags$br(),
                             tags$u("Filename:"), "Extension must be '.shp'.")

                  ),
                  conditionalPanel(
                    condition = "input.export_format == 3",
                    radioButtons("export_format_kml", NULL, choices = list("Export as KML" = 1, "Export as KMZ" = 2),
                                 selected = 2),
                    helpText(tags$u("Description:"),
                             "Within the kml or kmz file, predictions will be represented as polygons with a red outline.",
                             "Currently you cannot color-code the polygons by density value. The polygons will have their respective",
                             "prediction (density) and weight values as decriptions.",
                             tags$br(), tags$br(),
                             tags$u("Filename:"), "Extension must be '.kml' or '.kmz', depending on the",
                             tags$em("File format"), "selection.")
                  )
                )
              ),
              ########################################## Coordinate system
              box(
                width = 6,
                tags$strong("2) Export options: coordinate system"),
                helpText("Note that if you export predictions to an Excel .csv file in a longitude/latitude",
                         "coordinate system the eSDM calculates the centroids using the lat/long coordinates",
                         "and thus the centroids may not be geographically accurate."),
                checkboxInput("export_proj_native", "Export predictions in the native coordinate system of the selected SDM",
                              value = TRUE),
                conditionalPanel(
                  condition = "input.export_proj_native == false",
                  box(
                    width = 12,
                    radioButtons("export_proj_method", NULL, #tags$h5("Overlay coordinate system"),
                                 choices = list("Select SDM with desired coordinate system" = 1,
                                                "Enter numeric EPSG code" = 2,
                                                "Export predictions in WGS 84 geographic coordinates" = 3),
                                 selected = 1),
                    conditionalPanel("input.export_proj_method == 1", uiOutput("export_proj_sdm_uiOut_select")),
                    conditionalPanel(
                      condition = "input.export_proj_method == 2",
                      numericInput("export_proj_epsg", tags$h5("EPSG code"), value = 4326, step = 1)
                    )
                  )
                ),
                helpText("hi"),
                uiOutput("export_csv_ll_uiOut_check")
              )
            ),
            ########################################## Filename and export
            fluidRow(
              box(
                width = 6,
                tags$strong("3) Export options: filename"),
                uiOutput("export_filename_uiOut_text")
              ),
              box(
                width = 6,
                tags$strong("4) Export predictions"),
                tags$br(), tags$br(),
                uiOutput("export_out_uiOut_download")
              )
            )
          )
        )
      )
    )
  )
}
