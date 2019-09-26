### UI code for the 'Export Predictions' tab

ui.export <- function() {
  tabItem(
    tabName = "export",
    conditionalPanel("output.export_flag == false", ui.notice.no.pred.general()),
    conditionalPanel(
      condition = "output.export_flag",
      fluidRow(
        #############################################################
        box(
          title = "Select Predictions to Export", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
          ui.instructions.multipletables.select(text.in = "export:", sel.num = 1),
          DTOutput("export_table_orig_out"),
          tags$br(),
          DTOutput("export_table_over_out"),
          tags$br(),
          DTOutput("export_table_ens_out")
        ),
        #############################################################
        box(
          title = "Export Predictions", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
          conditionalPanel(
            condition = "output.export_tables_onesel_flag == false",
            tags$span(tags$strong("Please select exactly one set of predictions to export"), style = "color: red")
          ),
          conditionalPanel(
            condition = "output.export_tables_onesel_flag",
            fluidRow(
              ########################################## Exported file format
              box(
                width = 6,
                tags$strong("1) Export options: file format"),
                selectInput("export_format", tags$h5("Format in which to export predictions"),
                            choices = file.type.list3, selected = 1),
                column(
                  width = 12,
                  conditionalPanel(
                    condition = "input.export_format == 1",
                    helpText(tags$u("Description:"),
                             "The centroid is determined for each prediction polygon, and thus the exported .csv file consists of",
                             "columns with the coordinates of these centroids,",
                             "as well as the prediction (density), SE, and weight values for each of those points.",
                             tags$br(), tags$br(),
                             tags$u("Filename:"), "Extension of the downloaded file will be '.csv'.")
                  ),
                  conditionalPanel(
                    condition = "input.export_format == 2",
                    helpText(tags$u("Description:"),
                             "Predictions will be exported as polygons with the prediction (density), SE,",
                             "and weight values for each polygon. The eSDM will produce a zip file with the name",
                             "'eSDM_shp_Export.zip' that will contain the various shapefile files.",
                             tags$br(), tags$br(),
                             tags$u("Filename:"), "Extension of the downloaded file will be '.zip',",
                             "and within that folder there will be four files",
                             "with the following extensions: '.dbf', '.prj', '.shp', and '.shx'.")

                  ),
                  conditionalPanel(
                    condition = "input.export_format == 3",
                    radioButtons("export_format_kml", NULL, choices = list("Export as KML" = 1, "Export as KMZ" = 2),
                                 selected = 2),
                    helpText(tags$u("Description:"),
                             "Within the KML or KMZ file, predictions will be represented as polygons with a red outline.",
                             "Currently you cannot color-code the polygons by density value. The polygons will have their respective",
                             "prediction (density), SE, and weight values as descriptions",
                             tags$br(), tags$br(),
                             tags$u("Filename:"), "Extension of the downloaded file will be '.kml' if \"Export as KML\"",
                             "is selected and '.kmz' if \"Export as KMZ\" is selected.")
                  )
                )
              ),
              ########################################## Coordinate system
              box(
                width = 6,
                tags$strong("2) Export options: coordinate system"),
                tags$hr(style = "border-color: black;"), #-------------------------------------
                helpText("Note that if you export predictions to a CSV file in a longitude/latitude",
                         "coordinate system, the eSDM assumes the longitude/latitude coordinates are planar",
                         "and thus the centroids may not be geographically accurate. See",
                         tags$a("this link", href = "https://github.com/r-spatial/sf/issues/493"), "for more information"),
                checkboxInput("export_proj_native", "Export predictions in the native coordinate system of the selected SDM",
                              value = TRUE),
                conditionalPanel(
                  condition = "input.export_proj_native == false",
                  radioButtons("export_proj_method", NULL,
                               choices = list("Export predictions in WGS 84 geographic coordinates" = 1,
                                              "Select predictions with desired coordinate system" = 2,
                                              "Enter numeric EPSG code" = 3),
                               selected = 1),
                  uiOutput("export_proj_sdm_uiOut_select"),
                  conditionalPanel(
                    condition = "input.export_proj_method == 3",
                    numericInput("export_proj_epsg", tags$h5("EPSG code"), value = 4326)
                  )
                ),
                tags$hr(style = "border-color: black;"), #-------------------------------------
                conditionalPanel(
                  condition = "output.export_range360_flag && input.export_proj_360 == false && input.export_format == 1",
                  tags$h5("Warning: Centroids for polygons that span the international dateline will not be",
                          "geographically accurate or meaningful in this longitude range",
                          style = "color: red;")
                ),
                checkboxInput("export_proj_360",
                              paste("Export predictions with longitude coordinates in a range equivalent to",
                                    "[0, 360] decimal degrees rather than [-180, 180] decimal degrees"),
                              value = FALSE),
                conditionalPanel(
                  condition = "input.export_proj_360 && input.export_format == 1",
                  column(12, helpText("It is recommended use the 'native' or 'WGS 84' coordinate systems when using this feature."))
                ),
                conditionalPanel(
                  condition = "input.export_proj_360 && input.export_format != 1",
                  column(
                    width = 12,
                    helpText("It is recommended use the 'native' or 'WGS 84' coordinate systems when using this feature.",
                             "Prediction polygons that span the dateline will be multipart polygons split along the dateline.")
                  )
                )
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
                uiOutput("export_weight_inc_uiOut_text"),
                tags$br(),
                uiOutput("export_out_uiOut_download")
              )
            )
          )
        )
      )
    )
  )
}
