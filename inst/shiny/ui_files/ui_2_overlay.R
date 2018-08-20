### UI code for the 'Overlay Model Predictions' tab

ui.overlay <- function() {
  tabItem(
    tabName = "overlay",
    conditionalPanel("output.overlay_display_flag == false", ui.notice.no.pred.original()),
    conditionalPanel(
      condition = "output.overlay_display_flag",
      fluidRow(
        column(
          width = 4,
          fluidRow(
            box(
              title = "Import Study Area Polygon", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              checkboxInput("overlay_bound",
                            paste("Use a study area polygon as the boundary for the base geometry in the overlay process;",
                                  "uncheck this box to remove an imported study area polygon"),
                            value = FALSE),
              conditionalPanel(
                condition = "input.overlay_bound == true",
                column(12, radioButtons("overlay_bound_file_type", tags$h5("File type"), choices = file.type.list1, selected = 1)),
                box(
                  width = 12,
                  conditionalPanel(
                    condition = "input.overlay_bound_file_type == 1",
                    ui.instructions.upload.csv(),
                    ui.instructions.poly.csv(),
                    fluidRow(
                      column(6, fileInput("overlay_bound_csv_file", label.csv.upload, accept = ".csv")),
                      column(
                        width = 6,
                        tags$br(), tags$br(),
                        tags$span(textOutput("overlay_bound_csv_message"), style = "color:blue"),
                        textOutput("overlay_bound_csv_text")
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.overlay_bound_file_type == 2",
                    ui.instructions.upload.shp(),
                    fluidRow(
                      column(6, fileInput("overlay_bound_gis_shp_files", label.shp.upload, multiple = TRUE)),
                      column(
                        width = 6,
                        tags$br(),
                        tags$br(),
                        tags$span(textOutput("overlay_bound_gis_shp_message"), style = "color:blue"),
                        textOutput("overlay_bound_gis_shp_text")
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.overlay_bound_file_type == 3",
                    ui.instructions.upload.gdb(),
                    textInput("overlay_bound_gis_gdb_path", label.gdb.path, value = ".../folder.gdb"),
                    textInput("overlay_bound_gis_gdb_name", label.gdb.name, value = ""),
                    fluidRow(
                      column(7, actionButton("overlay_bound_gis_gdb_load", label.gdb.upload)),
                      column(
                        width = 5,
                        tags$span(textOutput("overlay_bound_gis_gdb_message"), style = "color:blue"),
                        textOutput("overlay_bound_gis_gdb_text")
                      )
                    )
                  )
                )
              )
            ),

            box(
              title = "Import Erasing Polygon", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              checkboxInput("overlay_land",
                            paste("Use an erasing polygon to remove are from the base geometry in the overlay process;",
                                  "uncheck this box to remove an imported erasing polygon"),
                            value = FALSE),
              conditionalPanel(
                condition = "input.overlay_land == true",
                column(
                  width = 12,
                  fluidRow(
                    column(6, radioButtons("overlay_land_load_type", NULL,
                                           choices = list("Use provided erasing polygon" = 1, "Upload personal erasing polygon" = 2),
                                           selected = 1)),
                    column(
                      width = 6,
                      conditionalPanel(
                        condition = "input.overlay_land_load_type == 2 ",
                        radioButtons("overlay_land_file_type", tags$h5("File type"), choices = file.type.list1, selected = 1)
                      )
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.overlay_land_load_type == 1",
                  box(
                    width = 12,
                    helpText("The provided erasing polygon is from the Global Self-consistent, Hierarchical, ",
                             "High-resolution Geography (GSHHG) Database. It is a low-resolution polygon that",
                             "represents the land of all continents, including Antarctica,",
                             "but not lakes, rivers, or islands within those continents.",
                             tags$br(),
                             "See the", tags$a("GSHHG website", href = "http://www.soest.hawaii.edu/pwessel/gshhg/"),
                             "for more information about the provided erasing polygon,",
                             "or to download polygons with higher resolutions."),
                    fluidRow(
                      column(6, actionButton("overlay_land_provided", "Import provided erasing polygon")),
                      column(
                        width = 6,
                        tags$span(textOutput("overlay_land_prov_message"), style = "color: blue"),
                        textOutput("overlay_land_prov_text")
                      )
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.overlay_land_load_type == 2 && input.overlay_land_file_type == 1",
                  box(
                    width = 12,
                    ui.instructions.upload.csv(),
                    ui.instructions.poly.csv(),
                    fluidRow(
                      column(6, fileInput("overlay_land_csv_file", label.csv.upload, accept = ".csv")),
                      column(
                        width = 6,
                        tags$br(), tags$br(),
                        tags$span(textOutput("overlay_land_csv_message"), style = "color: blue"),
                        textOutput("overlay_land_csv_text")
                      )
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.overlay_land_load_type == 2 && input.overlay_land_file_type == 2",
                  box(
                    width = 12,
                    ui.instructions.upload.shp(),
                    fluidRow(
                      column(6, fileInput("overlay_land_gis_shp_files", label.shp.upload, multiple = TRUE)),
                      column(
                        width = 6,
                        tags$br(), tags$br(),
                        tags$span(textOutput("overlay_land_gis_shp_message"), style = "color: blue"),
                        textOutput("overlay_land_gis_shp_text")
                      )
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.overlay_land_load_type == 2 && input.overlay_land_file_type == 3",
                  box(
                    width = 12,
                    ui.instructions.upload.gdb(),
                    textInput("overlay_land_gis_gdb_path", label.gdb.path, value = ".../folder.gdb"),
                    textInput("overlay_land_gis_gdb_name", label.gdb.name, value = ""),
                    fluidRow(
                      column(7, actionButton("overlay_land_gis_gdb_load", label.gdb.upload)),
                      column(
                        width = 5,
                        tags$span(textOutput("overlay_land_gis_gdb_message"), style = "color: blue"),
                        textOutput("overlay_land_gis_gdb_text")
                      )
                    )
                  )
                )
              )
            )
          )
        ),

        column(
          width = 8,
          fluidRow(
            box(
              title = "Imported Model Predictions", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              ui.instructions.table.select(text.pre = "original", text.in = "to use as the base geometry:", sel.num = 1),
              conditionalPanel("input.overlay_loaded_table_stats != true", DTOutput("overlay_loaded_table")),
              conditionalPanel("input.overlay_loaded_table_stats", DTOutput("overlay_loaded_stats_table")),
              column(12, checkboxInput("overlay_loaded_table_stats", paste("Display additional information - NOTE that you can only",
                                                                           "select or deselect a row when this box is unchecked")))
            )
          ),
          fluidRow(
            box(
              title = "Overlay Model Predictions", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              fluidRow(
                column(
                  width = 6,
                  fluidRow(
                    box(
                      width = 12,
                      tags$strong("1) Overlay options: study area and erasing polygons"),
                      tags$h5("Import these polygons in their respecitve boxes: 'Import Study Area Polygon' and 'Import Erasing Polygon'."),
                      tags$br(),
                      tags$strong("2) Overlay options: base geometry"),
                      tags$h5("Choose the base geometry in the 'Imported Model Predictions' box.")
                    ),
                    box(
                      width = 12,
                      tags$strong("3) Overlay options: coordinate system"),
                      helpText("The overlay process involves calculating the area of polygons and determining their intersection,",
                               "and thus the coordinate system during the overlay will have an effect on the overlay results."),
                      checkboxInput("overlay_proj_native",
                                    "Perform the overlay in the native coordinate system of the specified base geometry",
                                    value = TRUE),
                      conditionalPanel(
                        condition = "input.overlay_proj_native == false",
                        box(
                          width = 12,
                          radioButtons("overlay_proj_method", NULL, #tags$h5("Overlay coordinate system"),
                                       choices = list("Perform overlay in WGS 84 geographic coordinates" = 1,
                                                      "Select model with desired coordinate system" = 2,
                                                      "Enter numeric EPSG code" = 3),
                                       selected = 1),
                          conditionalPanel(
                            condition = "input.overlay_proj_method == 1",
                            helpText("When calculating area using WGS 84 geographic coordinates, the following assumptions are made:",
                                     "1) 'Equatorial axis of ellipsoid' = 6378137 and",
                                     "2) 'Inverse flattening of ellipsoid' = 1/298.257223563.", tags$br(),
                                     "See", tags$a("this article", href = "https://link.springer.com/article/10.1007%2Fs00190-012-0578-z"),
                                     "for more details about assumptions that must be made when calculating the area",
                                     "using WGS 84 geographic coordinates.")
                          ),
                          uiOutput("overlay_proj_sdm_uiOut_select"),
                          conditionalPanel(
                            condition = "input.overlay_proj_method == 3",
                            numericInput("overlay_proj_epsg", tags$h5("EPSG code"), value = 4326, step = 1),
                            helpText("See", tags$a("epsg.io", href = "http://epsg.io/"), "or the",
                                     tags$a("EPSG home page", href = "http://www.epsg.org/"),
                                     "for more information about EPSG codes")
                          )
                        )
                      )
                    )
                  )
                ),
                column(
                  width = 6,
                  fluidRow(
                    box(
                      width = 12,
                      tags$strong("4) Overlay options: percent overlap"),
                      helpText("Specify the minimum percentage of a base geometry polygon the must overlap with",
                               "original model prediction(s)",
                               "for that cell to have a non-NA overlaid prediction value.",
                               "A value of \"0\" means that cell will have a non-NA overlaid prediction value",
                               "if there is any overlap with any original model prediction."),
                      sliderInput("overlay_grid_coverage", label = NULL, min = 0, max = 100, value = 50)
                    ),
                    box(
                      width = 12,
                      tags$strong("5) Perform overlay"),
                      helpText(tags$strong("Reminder: imported study area and land polygons will be used during",
                                           "the overlay process. This process may take several minutes.")),
                      actionButton("overlay_create_overlaid_models_modal", "Overlay all predictions onto the specified base geometry"),
                      # actionButton("overlay_create_overlaid_models", "Overlay all predictions onto the specified base geometry"),
                      textOutput("overlay_overlay_all_text"),
                      tags$br(),
                      tags$span(uiOutput("overlay_overlaid_models_message"), style = "color: blue")
                    )
                  )
                )
              )
            )
          ),
          fluidRow(
            box(
              title = "Base Geometry and Overlaid Model Previews", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
              fluidRow(
                column(3, radioButtons("overlay_preview_which", NULL,
                                       choices = list("Base geometry preview" = 1, "Overlaid models preview" = 2))),
                column(
                  width = 9,
                  fluidRow(
                    box(
                      width = 12,
                      conditionalPanel(
                        condition = "input.overlay_preview_which == 1",
                        helpText("The base geometry will be outlined in black while if applicable the land and study area",
                                 "will be filled in tan and outlined in red, respectively.",
                                 "Note that if model predictions were made at a high resolution,",
                                 "then preview may appear to be completely black when zoomed out"),
                        uiOutput("overlay_preview_base_execute_uiOut_button"),
                        textOutput("overlay_preview_base_create_text")
                      ),
                      conditionalPanel(
                        condition = "input.overlay_preview_which == 2",
                        fluidRow(
                          column(6, uiOutput("overlay_preview_overlaid_models_uiOut_selectize")),
                          column(4, offset = 1, radioButtons("overlay_preview_overlaid_models_perc", tags$h5("Units"),
                                                             choices = list("Percentages" = 1, "Values" = 2), selected = 1))
                        ),
                        uiOutput("overlay_preview_overlaid_execute_uiOut_button")
                      )
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.overlay_preview_which == 1",
                shinycssloaders::withSpinner(leaflet::leafletOutput("overlay_preview_base"), type = 1)
              ),
              conditionalPanel(
                condition = "input.overlay_preview_which == 2",
                shinycssloaders::withSpinner(plotOutput("overlay_preview_overlaid"), type = 1)
              )
            )
          )
        )
      )
    )
  )
}
