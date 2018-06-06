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
              title = "Load Study Area Polygon", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              checkboxInput("overlay_bound",
                            paste("Use a study area polygon as the boundary for the base grid in the overlay process;",
                                  "uncheck this box to remove a loaded study area polygon"),
                            value = FALSE),
              conditionalPanel(
                condition = "input.overlay_bound == true",
                radioButtons("overlay_bound_file_type", tags$h5("File type"), choices = file.type.list1, selected = 1),
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
              title = "Load Land Polygon", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              checkboxInput("overlay_land",
                            paste("Use a land polygon to remove land from the base grid in the overlay process;",
                                  "uncheck this box to remove a loaded land polygon"),
                            value = FALSE),
              conditionalPanel(
                condition = "input.overlay_land == true",
                fluidRow(
                  column(6, radioButtons("overlay_land_load_type", tags$h5("Land polygon source"),
                                         choices = list("Use provided" = 1, "Upload personal" = 2),
                                         selected = 1)),
                  column(
                    width = 6,
                    conditionalPanel(
                      condition = "input.overlay_land_load_type == 2 ",
                      radioButtons("overlay_land_file_type", tags$h5("File type"), choices = file.type.list1, selected = 1)
                    )
                  )
                ),
                conditionalPanel(
                  condition = "input.overlay_land_load_type == 1",
                  box(
                    width = 12,
                    helpText("The provided land polygons are from the Global Self-consistent, Hierarchical, ",
                             "High-resolution Geography (GSHHG) Database. They represent all continents, including Antarctica,",
                             "but not lakes and rivers within those continents.",
                             "See the", tags$a("GSHHG website", href = "http://www.soest.hawaii.edu/pwessel/gshhg/"),
                             "for more information about the provided land polygons.", tags$br(),
                             "Note that higher resolution polygons will take longer to load."),
                    fluidRow(
                      column(6, selectInput("overlay_land_provided_res", tags$h5("Resolution of land polygon"),
                                            choices = list("Full" = 1, "High" = 2, "Intermediate" = 3, "Low" = 4, "Crude" = 5),
                                            selected = 1)),
                      column(6, ui.new.line(), tags$br(), actionButton("overlay_land_provided", "Load provided land polygon"))
                    ),
                    tags$span(textOutput("overlay_land_prov_message"), style = "color: blue"),
                    textOutput("overlay_land_prov_text")
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
                        tags$br(),
                        tags$br(),
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
                        tags$br(),
                        tags$br(),
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
              title = "Loaded Model Predictions", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              ui.instructions.table.select(text.pre = "loaded", text.in = "to use as the base grid:", sel.num = 1,
                                           text.other = TRUE),
              conditionalPanel("input.overlay_loaded_table_stats != true", DT::dataTableOutput("overlay_loaded_table")),
              conditionalPanel("input.overlay_loaded_table_stats", DT::dataTableOutput("overlay_loaded_stats_table")),
              column(12, checkboxInput("overlay_loaded_table_stats", "Display additional information"))
            )
          ),
          fluidRow(
            box(
              title = "Overlay Model Predictions", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              fluidRow(
                box(
                  width = 6,
                  tags$strong("1) Overlay options: coordinate system"),
                  helpText("The overlay process involves calculating the area of polygons and determining their intersection,",
                           "and thus the coordinate system during the overlay will have an effect on the overlay results."),
                  checkboxInput("overlay_proj_native",
                                "Perform the overlay in the native coordinate system of the specified base grid",
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
                        helpText("See", tags$a("epsg.io", href = "http://epsg.io/"), "or",
                                 tags$a("EPSG home", href = "http://www.epsg.org/"),
                                 "for more information and EPSG codes")
                      )
                    )
                  )
                ),
                column(
                  width = 6,
                  fluidRow(
                    box(
                      width = 12,
                      tags$strong("2) Overlay options: percent overlap"),
                      helpText("Specify what percentage of a base grid cell must be overlapped by",
                               "the original model prediction(s) for that cell to have a non-NA overlaid prediction value.",
                               "A value of \"0\" means that cell will have a non-NA overlaid prediction value",
                               "if there is any overlap with any original model prediction."),
                      sliderInput("overlay_grid_coverage", label = NULL, min = 0, max = 100, value = 50)
                    ),
                    box(
                      width = 12,
                      tags$strong("3) Perform overlay"),
                      helpText(tags$strong("It is strongly recommended to save the app environment before overlaying",
                                           "in case you are disconnected from the server during the process.")),
                      helpText(tags$strong("Reminder: loaded study area and land polygons will be used during",
                                           "the overlay process. This process may take several minutes.")),
                      tags$span(textOutput("overlay_overlay_samegrid_message_text"), style = "color: red"),
                      actionButton("overlay_create_overlaid_models", "Overlay all predictions onto the specified base grid"),
                      textOutput("overlay_overlay_all_text"),
                      tags$br(),
                      tags$span(textOutput("overlay_overlaid_models_message"), style = "color: blue")
                    )
                  )
                )
              )
            )
          ),
          fluidRow(
            box(
              title = "Preview of Base Grid", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE,
              fluidRow(
                box(
                  width = 12,
                  fluidRow(
                    column(
                      width = 6,
                      tags$h5("To preview the base grid, select a set of loaded model predictions"),
                      uiOutput("overlay_preview_base_execute_uiOut_button")
                    ),
                    column(6, helpText("Note: If model predictions were made at a high resolution,",
                                       "then preview may appear to be completely black when zoomed out"))
                  )
                )
              ),
              shinycssloaders::withSpinner(leaflet::leafletOutput("overlay_preview_base"), type = 1)
            ),
            box(
              title = "Preview of Overlaid Model Predictions", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE,
              conditionalPanel(
                condition = "output.overlay_preview_display_flag == false",
                ui.notice.no.pred.overlaid(box.width = 12)
              ),
              conditionalPanel(
                condition = "output.overlay_preview_display_flag",
                fluidRow(
                  box(
                    width = 12,
                    fluidRow(
                      column(9, uiOutput("overlay_preview_overlaid_models_uiOut_selectize")),
                      column(3, tags$br(), actionButton("overlay_preview_overlaid_execute", "Preview"))
                    )
                  )
                ),
                shinycssloaders::withSpinner(plotOutput("overlay_preview_overlaid"), type = 1)
              )
            )
          )
        )
      )
    )
  )
}
