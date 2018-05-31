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
              checkboxInput("overlay_bound", "Use a study area polygon as the boundary for the base grid in the overlay process",
                            value = FALSE),
              helpText("Uncheck the above box to remove the loaded study area polygon from the eSDM"),
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
                        tags$br(),
                        tags$br(),
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
              checkboxInput("overlay_land", "Use a land polygon to remove land from the base grid in the overlay process",
                            value = FALSE),
              conditionalPanel(
                condition = "input.overlay_land == true",
                helpText("Uncheck the above box to remove the loaded land polygon from the eSDM"),
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
                             "High-resolution Geography (GSHHG) Database and contain hierarchical levels L1 and L6,",
                             "meaning they are polygons representing all continents including Antarctica,",
                             "but not lakes and rivers within those continents.", tags$br(),
                             "See the", tags$a("GSHHG website", href = "http://www.soest.hawaii.edu/pwessel/gshhg/"),
                             "for more information about the provided land polygons.", tags$br(),
                             "Please note that higher resolution polygons will take longer to load."),
                    fluidRow(
                      column(6, selectInput("overlay_land_provided_res", tags$h5("Resolution of land polygon"),
                                            choices = list("Full" = 1, "High" = 2, "Intermediate" = 3, "Low" = 4, "Crude" = 5),
                                            selected = 1)),
                      column(6, ui.new.line(), tags$br(), actionButton("overlay_land_provided", "Load provided land polygon"))
                      # column(3, ui.new.line(), tags$br(), )
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

        column(8,
               fluidRow(
                 box(
                   title = "Loaded Model Predictions", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
                   ui.instructions.table.select(text.pre = "loaded", text.in = "to use as the base grid:", sel.num = 1,
                                                text.other = TRUE),
                   conditionalPanel("input.overlay_loaded_table_stats != true", DT::dataTableOutput("overlay_loaded_table")),
                   conditionalPanel("input.overlay_loaded_table_stats", DT::dataTableOutput("overlay_loaded_stats_table")),
                   column(12, checkboxInput("overlay_loaded_table_stats", "Display additional information"))
                 ),
                 box(
                   title = "Overlay Model Predictions", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
                   fluidRow(
                     column(
                       width = 6,
                       box(
                         width = 12,
                         tags$strong("Overlay options: coordinate system"),
                         # helpText("A major element of the overlay process is calculating the area of polygons and their overlap.",
                         #          "Thus, the coordinate system of the model predictions during the overlay process",
                         #          "can have an effect on the overlay results."),
                         # helpText("When calculating area using WGS 84 geographic coordinates, the following assumptions are made:",
                         #          "1) 'Equatorial axis of ellipsoid' = 6378137 and",
                         #          "2) 'Inverse flattening of ellipsoid' = 1/298.257223563.", tags$br(),
                         #          "See", tags$a("this article", href = "https://link.springer.com/article/10.1007%2Fs00190-012-0578-z"),
                         #          "for more details about assumptions that must be made when calculating the area",
                         #          "using WGS 84 geographic coordinates."),
                         checkboxInput("overlay_proj_native",
                                       "Perform the overlay in the native coordinate system of the specified base grid",
                                       value = TRUE),
                         conditionalPanel(
                           condition = "input.overlay_proj_native == false",
                           box(
                             width = 12,
                             radioButtons("overlay_proj_opt", NULL, #tags$h5("Overlay coordinate system"),
                                          choices = list("Select model with desired coordinate system" = 1,
                                                         "Enter numeric EPSG code" = 2,
                                                         "Perform overlay in WGS 84 geographic coordinates" = 3),
                                          selected = 1),
                             conditionalPanel("input.overlay_proj_opt == 1", uiOutput("overlay_proj_which_uiOut_select")),
                             conditionalPanel(
                               condition = "input.overlay_proj_opt == 2",
                               numericInput("overlay_proj_epsg", tags$h5("EPSG code"), value = 4326, step = 1)
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
                           tags$strong("Overlay options: percent overlap"),
                           helpText("The slider bar specifies the percent that the original model prediction(s) must overlap",
                                    "a base grid cell for that cell to have a non-NA overlaid prediction value.",
                                    "A slider bar value of \"0\" means that cell will have a non-NA overlaid prediction value",
                                    "if there is any overlap with any original model prediction."),
                           sliderInput("overlay_grid_coverage", label = NULL, min = 0, max = 100, value = 50)
                         ),
                         box(
                           width = 12,
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
                 ),
                 box(
                   title = "Preview of Base Grid", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE,
                   fluidRow(
                     box(
                       width = 12,
                       fluidRow(
                         column(9, helpText("Note: If model predictions were made at a high resolution, ",
                                            "then preview may appear to be completely black")),
                         column(3, tags$br(), actionButton("overlay_preview_base_execute", "Preview"))
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
