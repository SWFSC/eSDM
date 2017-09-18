ui.overlay <- function() {
  tabItem(tabName = "overlay",
          conditionalPanel("output.overlay_display_flag == false", ui.no.model.pred.loaded1()), 
          conditionalPanel(
            condition = "output.overlay_display_flag", 
            fluidRow(
              column(4,
                     fluidRow(
                       box(
                         title = "Load Study Area Polygon", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE, 
                         checkboxInput("overlay_bound", "Use a study area polygon in the overlay process", value = FALSE),
                         conditionalPanel(
                           condition = "input.overlay_bound == true", 
                           fluidRow(
                             column(6, radioButtons("overlay_bound_file_type", h5("File type"), choices = file.type.list1, selected = 1)), 
                             column(5, br(), helpText("Uncheck box to remove loaded study area polygon"))
                           ), 
                           box(width = 12, 
                               conditionalPanel(
                                 condition = "input.overlay_bound_file_type == 1",
                                 ui.csv.poly.instructions(), 
                                 fluidRow(
                                   column(6, fileInput("overlay_bound_csv_file", h5("Upload csv file"), accept = ".csv")), 
                                   column(6, 
                                          br(), br(), 
                                          textOutput("overlay_bound_csv_message"), 
                                          textOutput("overlay_bound_csv_text")
                                   )
                                 )
                               ),
                               conditionalPanel(
                                 condition = "input.overlay_bound_file_type == 2",
                                 ui.gis.shp.intructions(),
                                 fluidRow(
                                   column(6, fileInput("overlay_bound_gis_shp_files", h5("Upload GIS files"), multiple = T)),
                                   column(6,
                                          br(), br(),
                                          textOutput("overlay_bound_gis_shp_message"),
                                          textOutput("overlay_bound_gis_shp_text")
                                   )
                                 )
                               ),
                               conditionalPanel(
                                 condition = "input.overlay_bound_file_type == 3",
                                 ui.gis.gdb.intructions(),
                                 textInput("overlay_bound_gis_gdb_path", h5(".gbd path"), value = "C:/Ensemble Shiny/Ensemble_R_Shiny/.gdb"),
                                 textInput("overlay_bound_gis_gdb_name", h5("Filename within .gbd folder"), value = ""),
                                 fluidRow(
                                   column(6, actionButton("overlay_bound_gis_gdb_load", "Upload file from specified path")),
                                   column(6,
                                          textOutput("overlay_bound_gis_gdb_message"),
                                          textOutput("overlay_bound_gis_gdb_text")
                                   )
                                 )
                               )
                           )
                         )
                       ), 
                       
                       box(
                         title = "Load Land Polygon", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE, 
                         checkboxInput("overlay_land", "Use a land polygon in the overlay process", value = FALSE),
                         conditionalPanel(
                           condition = "input.overlay_land == true", 
                           helpText("Uncheck box to remove loaded land polygon"), 
                           br(), 
                           fluidRow(
                             column(6, radioButtons("overlay_land_load_type", h5("Land polygon source"), 
                                                    choices = list("Use provided" = 1, "Upload personal" = 2), 
                                                    selected = 1)), 
                             column(6, 
                                    conditionalPanel(
                                      condition = "input.overlay_land_load_type == 2 ", 
                                      radioButtons("overlay_land_file_type", h5("File type"), choices = file.type.list1, selected = 2)
                                    )
                             )
                           ), 
                           conditionalPanel(
                             condition = "input.overlay_land_load_type == 1", 
                             box(width = 12, 
                                 helpText("The provided land polygon is from the Global Self-consistent, Hierarchical,", 
                                          "High-resolution Geography (GSHHG) Database.", br(), 
                                          "See the", a("GSHHG website", href="http://www.soest.hawaii.edu/pwessel/gshhg/"),
                                          "for more information about the provided land polygons."), 
                                 fluidRow(
                                   column(3, selectInput("overlay_land_provided_res", h5("Resolution of land polygon"), 
                                                         choices = list("Full" = 1, "High" = 2, "Intermediate" = 3, "Low" = 4, "Crude" = 5), 
                                                         selected = 1)), 
                                   column(5, ui.new.line(), br(), actionButton("overlay_land_provided", "Load provided land polygon")), 
                                   column(4, ui.new.line(), br(), textOutput("overlay_land_provided_message")))
                             )
                           ), 
                           conditionalPanel(
                             condition = "input.overlay_land_load_type == 2 && input.overlay_land_file_type == 1",
                             box(width = 12, 
                                 ui.csv.poly.instructions(), 
                                 fluidRow(
                                   column(6, fileInput("overlay_land_csv_file", h5("Upload csv file"), accept = ".csv")),
                                   column(6, br(), br(),
                                          textOutput("overlay_land_csv_message"),
                                          textOutput("overlay_land_csv_text"))
                                 )
                             )
                           ),
                           conditionalPanel(
                             condition = "input.overlay_land_load_type == 2 && input.overlay_land_file_type == 2",
                             box(width = 12, 
                                 ui.gis.shp.intructions(), 
                                 fluidRow(
                                   column(6, fileInput("overlay_land_gis_shp_files", h5("Upload GIS files"), multiple = T)),
                                   column(6,
                                          br(), br(),
                                          textOutput("overlay_land_gis_shp_message"), 
                                          textOutput("overlay_land_gis_shp_text")
                                   )
                                 )
                             )
                           ),
                           conditionalPanel(
                             condition = "input.overlay_land_load_type == 2 && input.overlay_land_file_type == 3",
                             box(width = 12, 
                                 ui.gis.gdb.intructions(),
                                 textInput("overlay_land_gis_gdb_path", h5(".gbd path"), value = "C:/Ensemble Shiny/Ensemble_R_Shiny/.gdb"),
                                 textInput("overlay_land_gis_gdb_name", h5("Name of file within .gbd"), value = ""), 
                                 fluidRow(
                                   column(6, actionButton("overlay_land_gis_gdb_load", "Upload file from specified path")),
                                   column(6,
                                          textOutput("overlay_land_gis_gdb_message"), 
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
                         title = "Loaded Model Predictions", status = "warning", solidHeader = FALSE, width = 12, 
                         collapsible = TRUE, 
                         conditionalPanel("input.overlay_loaded_table_stats != true", DT::dataTableOutput("overlay_loaded_table")),
                         conditionalPanel("input.overlay_loaded_table_stats", DT::dataTableOutput("overlay_loaded_stats_table")),
                         fluidRow(
                           column(4, checkboxInput("overlay_loaded_table_stats", "Display additional information")),
                           column(8, 
                                  conditionalPanel(
                                    condition = "input.overlay_loaded_table_stats != true", 
                                    helpText("Click on a row to select model predictions to use as base grid")
                                  ), 
                                  conditionalPanel(
                                    condition = "input.overlay_loaded_table_stats", 
                                    helpText("A row can only be selected if 'Display additional information' is unchecked")
                                  )
                           )
                         )
                       ),
                       box(
                         title = "Overlay Model Predictions", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE, 
                         fluidRow(
                           box(width = 6, 
                               h5("Overlay projection options:"), 
                               helpText("A major element of the overlay process is calculating the area of polygons and their overlap.", 
                                        "Thus, the projection of the model predictions during the overlay process", 
                                        "can have an effect on the overlay results."), 
                               helpText("When calculating area using WGS 84 geographic coordinates, the following assumptions are made:", 
                                        "1) 'Equatorial axis of ellipsoid' = 6378137 and", 
                                        "2) 'Inverse flattening of ellipsoid' = 1/298.257223563.", br(), 
                                        "See", a("this article", href = "https://link.springer.com/article/10.1007%2Fs00190-012-0578-z"), 
                                        "for more details about assumptions that must be made when calculating the area", 
                                        "using WGS 84 geographic coordinates."), 
                               checkboxInput("overlay_proj_ll", "Perform overlay in WGS 84 geographic coordinates", value = TRUE), 
                               conditionalPanel(
                                 condition = "input.overlay_proj_ll", 
                                 helpText("Area calculations will be performed in WGS 84 geographic coordinates")
                               ), 
                               conditionalPanel(
                                 condition = "input.overlay_proj_ll == false", 
                                 column(12, uiOutput("overlay_proj_which_uiOut_select"))
                               )
                           ), 
                           column(6, 
                                  fluidRow(
                                    box(width = 12, 
                                        h5("Overlay percent overlap options:"), 
                                        helpText("The percentage of a base grid cell that must be covered by original model predictions", 
                                                 "for that base grid cell not to have a value of NA. If \"0\" is selected,", 
                                                 "then the base grid cell will not have a value of NA if there is any overlap"),
                                        sliderInput("overlay_grid_coverage", label = NULL, min = 0, max = 100, value = 100)
                                    ), 
                                    box(width = 12,
                                        helpText(strong("Reminder: loaded study area and land polygons will be used during", 
                                                        "the overlay process"), br(), 
                                                 "This process may take several minutes"),
                                        actionButton("overlay_create_overlaid_models", "Overlay all predictions onto specified base grid"),
                                        textOutput("overlay_text_overlaid_models")
                                    )
                                  )
                           )
                         )
                       ),
                       box(
                         title = "Preview of Base Grid", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, 
                         fluidRow(
                           box(width = 12, 
                               fluidRow(
                                 column(9, helpText("Note: If model predictions were made at a high resolution,", 
                                                    "then preview may appear to be completely black")), 
                                 column(3, br(), actionButton("overlay_preview_base_execute", "Preview"))
                               )
                           )
                         ),
                         shinycssloaders::withSpinner(plotOutput("overlay_preview_base"), type = 1)
                       ), 
                       box(
                         title = "Preview of Overlaid Model Predictions", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE,
                         fluidRow(
                           box(width = 12, 
                               fluidRow(
                                 column(9, uiOutput("overlay_preview_overlaid_models_uiOut_selectize")), 
                                 column(3, br(), actionButton("overlay_preview_overlaid_execute", "Preview"))
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
}