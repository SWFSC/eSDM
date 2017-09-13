ui.overlay <- function() {
  tabItem(tabName = "overlay",
          conditionalPanel(condition = "output.overlay_display_flag == false", ui.no.model.pred.loaded1()), 
          conditionalPanel(
            condition = "output.overlay_display_flag", 
            fluidRow(
              column(4,
                     fluidRow(
                       box(
                         title = "Load Study Area Polygon", status = "warning", solidHeader = FALSE, 
                         width = 12, collapsible = TRUE, 
                         checkboxInput("overlay_bound_gis", "Use a study area polygon in overlay process", value = FALSE),
                         conditionalPanel(
                           condition = "input.overlay_bound_gis == true", 
                           fluidRow(
                             column(6, radioButtons("overlay_bound_file_type", h5("File type"), choices = file.type.list1, selected = 1)), 
                             column(5, br(), helpText("Uncheck box to remove loaded study area polygon"))
                           ), 
                           box(width = 12, 
                               conditionalPanel(
                                 condition = "input.overlay_bound_file_type == 1",
                                 ui.csv.poly.instructions(), 
                                 fluidRow(
                                   column(6, fileInput("overlay_bound_csv_file", h5("Load csv file"), accept = ".csv")), 
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
                                   column(6, fileInput("overlay_bound_gis_shp_files", h5("Load GIS files"), multiple = T)),
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
                                   column(6, actionButton("overlay_bound_gis_gdb_load", "Load file from specified path")),
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
                         title = "Load Land Polygon", status = "warning", solidHeader = FALSE, 
                         width = 12, collapsible = TRUE, 
                         checkboxInput("overlay_land_gis", "Use a land area polygon in overlay process", value = FALSE),
                         conditionalPanel(
                           condition = "input.overlay_land_gis == true", 
                           helpText("Uncheck box to remove loaded study area polygon"), 
                           br(), 
                           fluidRow(
                             column(6, radioButtons("overlay_land_load_type", h5("Land polygon source"), 
                                                 choices = list("Use provided" = 1, "Load personal" = 2), 
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
                                 tagList(span(class="help-block", "The provided land polygon is from the Global", 
                                              "Self-consistent, Hierarchical, High-resolution Geography Database. See the", 
                                              a("GSHHG homepage", href="http://www.soest.hawaii.edu/pwessel/gshhg/"),
                                              "for resolution details or other information"
                                 )), 
                                 fluidRow(
                                   column(5, selectInput("overlay_land_provided_res", h5("Resolution of file to load"), 
                                                         choices = list("Full" = 1, "High" = 2, "Intermediate" = 3, "Low" = 4, "Crude" = 5), 
                                                         selected = 1)), 
                                   column(3, br(), br(), actionButton("overlay_land_provided", "Load file")), 
                                   column(4, ui.new.line(), textOutput("overlay_land_provided_message")))
                             )
                           ), 
                           conditionalPanel(
                             condition = "input.overlay_land_load_type == 2 && input.overlay_land_file_type == 1",
                             box(width = 12, 
                                 ui.csv.poly.instructions(), 
                                 fluidRow(
                                   column(6, fileInput("overlay_land_csv_file", h5("Load csv file"), accept = ".csv")),
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
                                   column(6, fileInput("overlay_land_gis_shp_files", h5("Load GIS files"), multiple = T)),
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
                                   column(6, actionButton("overlay_land_gis_gdb_load", "Load file from specified path")),
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
                         conditionalPanel(
                           condition = "output.overlay_loaded_table != null", 
                           fluidRow(
                             column(4, 
                                    checkboxInput("overlay_loaded_table_stats", "Display additional model information"),
                                    uiOutput("overlay_preview_options_uiOut_checkGroup")
                             ),
                             column(8, 
                                    conditionalPanel(
                                      condition = "input.overlay_loaded_table_stats != true", 
                                      helpText("Click on row(s) to select model predictions to perform an action")
                                    ), 
                                    conditionalPanel(
                                      condition = "input.overlay_loaded_table_stats", 
                                      helpText("Rows can only be selected if 'Display additional information' is unchecked")
                                    ), 
                                    ui.new.line(), 
                                    actionButton("overlay_preview_execute", "Preview grid"), 
                                    br(), 
                                    helpText("Note: If the model is large and and is fairly high resolution,", 
                                             "then the base grid preview likely will appear to be completely black")
                             )
                           )
                         )
                       ),
                       box(
                         title = "Overlay Model Predictions", status = "warning", solidHeader = FALSE, 
                         width = 7, collapsible = TRUE, 
                         conditionalPanel(
                           condition = "output.overlay_loaded_table != null",
                           helpText(strong("Reminder: loaded study area and land polygons will be used in overlay process")),
                           # strong("Reminder: loaded study area and land polygons will be used in overlay process"),
                           # helpText("Reminder: loaded study area and land polygons will be used in overlay process"),
                           # h5("Reminder: loaded study area and land polygons will be used in overlay process"),
                           h5("Overlay options:"), 
                           box(width = 11, 
                               helpText("A major element of the overlay process is calculating the area of polygons", 
                                        "and their overlap. Thus, the projection of the model predictions when the", 
                                        "overlay process is performed can alter the overlay results"), 
                               ui.areacalc.ll.assumptions(), 
                               checkboxInput("overlay_proj_ll", "Perform overlay in lat/long WGS84 geographic coordinates", value = TRUE), 
                               conditionalPanel(
                                 condition = "input.overlay_proj_ll", 
                                 helpText("Area calculations will be performed using lat/long geographic coordinates")
                               ), 
                               conditionalPanel("input.overlay_proj_ll == false", column(12, uiOutput("overlay_proj_which_uiOut_select")))
                           ), 
                           box(width = 11, 
                               sliderInput("overlay_grid_coverage", 
                                           h5("The percentage of a base grid cell that must be covered by original", 
                                              "model predictions for the base grid cell not to have a value of NA.", 
                                              "If '0' is selected, then the base grid cell will not have a value of NA if there is any overlap"), 
                                           min = 0, max = 100, value = 100)
                           ), 
                           fluidRow(
                             column(6, 
                                    actionButton("overlay_create_overlaid_models", "Overlay all models onto selected gird"),
                                    textOutput("overlay_text_overlaid_models")
                             ), 
                             column(6, helpText("This will likely take several minutes"))
                           )
                         )
                       ),
                       box(
                         title = "Preview of Overlay Base", status = "primary", solidHeader = TRUE, width = 5, collapsible = TRUE,
                         shinycssloaders::withSpinner(plotOutput("overlay_preview_base"), type = 1)
                       )
                     )
              )
            )
          )
  )
}