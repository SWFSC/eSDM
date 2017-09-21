### UI code for the 'Evaluation Metrics' tab

ui.evalMetrics <- function() {
  tabItem(tabName = "evalMetrics",
          conditionalPanel(condition = "output.eval_display_flag == false", ui.no.model.pred.loaded1()), 
          conditionalPanel(
            condition = "output.eval_display_flag",
            fluidRow(
              ############################################################################### Available model predictions
              box(
                title = "Select Predictions to Evaluate", status = "warning", solidHeader = FALSE, width = 7, collapsible = TRUE, 
                DT::dataTableOutput("eval_models_table_orig_out"), 
                br(), 
                DT::dataTableOutput("eval_models_table_over_out"), 
                br(), 
                DT::dataTableOutput("eval_models_table_ens_out")
              ),
              ############################################################################### Load validation data
              box(
                title = "Load Validation Data", status = "warning", solidHeader = FALSE, width = 5, collapsible = TRUE,
                fluidRow(
                  # column(6, radioButtons("eval_data_file_num", h5("Validation data are..."), 
                  #                        choices = list("in the same file" = 1, "in different files" = 2),
                  #                        selected = 1)), 
                  column(6, radioButtons("eval_load_type_1", h5("Validation data file type"), choices = file.type.list1, selected = 1)), 
                  column(6, radioButtons("eval_data_type_1", h5("Validation data type"),
                                         choices = list("Counts (numerical)" = 1, "Presence or absence" = 2),
                                         selected = 1))
                ),
                # conditionalPanel( # P/A points are in same file
                #   condition = "input.eval_data_file_num == 1",
                #########################################################  Excel csv file
                conditionalPanel(
                  condition = "input.eval_load_type_1 == 1",
                  helpText("Excel csv file must have lat/long coordinates for points and a column with presence/absence/count data"), 
                  fileInput("eval_csv_1", h5("Upload Excel .csv file")),
                  conditionalPanel("output.eval_csv_1_flag == false", strong("For csv file, please choose a file that has a .csv file extension")),
                  conditionalPanel(
                    condition = "output.eval_csv_1_flag", 
                    box(width = 12, 
                        uiOutput("eval_csv_names_1_uiOut_select"),
                        conditionalPanel(
                          condition = "input.eval_data_type_1 == 2", 
                          fluidRow(
                            column(6, uiOutput("eval_csv_codes_1_p_uiOut_select")),
                            column(6, uiOutput("eval_csv_codes_1_a_uiOut_select"))
                          )
                        ),
                        fluidRow(
                          column(4, uiOutput("eval_csv_execute_1_uiOut_button")), 
                          column(4, textOutput("eval_csv_data_1_text"))
                        )
                    )
                  )
                ),
                ######################################################### GIS shp file
                conditionalPanel(
                  condition = "input.eval_load_type_1 == 2",
                  ui.gis.shp.intructions(),
                  fileInput("eval_gis_shp_1", label = h5("Upload GIS shapefile files"), multiple = T), 
                  conditionalPanel(
                    condition = "output.eval_gis_1_shp_flag == false", 
                    strong("Could not load GIS shapefile using the provided file(s)")
                  )
                ), 
                ######################################################### GIS gdb file
                conditionalPanel(
                  condition = "input.eval_load_type_1 == 3",
                  ui.gis.gdb.intructions(),
                  textInput("eval_gis_gdb_path_1", label.gdb.path, value = ".../folder.gdb"),
                  fluidRow(
                    column(6, textInput("eval_gis_gdb_name_1", label.gdb.name, value = "")), 
                    column(6, br(), br(), actionButton("eval_gis_gdb_load_1", label.gdb.upload))
                  ), 
                  br(), 
                  conditionalPanel(
                    condition = "output.eval_gis_1_gdb_flag == false", 
                    strong("Could not load GIS file using the provided path and filename")
                  )
                ), 
                ######################################################### GIS shp or gdb file
                conditionalPanel(
                  condition = "output.eval_gis_1_flag",
                  box(title = NULL, width = 12, 
                      uiOutput("eval_gis_names_1_uiOut_select"),
                      conditionalPanel(
                        condition = "input.eval_data_type_1 == 2", 
                        fluidRow(
                          column(6, uiOutput("eval_gis_codes_1_p_uiOut_select")),
                          column(6, uiOutput("eval_gis_codes_1_a_uiOut_select"))
                        )
                      ),
                      fluidRow(
                        column(4, uiOutput("eval_gis_execute_1_uiOut_button")), 
                        column(4, textOutput("eval_data_1_gis_text"))
                      )
                  )
                ),
                # )
                # 
                # ### Presence and absence points are in different files
                # conditionalPanel(
                #   condition = "input.eval_data_file_num == 2",
                #   helpText(h2("Not yet implemented")),
                #     radioButtons("eval_pa_file_p_or_a", label = NULL, 
                #                  choices = list("Load presence points" = "p", "Load absence points" = "a"),
                #                  selected = 1),
                #     
                #     # Load presence points
                #     conditionalPanel(
                #       condition = "input.eval_pa_file_p_or_a == 'p'",
                #       radioButtons("eval_load_type_2p", h5("Presence points file type"),
                #                    choices = list("CSV file" = 1, "GIS file" = 2),
                #                    selected = 1),
                #       
                #       # Presence - csv
                #       conditionalPanel(
                #         condition = "input.eval_load_type_2p == 1",
                #         helpText("Select lat and long columns after uploading csv file"),
                #         fileInput("eval_csv_2p", label = h5("Upload presence .csv file")),
                #         uiOutput("eval_csv_names_2p_uiOut_select")
                #       ),
                #       
                #       # Presence - gis
                #       conditionalPanel(
                #         condition = "input.eval_load_type_2p == 2",
                #         radioButtons("eval_gis_file_type_2p", "GIS file type",
                #                      choices = list("Shapefile" = 1, "File geodatabase (.gdb) file" = 2)),
                #         conditionalPanel(
                #           condition = "input.eval_gis_file_type_2p == 1",
                #           ui.gis.shp.intructions(),
                #           fileInput("eval_gis_shp_2p", h5("Upload GIS files"), multiple = TRUE)
                #         ),
                #         conditionalPanel(
                #           condition = "input.eval_gis_file_type_2p == 2",
                #           ui.gis.gdb.intructions(),
                #           textInput("eval_gis_gdb_path_2p", label.gdb.path, value = "C:/Ensemble Shiny/Ensemble_R_Shiny/"),
                #           textInput("eval_gis_gdb_name_2p", label.gdb.name, value = ""),
                #           actionButton("eval_gis_gdb_load_2p", label.gdb.upload)
                #         )
                #       )
                #     ),
                #     
                #     # Load absence points
                #     conditionalPanel(
                #       condition = "input.eval_pa_file_p_or_a == 'a'",
                #       radioButtons("eval_load_type_2a", "Absence points file type", 
                #                    choices = list("CSV file" = 1, "GIS file" = 2), selected = 1),
                #       # Absence - csv
                #       conditionalPanel(
                #         condition = "input.eval_load_type_2a == 1",
                #         helpText("Select lat and long columns after uploading csv file"),
                #         fileInput("eval_csv_2a", h5("Upload absence .csv file")),
                #         uiOutput("eval_csv_names_2a_uiOut_select")
                #         
                #       ),
                #       # Absence - gis
                #       conditionalPanel(
                #         condition = "input.eval_load_type_2a == 2",
                #         radioButtons("eval_gis_file_type_2a", "GIS file type", 
                #                      choices = list("Shapefile" = 1, "File geodatabase (.gdb) file" = 2)),
                #         conditionalPanel(
                #           condition = "input.eval_gis_file_type_2a == 1",
                #           ui.gis.shp.intructions(),
                #           fileInput("eval_gis_shp_2a", h5("Upload GIS files"), multiple = TRUE)
                #         ),
                #         conditionalPanel(
                #           condition = "input.eval_gis_file_type_2a == 2",
                #           ui.gis.gdb.intructions(),
                #           textInput("eval_gis_gdb_path_2a", label.gdb.path, value = "C:/Ensemble Shiny/Ensemble_R_Shiny/"),
                #           textInput("eval_gis_gdb_name_2a", label.gdb.name, value = ""),
                #           actionButton("eval_gis_gdb_load_2a", label.gdb.upload)
                #         )
                #       )
                #     )
                #   ), 
                textOutput("eval_data_1_message")
                # textOutput("eval_data_pres_1_message"), 
                # textOutput("eval_data_abs_1_message")
              )
            ),
            fluidRow(
              column(7, 
                     fluidRow(
                       ############################################################################### Calculate metrics
                       box(
                         title = "Calculate Metrics", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
                         conditionalPanel(
                           condition = "output.eval_display_calc_metrics_flag == false", 
                           helpText("Load validation data in order to calculate metrics")
                         ), 
                         conditionalPanel(
                           condition = "output.eval_display_calc_metrics_flag", 
                           fluidRow(
                             column(4,
                                    h5("Validation data info"), 
                                    tableOutput("table_pa_pts_out"), 
                                    tags$style(type="text/css", "#table_pa_pts_out td:first-child {font-weight:bold;}")
                                    #tr:first-child for first row
                             ),
                             column(3, offset = 1, uiOutput("eval_metrics_which_uiOut_check")),
                             column(4, br(), 
                                    actionButton("eval_metrics_execute", "Calculate metrics"), 
                                    textOutput("eval_metrics_text")
                             )
                           )
                         )
                       ),
                       box(
                         title = "Metric Descriptions and References", status = "warning", solidHeader = FALSE, width = 12, 
                         collapsible = TRUE,
                         fluidRow(
                           column(3, radioButtons("eval_metrics_description", NULL, 
                                                  choices = list("Area under the curve (AUC)" = 1, "True Skill Statistic (TSS)" = 2, 
                                                                 "Root mean squared error (RMSE)" = 3))), 
                           column(8, 
                                  conditionalPanel(
                                    condition = "input.eval_metrics_description == 1", 
                                    helpText(p(strong("AUC decription:"), 
                                               "The Area Under the Curve of the ROCR plot is a threshold independent metric that evaluates", 
                                               "the percentage of the time a random selection from the positive group will have a score", 
                                               "greater than a random selection from the negative class (Deleo 1993)."), 
                                             "See", a("Fielding and Bell 1997",
                                                      href = "http://cescos.fau.edu/gawliklab/papers/FieldingAHandJFBell1997.pdf"), 
                                             "for more information.")
                                  ), 
                                  conditionalPanel(
                                    condition = "input.eval_metrics_description == 2", 
                                    helpText(p(strong("TSS decription:"), 
                                               "The True Skill Statistic is a threshold dependent measure, and thus is calculated with respect to", 
                                               "a density cut-off, where densities above the cutoff are considered presence, and vice versa.", 
                                               "TSS = (true positive rate) + specificity (true negative rate) - 1,", 
                                               "and the maximum TSS for all posible density cutoffs is reported.", 
                                               "TSS is recommended for use in place of kappa because kappa", 
                                               "depends on prevalence while TSS does not."), 
                                             "See", a(HTML(paste("Allouche", em("et al."), "2006")),
                                                      href = "http://onlinelibrary.wiley.com/doi/10.1111/j.1365-2664.2006.01214.x/epdf"), 
                                             "for more information.")
                                  ), 
                                  conditionalPanel(
                                    condition = "input.eval_metrics_description == 3", 
                                    helpText(p(strong("RMSE decription:"), "The root mean squared error is a metric that measures the difference", 
                                               "between predicted response variable values and observed values. RMSE requires count data to be", 
                                               "loaded rather than presence/absence data. To calculate RMSE the process is as follows:", 
                                               "for each validation data point, subtract the observed abundance from the predicted abundance and", 
                                               "square it. Then take the square root of the mean of all of these squared error values."), 
                                             "See", a("this page", href = "https://en.wikipedia.org/wiki/Root-mean-square_deviation"), 
                                             "for more information.")
                                  )
                           )
                         )
                       )
                     )
              ),
              
              ############################################################################### Calculated metrics
              column(5, 
                     fluidRow(
                       box(
                         title = "Metrics", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE, 
                         fluidRow(
                           column(6, tableOutput("table_eval_metrics_out")), 
                           column(5,
                                  conditionalPanel(
                                    condition = "output.table_eval_metrics_out != null", 
                                    downloadButton("eval_metrics_table_save", "Download metrics"), 
                                    br(), br(), 
                                    helpText(strong("Note:"), 
                                             "The downloaded Excel csv file will have both metric values and model information", 
                                             "for each set of predictions. Because ensemble predictions have different information", 
                                             "than original and overlaid predictions, some column headers are formatted as", 
                                             "'Original+Overlaid info/Ensemble info'.")
                                  )
                           )
                         )
                       )
                     )
              )
            )
          )
  )
}