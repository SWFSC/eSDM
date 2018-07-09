### UI code for the 'Evaluation Metrics' tab

ui.evalMetrics <- function() {
  tabItem(
    tabName = "evalMetrics",
    conditionalPanel(condition = "output.eval_display_flag == false", ui.notice.no.pred.original()),
    conditionalPanel(
      condition = "output.eval_display_flag",
      fluidRow(
        ############################################################################### Available model predictions
        box(
          title = "Select Predictions to Evaluate", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
          ui.instructions.multipletables.select(text.in = "evaluate:"),
          DTOutput("eval_models_table_orig_out"),
          tags$br(),
          DTOutput("eval_models_table_over_out"),
          tags$br(),
          DTOutput("eval_models_table_ens_out")
        ),
        ############################################################################### Load validation data
        box(
          title = "Load Validation Data", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
          fluidRow(
            column(6, radioButtons("eval_load_type", tags$h5("Validation data file type"),
                                   choices = file.type.list1, selected = 1)),
            column(6, radioButtons("eval_data_type", tags$h5("Validation data type"),
                                   choices = list("Count (numerical)" = 1, "Presence/absence" = 2),
                                   selected = 1))
          ),
          #########################################################  Excel csv file
          conditionalPanel(
            condition = "input.eval_load_type == 1",
            ui.instructions.upload.csv(),
            helpText("The Excel .csv file must have columns with the longitude and latitude coordinates of the data,",
                     "in addition to the column with validation data.",
                     "The longitude and latitude coordinates are assumed to be WGS 84 geographic coordinates."),
            fileInput("eval_csv", label.csv.upload, accept = ".csv"),
            conditionalPanel("output.eval_csv_flag == false", ui.error.upload.csv),
            conditionalPanel(
              condition = "output.eval_csv_flag",
              box(
                width = 12,
                uiOutput("eval_csv_names_uiOut_select"),
                conditionalPanel(
                  condition = "input.eval_data_type == 1",
                  conditionalPanel(
                    condition = "output.eval_csv_error_flag == 1",
                    helpText("Please select exactly three columns above")
                  )
                ),
                conditionalPanel(
                  condition = "input.eval_data_type == 2",
                  conditionalPanel(
                    condition = "output.eval_csv_error_flag == 1",
                    helpText("Please select exactly three columns above to select presence and absence codes")
                  ),
                  conditionalPanel(
                    condition = "output.eval_csv_error_flag == 2",
                    helpText("There are more than 50 unique codes for the currently selected validation data column.",
                             "Please select a different column or select \"Counts (numerical)\" for",
                             tags$em("Validation data type"), ".")
                  ),
                  column(6, uiOutput("eval_csv_codes_p_uiOut_select")),
                  column(6, uiOutput("eval_csv_codes_a_uiOut_select"))
                ),
                fluidRow(
                  column(6, uiOutput("eval_csv_execute_uiOut_button")),
                  column(6, textOutput("eval_csv_data_text"))
                )
              )
            )
          ),
          ######################################################### GIS shp file
          conditionalPanel(
            condition = "input.eval_load_type == 2",
            ui.instructions.upload.shp(),
            fileInput("eval_gis_shp", label.shp.upload, multiple = TRUE),
            conditionalPanel("output.eval_gis_shp_flag == false", ui.error.upload.shp)
          ),
          ######################################################### GIS gdb file
          conditionalPanel(
            condition = "input.eval_load_type == 3",
            ui.instructions.upload.gdb(),
            textInput("eval_gis_gdb_path", label.gdb.path, value = ".../folder.gdb"),
            fluidRow(
              column(6, textInput("eval_gis_gdb_name", label.gdb.name, value = "")),
              column(6, tags$br(), tags$br(), actionButton("eval_gis_gdb_load", label.gdb.upload))
            ),
            tags$br(),
            conditionalPanel("output.eval_gis_gdb_flag == false", ui.error.upload.gdb)
          ),

          ######################################################### GIS shp or gdb file
          conditionalPanel(
            condition = "output.eval_gis_flag",
            box(
              width = 12,
              uiOutput("eval_gis_names_uiOut_select"),
              conditionalPanel(
                condition = "input.eval_data_type == 2",
                conditionalPanel(
                  condition = "output.eval_gis_error_flag == 2",
                  helpText("There are more than 50 unique codes for the currently selected validation data column.",
                           "Please select a different column or select \"Counts (numerical)\" for",
                           tags$em("Validation data type"), ".")
                ),
                column(6, uiOutput("eval_gis_codes_p_uiOut_select")),
                column(6, uiOutput("eval_gis_codes_a_uiOut_select"))
              ),
              fluidRow(
                column(6, uiOutput("eval_gis_execute_uiOut_button")),
                column(6, textOutput("eval_data_gis_text"))
              )
            )
          ),
          tags$span(textOutput("eval_data_message"), style = "color: blue")
        )
      ),
      fluidRow(
        column(
          width = 6,
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
                  column(
                    width = 5,
                    tags$strong(textOutput("table_eval_pts_title")),
                    tableOutput("table_eval_pts_out"),
                    tags$style(type="text/css", "#table_pa_pts_out td:first-child {font-weight:bold;}")
                    #tr:first-child for first row
                  ),
                  column(
                    width = 3, offset = 1,
                    uiOutput("eval_metrics_which_uiOut_check"),
                    tags$br(),
                    helpText("See 'Metrics Descriptions and References' section below for metric information")
                  ),
                  column(
                    width = 3,
                    tags$br(),
                    actionButton("eval_metrics_execute", "Calculate metrics"),
                    tags$br(), tags$br(),
                    textOutput("eval_metrics_text"),
                    tags$span(textOutput("eval_metrics_message"), style = "color: blue")
                  )
                )
              )
            ),
            box(
              title = "Metric Descriptions and References", status = "warning", solidHeader = FALSE, width = 12,
              collapsible = TRUE,
              fluidRow(
                column(3, radioButtons("eval_metrics_description", NULL,
                                       choices = list("Area under the curve (AUC)" = 1,
                                                      "True skill statistic (TSS)" = 2,
                                                      "Root mean squared error (RMSE)" = 3))),
                column(
                  width = 8,
                  conditionalPanel(
                    condition = "input.eval_metrics_description == 1",
                    helpText(
                      tags$p(tags$strong("AUC decription:"),
                             "The area under the curve (AUC) of the ROCR plot is a threshold independent metric that evaluates",
                             "the percentage of the time a random selection from the positive group will have a score",
                             "greater than a random selection from the negative class (Deleo 1993)."),
                      "See", tags$a("Fielding and Bell 1997",
                                    href = "http://cescos.fau.edu/gawliklab/papers/FieldingAHandJFBell1997.pdf"),
                      "for more information."
                    )
                  ),
                  conditionalPanel(
                    condition = "input.eval_metrics_description == 2",
                    helpText(
                      tags$p(tags$strong("TSS decription:"),
                             "The true skill statistic (TSS) is a threshold dependent measure, and thus is calculated with respect",
                             "to a density cut-off, where densities above the cutoff are considered presence, and vice versa.",
                             "TSS = (true positive rate) + specificity (true negative rate) - 1, ",
                             "and the maximum TSS for all posible density cutoffs is reported.",
                             "TSS is recommended for use in place of kappa because kappa",
                             "depends on prevalence while TSS does not."),
                      "See", tags$a(HTML(paste("Allouche", tags$em("et al."), "2006")),
                                    href = "http://onlinelibrary.wiley.com/doi/10.1111/j.1365-2664.2006.01214.x/epdf"),
                      "for more information."
                    )
                  ),
                  conditionalPanel(
                    condition = "input.eval_metrics_description == 3",
                    helpText(
                      tags$p(tags$strong("RMSE decription:"), "The root mean squared error  (RMSE) is a metric that measures the",
                             "difference between predicted response variable values and observed values.",
                             "RMSE requires count data to be loaded rather than presence/absence data.",
                             "To calculate RMSE the process is as follows:",
                             "for each validation data point, subtract the observed abundance from the predicted abundance and",
                             "square it. Then take the square root of the mean of all of these squared error values."),
                      "See", tags$a("this page", href = "https://en.wikipedia.org/wiki/Root-mean-square_deviation"),
                      "for more information."
                    )
                  )
                )
              )
            )
          )
        ),

        ############################################################################### Calculated metrics
        box(
          title = "Metrics", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
          fluidRow(
            column(
              width = 6,
              tableOutput("table_eval_metrics_out"),
              tags$br(),
              column(10, textOutput("eval_metrics_overlap_text"))
            ),
            column(
              width = 6,
              conditionalPanel(
                condition = "output.table_eval_metrics_out != null",
                downloadButton("eval_metrics_table_save", "Download metrics"),
                tags$br(),
                tags$br(),
                helpText(tags$strong("Note:"),
                         "The downloaded Excel csv file will have both metric values and model information",
                         "for each set of predictions.",
                         "Ensemble predictions have different information than original and overlaid predictions,",
                         "and thus if evaluation metrics have been calculated for both ensemble predictions and",
                         "original and/or overlaid predictions then some column headers will be formatted as",
                         "'Original+Overlaid info name/Ensemble info name'.")
              )
            )
          )
        )
      )
    )
  )
}
