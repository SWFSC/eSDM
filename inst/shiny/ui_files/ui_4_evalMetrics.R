### UI code for the 'Evaluation Metrics' tab

ui.evalMetrics <- function() {
  tabItem(
    tabName = "evalMetrics",
    conditionalPanel(condition = "output.eval_display_flag == false", ui.notice.no.pred.general()),
    conditionalPanel(
      condition = "output.eval_display_flag",
      fluidRow(
        ############################################################################### Available predictions
        box(
          title = "Select Predictions to Evaluate", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
          ui.instructions.multipletables.select(text.in = "evaluate:"),
          DTOutput("eval_models_table_orig_out"),
          tags$br(),
          DTOutput("eval_models_table_over_out"),
          tags$br(),
          DTOutput("eval_models_table_ens_out")
        ),
        ############################################################################### Import validation data
        box(
          title = "Import Validation Data", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
          tags$h5("Importing validation data will overwrite any previously imported validation data.",
                  "All points with values of 'NA' will be removed during importing.",
                  style = "color: blue;"),
          fluidRow(
            column(6, radioButtons("eval_load_type", tags$h5("Validation data file type"),
                                   choices = file.type.list1, selected = 1)),
            column(6, radioButtons("eval_data_type", tags$h5("Validation data type"),
                                   choices = list("Count (numerical)" = 1, "Presence/absence" = 2),
                                   selected = 1))
          ),
          #########################################################  CSV file
          conditionalPanel(
            condition = "input.eval_load_type == 1",
            ui.instructions.upload.csv(),
            helpText("The CSV file must have columns with the longitude and latitude coordinates of the data,",
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
                    tags$h5("Please select exactly three columns above", style = "color: red;")
                  )
                ),
                conditionalPanel(
                  condition = "input.eval_data_type == 2",
                  conditionalPanel(
                    condition = "output.eval_csv_error_flag == 1",
                    tags$h5("Please select exactly three columns above to select presence and absence codes", style = "color: red;")
                  ),
                  conditionalPanel(
                    condition = "output.eval_csv_error_flag == 2",
                    tags$h5("There are more than 50 unique codes for the currently selected validation data column;",
                            "please select a different column or select \"Counts (numerical)\" for",
                            tags$em("Validation data type"),
                            style = "color: red;")
                  ),
                  fluidRow(
                    column(6, uiOutput("eval_csv_codes_p_uiOut_select")),
                    column(6, uiOutput("eval_csv_codes_a_uiOut_select"))
                  )
                ),
                textOutput("eval_csv_data_text")
              )
            ),
            uiOutput("eval_csv_execute_uiOut_button")
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
                  tags$h5("There are more than 50 unique codes for the currently selected validation data column;",
                          "please select a different column or select \"Counts (numerical)\" for",
                          tags$em("Validation data type"),
                          style = "color: red;")
                ),
                fluidRow(
                  column(6, uiOutput("eval_gis_codes_p_uiOut_select")),
                  column(6, uiOutput("eval_gis_codes_a_uiOut_select"))
                )
              ),
              textOutput("eval_data_gis_text")
            )
          ),
          uiOutput("eval_gis_execute_uiOut_button"),
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
                helpText("Import validation data in order to calculate metrics")
              ),
              conditionalPanel(
                condition = "output.eval_display_calc_metrics_flag",
                fluidRow(
                  column(
                    width = 6,
                    tags$strong("Validation data info"),
                    textOutput("table_eval_pts_filename_out"),
                    tableOutput("table_eval_pts_out"),
                    # tags$style(type="text/css", "#table_eval_pts_out td:first-child {font-weight:bold;}")
                    #tr:first-child for first row
                    column(12, tags$span(textOutput("table_eval_pts_countmessage_out"), style = "color: blue"))
                  ),
                  column(
                    width = 5, offset = 1,
                    uiOutput("eval_metrics_which_uiOut_check"),
                    tags$br(),
                    actionButton("eval_metrics_execute", "Calculate metrics"),
                    tags$br(), tags$br(),
                    textOutput("eval_metrics_text"),
                    tags$span(textOutput("eval_metrics_message"), style = "color: blue")
                  )
                )
              )
            ),
            ############################################################################### Metric descriptions and references
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
                      tags$p(tags$strong("Description:"),
                             "The area under the receiver operating characteristic (ROC) function (AUC)",
                             "is a threshold-independent metric that evaluates the percentage of the time",
                             "a random selection from the positive group will have a score",
                             "greater than a random selection from the negative class.",
                             "Specifically, AUC is calculated as the area under the curve of the ROC plot,",
                             "which is generated by plotting the sensitivity values (y axis) by",
                             "one minus the specificity values (x axis) for all possible thresholds."),
                      "See", tags$a("Fielding and Bell (1997)", href = "https://doi.org/10.1017/S0376892997000088"),
                      "for more information."
                    )
                  ),
                  conditionalPanel(
                    condition = "input.eval_metrics_description == 2",
                    helpText(
                      tags$p(tags$strong("Description:"),
                             "The true skill statistic (TSS) is a threshold-dependent measure (formula below),",
                             "meaning it depends a presence/absence threshold for converting continuous predictions ",
                             "to binary predictions.",
                             "This threshold is determined using the sensitivity-specificity maximization approach",
                             "(", tags$a(HTML(paste("Liu et al. 2005")),
                                         href = "https://doi.org/10.1111/j.0906-7590.2005.03957.x"), ").",
                             "Note that this approach is equivalent to determining the maximum TSS using all possible thresholds.",
                             "TSS is recommended for use in place of kappa because unlike kappa,",
                             "TSS is not inherently dependent on prevalence."),
                      tags$p("TSS = sensitivity (true positive rate) + specificity (true negative rate) - 1"),
                      "See", tags$a(HTML(paste("Allouche et al. 2006)")),
                                    href = "https://doi.org/10.1111/j.1365-2664.2006.01214.x"),
                      "for more information."
                    )
                  ),
                  conditionalPanel(
                    condition = "input.eval_metrics_description == 3",
                    helpText(
                      tags$p(tags$strong("Description:"),
                             "The root-mean-square error (RMSE) is a scale-dependent metric, which measures the",
                             "difference between predicted response variable values and observed values.",
                             "RMSE requires count data rather than presence/absence data. To calculate RMSE,",
                             "square the difference between the observed and predicted abundances for each validation data point.",
                             "Then take the square root of the mean of these squared error values."),
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
          title = "Metric Results", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
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
                         "The downloaded CSV file will have both metric values and information about each set of predictions.",
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
