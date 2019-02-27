### UI code for the 'Create Ensemble Predictions' tab

ui.createEns <- function() {
  tabItem(
    tabName = "createEns",
    conditionalPanel("output.ens_display_flag == false", ui.notice.no.pred.overlaid()),
    conditionalPanel(
      condition = "output.ens_display_flag",
      ######################################################################### Choose overlaid predictions to be used in ensemble
      fluidRow(
        box(
          title = "Overlaid Predictions", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
          tags$span(textOutput("create_ens_base_message"), style = "color: blue;"),
          conditionalPanel(
            condition = "input.create_ens_table_subset == false",
            tags$h5(tags$strong("Select overlaid predictions to ensemble:"),
                    "All sets of overlaid predictions will be used when creating ensemble predictions.",
                    "You can select specific sets of overlaid predictions to ensemble when",
                    tags$em("Create ensemble using a subset..."),
                    "is checked."),
            tableOutput("create_ens_table")
          ),
          conditionalPanel(
            condition = "input.create_ens_table_subset == true",
            ui.instructions.table.select(text.pre = "overlaid", text.in = "to ensemble:"),
            DTOutput("create_ens_datatable")
          ),
          column(12, checkboxInput("create_ens_table_subset", "Create ensemble using a subset of the sets of overlaid predictions"))
        )
      ),

      ######################################################################### Create Ensemble Predictions box
      fluidRow(
        box(
          title = "Create Ensemble Predictions", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
          conditionalPanel(
            condition = "output.ens_overlaid_selected_flag == false",
            tags$h5("Select at least two sets of overlaid predictions to create an ensemble", style = "color: red")
          ),
          conditionalPanel(
            condition = "output.ens_overlaid_selected_flag",
            fluidRow(
              ####################################################### Rescale predictions box
              column(
                width = 3,
                fluidRow(
                  box(
                    width = 12,
                    tags$strong("1) Ensemble options: rescaling method"),
                    # radioButtons("create_ens_rescale_type", NULL,
                    #              choices = list("None" = 1, "Abundance" = 2, "Normalization" = 3,
                    #                             "Standardization" = 4, "Sum to 1" = 5),
                    #              selected = 1),
                    radioButtons("create_ens_rescale_type", NULL,
                                 choices = list("None" = 1, "Abundance" = 2, "Sum to 1" = 3),
                                 selected = 1),
                    column(
                      width = 12,
                      conditionalPanel(
                        condition = "input.create_ens_rescale_type == 1",
                        helpText(tags$u("Description:"), "Overlaid predictions will not be changed"),
                        uiOutput("create_ens_rescale_type_message")
                      ),
                      conditionalPanel(
                        condition = "input.create_ens_rescale_type == 2",
                        numericInput("create_ens_rescale_abund", tags$h5("Abundance to which to rescale predictions"),
                                     value = 0, min = 0, step = 1),
                        helpText(tags$u("Description:"),
                                 "For each set of overlaid predictions, rescale them so that the predicted",
                                 "abundance is the value entered above")
                      ),
                      conditionalPanel(
                        condition = "input.create_ens_rescale_type == 3",
                        helpText(tags$u("Description:"),
                                 "For each set of overlaid predictions, rescale them so they sum to one")
                      )
                      # conditionalPanel(
                      #   condition = "input.create_ens_rescale_type == 3",
                      #   helpText(tags$u("Description:"),
                      #            "For each set of overlaid predictions, rescale them (X) into a range of [0, 1]",
                      #            "using the following formula:"),
                      #   column(
                      #     width = 12,
                      #     helpText(HTML(paste0("X", tags$sub("new")), "= (X -",
                      #                   paste0("X", tags$sub("min"), ")"), "/",
                      #                   paste0("(X", tags$sub("max"), " - X", tags$sub("min"), ")")))
                      #   )
                      # ),
                      # conditionalPanel(
                      #   condition = "input.create_ens_rescale_type == 4",
                      #   helpText(tags$u("Description:"),
                      #            "For each set of overlaid predictions, rescale them (X) to have a mean", HTML("(&mu;)"),
                      #            "of 0 and", "standard deviation", HTML("(&sigma;)"),
                      #            "of 1 (unit variance) using the following formula:"),
                      #   column(12, helpText(HTML(paste0("X", tags$sub("new")), "= (X - &mu;) / &sigma;")))
                      # ),
                      # conditionalPanel(
                      #   condition = "input.create_ens_rescale_type == 5",
                      #   helpText(tags$u("Description:"),
                      #            "For each set of overlaid predictions, rescale them so they sum to one")
                      # )
                    )
                  ),
                  ################################################### Exclusion polygon preview
                  conditionalPanel(
                    condition = "input.create_ens_reg",
                    box(
                      title = "Preview exclusion polygon(s)", width = 12, status = "primary", solidHeader = TRUE,
                      collapsible = TRUE,
                      conditionalPanel(
                        condition = "output.create_ens_weighted_poly_flag == false",
                        helpText("No exclusion polygons have been assigned")
                      ),
                      conditionalPanel(
                        condition = "output.create_ens_weighted_poly_flag",
                        tags$h5("Select overlaid predictions to preview with their exclusion polygon(s)"),
                        fluidRow(
                          column(6, uiOutput("create_ens_reg_preview_model_uiOut_select")),
                          column(6, uiOutput("create_ens_reg_preview_execute_uiOut_button"))
                        ),
                        helpText("The overlaid predictions will be black and the exclusion polygon(s) will have a red border"),
                        shinycssloaders::withSpinner(plotOutput("create_ens_reg_preview_plot"), type = 1)
                      )
                    )
                  )
                )
              ),
              column(
                width = 7,
                fluidRow(
                  ####################################################### Regional exclusion
                  box(
                    width = 12,
                    tags$strong("2) Ensemble options: regional exclusion"),
                    checkboxInput("create_ens_reg", "Exclude specific regions of overlaid predictions when creating ensemble", value = FALSE),
                    conditionalPanel(
                      condition = "input.create_ens_reg",
                      box(
                        width = 4,
                        # helpText(tags$strong("Regional exclusion:"),
                        #          "Exclude specific regions of overlaid predictions when creating ensemble"),
                        helpText(tags$strong("Exclusion polygon(s)"),
                                 "Import and assign exclusion polygon(s) to overlaid predictions.",
                                 "Area(s) of the specified predictions that intersect with the imported exclusion polygon will",
                                 "not be included in the ensemble",
                                 "You may import and assign multiple polygons to exclude multiple regions for a single set of predictions.",
                                 "However, exclusion polygons must not overlap.", tags$br(),
                                 "All predictions not assigned an exclusion polygon will be included in the ensemble."),
                        uiOutput("create_ens_reg_model_uiOut_selectize"),
                        selectInput("create_ens_reg_type", tags$h5("Exclusion polygon file type"),
                                    choices = file.type.list1, selected = 1)
                      ),
                      box(
                        width = 8,
                        ############## File type: csv
                        conditionalPanel(
                          condition = "input.create_ens_reg_type == 1",
                          ui.instructions.upload.csv(),
                          ui.instructions.poly.csv.single(),
                          ui.instructions.ens.weightpolyNA(),
                          fileInput("create_ens_reg_csv_file", label.csv.upload, accept = ".csv")
                        ),
                        ############## File type: shp
                        conditionalPanel(
                          condition = "input.create_ens_reg_type == 2",
                          ui.instructions.upload.shp(),
                          ui.instructions.ens.weightpolyNA(),
                          fileInput("create_ens_reg_shp_files", label.shp.upload, multiple = TRUE),
                          conditionalPanel("output.create_ens_reg_shp_flag == false", ui.error.upload.shp)
                        ),
                        ############## File type: gdb
                        conditionalPanel(
                          condition = "input.create_ens_reg_type == 3",
                          ui.instructions.upload.gdb(),
                          ui.instructions.ens.weightpolyNA(),
                          fluidRow(
                            column(
                              width = 6,
                              textInput("create_ens_reg_gdb_path", label.gdb.path, value = ".../folder.gdb"),
                              actionButton("create_ens_reg_gdb_load", label.gdb.upload)
                            ),
                            column(6, textInput("create_ens_reg_gdb_name", label.gdb.name, value = ""))
                          ),
                          conditionalPanel("output.create_ens_reg_gdb_flag == false", ui.error.upload.gdb)
                        ),
                        ############## General
                        sliderInput("create_ens_reg_coverage",
                                    tags$h5("Overlap percentage: Percentage of prediction polygon that must intersect with",
                                            "the exclusion polygon(s) for the prediction polygon to be excluded.",
                                            "If '0' is selected then the prediction polygon will be excluded if there is any overlap."),
                                    min = 0, max = 100, value = 100),
                        tags$br(),
                        actionButton("create_ens_reg_add_execute", "Assign exclusion polygon to selected predictions"),
                        tags$span(textOutput("create_ens_reg_add_text"), style = "color: blue")
                      ),
                      box(
                        width = 12,
                        helpText(tags$strong("Assigned exculsion polygons")),
                        conditionalPanel(
                          condition = "output.create_ens_weighted_poly_flag == false",
                          helpText("No weight polygons have been assigned")
                        ),
                        conditionalPanel(
                          condition = "output.create_ens_weighted_poly_flag",
                          fluidRow(
                            column(6, tableOutput("create_ens_reg_table_out")),
                            column(
                              width = 6,
                              uiOutput("create_ens_reg_remove_choices_uiOut_select"),
                              actionButton("create_ens_reg_remove_execute", "Remove selected exclusion polygons"),
                              tags$span(textOutput("create_ens_reg_remove_text"), style = "color: blue")
                            )
                          )
                        )
                      )
                    )
                  ),

                  ####################################################### Ensembling method
                  box(
                    width = 12,
                    fluidRow(
                      column(
                        width = 4,
                        tags$strong("3) Ensemble options: ensembling method"),
                        radioButtons("create_ens_type", NULL, choices = list("Unweighted" = 1, "Weighted" = 2), selected = 1),
                        column(
                          width = 12,
                          conditionalPanel(
                            condition = "input.create_ens_type == 2",
                            radioButtons("create_ens_weight_type", tags$h5("Weighted ensembling method"),
                                         choices = list("Manual entry" = 1, "Evaluation metric" = 2,
                                                        "Pixel-level spatial weights" = 3),
                                         selected = 1)
                          )
                        )
                      ),
                      column(
                        width = 8,
                        fluidRow(
                          ##################################### Unweighted ensembling options
                          conditionalPanel(
                            condition = "input.create_ens_type == 1",
                            box(
                              width = 12,
                              helpText(tags$strong("Unweighted ensembling method:"),
                                       "Calculate the mean of all corresponding predictions")
                            )
                          ),

                          ##################################### Weighted ensembling options
                          conditionalPanel(
                            condition = "input.create_ens_type == 2",

                            ######################### Weighting by manual entry
                            conditionalPanel(
                              condition = "input.create_ens_weight_type == 1",
                              box(
                                width = 12,
                                helpText(tags$strong("Weighted ensembling method:"),
                                         "Calculate the weighted mean of all corresponding predictions"),
                                helpText(tags$strong("Manual entry method:"),
                                         "Entered weights correspond to the order of the models in the overlaid predictions table.",
                                         "Weights must sum to 1 but may be entered as either decimals or fractions.",
                                         "They must be entered in the following format:",
                                         "'weight, weight, ..., weight', e.g. '0.5, 1/4, 1/4'."),
                                uiOutput("create_ens_weight_manual_uiOut_text") #,
                                # tags$span(uiOutput("create_ens_weight_manual_warning_uiOut_text"), style = "color: red;")
                              )
                            ),
                            ######################### Weighting by evaluation metric
                            conditionalPanel(
                              condition = "input.create_ens_weight_type == 2",
                              box(
                                width = 12,
                                helpText(tags$strong("Weighted ensembling method:"),
                                         "Calculate the weighted mean of all corresponding predictions"),
                                helpText(tags$strong("Evaluation metric method:"),
                                         "The relative weights are the metric values rescaled so that the maximum value is one."),
                                uiOutput("create_ens_weights_metric_uiOut_text"),
                                fluidRow(
                                  column(4, uiOutput("create_ens_weights_metric_uiOut_radio")),
                                  column(8, tableOutput("create_ens_weights_metric_table_out"))
                                )
                              )
                            ),
                            ######################### Weighting by pixel-level spatial weights
                            conditionalPanel(
                              condition = "input.create_ens_weight_type == 3",
                              box(
                                width = 12,
                                helpText(tags$strong("Weighted ensembling method:"),
                                         "Calculate the weighted mean of all corresponding predictions"),
                                helpText(tags$strong("Pixel-level spatial weights method:"),
                                         "Overlaid predictions are multiplied by their corresponding spatial weight. These pixel-level",
                                         "spatial weights were specified by the 'Column with weight data' input",
                                         "when each set of original predictions was initially imported into the GUI.",
                                         "If a set of overlaid predictions does not have pixel-level spatial weights, ",
                                         "then the row corresponding to that set will say \"No\" in the table below and",
                                         "those predictions will have a weight of one when the ensemble is created"),
                                tableOutput("create_ens_weights_pix_table_out")
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              ####################################################### Create ensemble button
              box(
                width = 2,
                tags$strong("4) Create ensemble"),
                tags$br(), tags$br(),
                uiOutput("create_ens_create_action_uiOut_button"),
                tags$br(),
                tags$span(uiOutput("ens_create_ensemble_text"), style = "color: blue")
              )
            )
          )
        )
      ),

      ######################################################################### Created ensemble predictions
      conditionalPanel(
        condition = "output.ens_display_ens_flag",
        fluidRow(
          box(
            title = "Created Ensemble Predictions", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE,
            ui.instructions.table.select(text.pre = "ensemble", text.in = "with which to perform an action:"),
            DTOutput("ens_datatable_ensembles"),
            tags$br(),
            column(
              width = 4,
              radioButtons("ens_select_action", tags$h5("Action to perform with selected ensemble predictions"),
                           choices = list("Plot interactive preview" = 1, "Plot static preview" = 2,
                                          "Download static preview" = 3, "Remove from GUI" = 4,
                                          "Calculate predicted abundance" = 5, "Plot among-model variance" = 6),
                           selected = 1)
            ),
            column(
              width = 8,
              conditionalPanel(
                condition = "output.ens_models_selected_flag == false",
                tags$span(tags$h5("Select at least one set of ensemble predictions to perform an action"), style = "color: red")
              ),
              conditionalPanel(
                condition = "output.ens_models_selected_flag",
                tags$h5("Action option(s)"),
                fluidRow(
                  box(
                    width = 12,
                    ####################################### Plot interactive ensemble preview
                    conditionalPanel(
                      condition = "input.ens_select_action == 1",
                      fluidRow(
                        column(4, radioButtons("ens_preview_interactive_perc", tags$h5("Units"),
                                               choices = list("Percentages" = 1, "Values" = 2), selected = 1)),
                        column(8, tags$br(), tags$br(), uiOutput("ens_preview_interactive_execute_uiOut_button"))
                      ),
                      helpText("Note that if you are not connected to the internet then the background map will not display")
                    ),
                    ####################################### Plot static ensemble preview
                    conditionalPanel(
                      condition = "input.ens_select_action == 2",
                      fluidRow(
                        column(4, radioButtons("ens_preview_perc", tags$h5("Units"),
                                               choices = list("Percentages" = 1, "Values" = 2),
                                               selected = 1)),
                        column(8, tags$br(), tags$br(), actionButton("ens_preview_execute", "Plot static preview"))
                      )
                    ),
                    ####################################### Download static ensemble preview
                    conditionalPanel(
                      condition = "input.ens_select_action == 3",
                      fluidRow(
                        column(4, radioButtons("ens_download_preview_perc", tags$h5("Units"),
                                               choices = list("Percentages" = 1, "Values" = 2),
                                               selected = 1)),
                        column(4, radioButtons("ens_download_preview_res", tags$h5("Resolution"),
                                               choices = list("High (300 ppi)" = 1, "Low (72 ppi)" = 2),
                                               selected = 1)),
                        column(4, radioButtons("ens_download_preview_format", tags$h5("File format"),
                                               choices = list("JPEG" = 1, "PDF" = 2, "PNG" = 3),
                                               selected = 3))
                      ),
                      radioButtons("ens_download_preview_dim", tags$h5("File dimensions"),
                                   choices = list("8 in by 8 in" = 1, "Dimensions of 'Static Preview' window" = 2),
                                   selected = 1),
                      uiOutput("ens_download_preview_name_uiOut_text"),
                      uiOutput("ens_download_preview_execute_uiOut_download")

                    ),
                    ####################################### Remove ensemble(s)
                    conditionalPanel(
                      condition = "input.ens_select_action == 4",
                      actionButton("ens_remove_execute", "Remove selected ensemble predictions"),
                      textOutput("ens_remove_text")
                    ),
                    ####################################### Calculate abundance of ensemble(s)
                    conditionalPanel(
                      condition = "input.ens_select_action == 5",
                      uiOutput("ens_calc_abund_execute_uiOut_text"),
                      fluidRow(
                        column(6, uiOutput("ens_calc_abund_execute_uiOut_button")),
                        column(6, tableOutput("ens_abund_table_out"))
                        # tags$style(type = "text/css", "#ens_abund_table_out td:first-child {font-weight:bold;}")
                        # #tr:first-child for first row
                      )
                    ),
                    conditionalPanel(
                      condition = "input.ens_select_action == 6",
                      helpText("Plot the ensemble predictions and standard deviation of the rescaled overlaid predictions side-by-side.",
                               "Both plot will have the same scale"),
                      uiOutput("ens_var_execute_uiOut_button")
                    )
                  )
                )
              )
            )
          ),
          ################################################# Preview of ensemble(s)
          box(
            title = "Ensemble Preview", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE,
            conditionalPanel(
              condition = "input.ens_select_action == 1",
              shinycssloaders::withSpinner(leafletOutput("ens_preview_interactive_plot", height = 500), type = 1)
            ),
            conditionalPanel(
              condition = "input.ens_select_action == 2",
              shinycssloaders::withSpinner(plotOutput("ens_preview_plot", height = 500), type = 1)
            ),
            conditionalPanel(
              condition = "input.ens_select_action == 6",
              shinycssloaders::withSpinner(plotOutput("ens_var_plot", height = 500), type = 1)
            )
          )
        )
      )
    )
  )
}
