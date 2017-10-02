### UI code for the 'Create Ensemble Predictions' tab

ui.createEns <- function() {
  tabItem(
    tabName = "createEns", 
    conditionalPanel("output.ens_display_flag == false", ui.notice.no.pred.overlaid()), 
    conditionalPanel(
      condition = "output.ens_display_flag", 
      fluidRow(
        ##################################################################### Choose overlaid models to be udes in ensemble
        box(
          title = "Overlaid Model Predictions", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE, 
          conditionalPanel("input.create_ens_table_subset == false", tableOutput("create_ens_table")), 
          conditionalPanel("input.create_ens_table_subset == true", DT::dataTableOutput("create_ens_datatable")), 
          column(4, checkboxInput("create_ens_table_subset", "Create ensemble using a subset of the overlaid models")), 
          column(
            width = 8, 
            conditionalPanel(
              condition = "input.create_ens_table_subset", 
              helpText("Click on a row to select models to use in ensemble")
            )
          )
        )
      ), 
      
      fluidRow(
        ##################################################################### Ensembling method box
        box(
          title = "Create Ensemble Predictions", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE, 
          fluidRow(
            box(
              width = 7, 
              fluidRow(
                column(
                  width = 4, 
                  strong("Ensembling method"), 
                  radioButtons("create_ens_type", NULL, choices = list("Unweighted" = 1, "Weighted" = 2), selected = 1), 
                  column(
                    width = 12, 
                    conditionalPanel(
                      condition = "input.create_ens_type == 2", 
                      radioButtons("create_ens_weight_type", h5("Weighted ensembling method"), 
                                   choices = list("Manual entry" = 1, "Evaluation metric" = 2, 
                                                  "Pixel-level spatial weights" = 3, "Polygon(s) with weights" = 4), 
                                   selected = 1)
                    )
                  )
                ), 
                column(
                  width = 8, 
                  br(), 
                  conditionalPanel(
                    condition = "output.ens_overlaid_selected_flag == false", 
                    strong("Please select at least two sets of overlaid model predictions to ensemble")
                  ), 
                  
                  fluidRow(
                    #################################################### Unweighted ensembling options
                    conditionalPanel(
                      condition = "output.ens_overlaid_selected_flag && input.create_ens_type == 1", 
                      box(
                        width = 12, 
                        helpText(strong("Unweighted ensembling method:"), 
                                 "Calculate the simple mean of all predictions in each grid cell.")
                      )
                    ), 
                    
                    #################################################### Weighted ensembling options
                    conditionalPanel(
                      condition = "output.ens_overlaid_selected_flag && input.create_ens_type == 2", 
                      
                      ################################## Weighting by manual entry
                      conditionalPanel(
                        condition = "input.create_ens_weight_type == 1", 
                        box(
                          width = 12, 
                          helpText(strong("Weighted ensembling method:"), 
                                   "Calculate the weighted mean of all predictions in each grid cell."), 
                          helpText(strong("Manual entry method:"), 
                                   "Entered weights correspond to the order of the models in the", 
                                   "overlaid model predictions table.", 
                                   "Weights must be entered in the following format: 'weight, weight, ..., weight'."), 
                          uiOutput("create_ens_weight_manual_uiOut_text")
                        )
                      ), 
                      ################################## Weighting by evaluation metric
                      conditionalPanel(
                        condition = "input.create_ens_weight_type == 2", 
                        box(
                          width = 12, 
                          helpText(strong("Weighted ensembling method:"), 
                                   "Calculate the weighted mean of all predictions in each grid cell."), 
                          helpText(strong("Evaluation metric method:"), 
                                   "The table displays both the evaluation metric values and the relative weights for the models.", 
                                   "The relative weights are the metric values rescalued so that the maximum value is one."), 
                          conditionalPanel(
                            condition = "output.create_ens_weights_metric_flag == false", 
                            helpText(strong("No metrics have been calculated for the selected overlaid model predictions"), br(), 
                                     "To use this weighting method, please go to the 'Evaluation Metrics' tab and, ", 
                                     "for all of the overlaid models you plan to use in the ensemble, calculate", 
                                     "the metric you wish to use as a weight.")
                          ), 
                          conditionalPanel(
                            condition = "output.create_ens_weights_metric_flag", 
                            fluidRow(
                              column(width = 4, uiOutput("create_ens_weights_metric_uiOut_radio")), 
                              column(width = 8, tableOutput("create_ens_weights_metric_table_out"))
                            )
                          )
                        )
                      ), 
                      ################################## Weighting by pixel-level spatial weights
                      conditionalPanel(
                        condition = "input.create_ens_weight_type == 3", 
                        box(
                          width = 12, 
                          helpText(strong("Weighted ensembling method:"), 
                                   "Calculate the weighted mean of all predictions in each grid cell."), 
                          helpText(strong("Pixel-level spatial weights method:"), 
                                   "Overlaid predictions are multiplied by their corresponding spatial weight. These pixel-level", 
                                   "spatial weights were specified by the 'Column with weight data' input", 
                                   "when each set of model predictions was initally loaded into the app.", 
                                   "If a set of overlaid model predictions does not have pixel-level spatial weights, ", 
                                   "then the row corresponding to that set will say \"No\" in the table below and", 
                                   "those predictions will have a weight of one when the enseble is created"), 
                          conditionalPanel(
                            condition = "output.create_ens_weights_pix_flag == false", 
                            helpText(strong("At least one of the selected overlaid predictions", 
                                            "must have pixel-level spatial weights"))
                          ), 
                          conditionalPanel(
                            condition = "output.create_ens_weights_pix_flag", 
                            tableOutput("create_ens_weights_pix_table_out")
                          )
                        )
                      ), 
                      ################################## Weighting by polygon spatial weights
                      conditionalPanel(
                        condition = "input.create_ens_weight_type == 4", 
                        box(
                          width = 12, 
                          helpText(strong("Weighted ensembling method:"), 
                                   "Calculate the weighted mean of all predictions in each grid cell."), 
                          helpText(strong("Polygon(s) with weights method"), 
                                   "Select one or more sets of overlaid model predictions and load weight polygons", 
                                   "to assign to the selected sets of predictions. These weight polygons designate area(s)", 
                                   "in which the selected sets of predictions will be weighted.", 
                                   "Currently you may only assign one weight for each weight polygon, but you may load", 
                                   "multiple polygons to apply unique weights to multiple prediction regions", 
                                   "for one set of predictions. However, these polygons must not overlap.", 
                                   "All predictions whose grid cell overlaps with a weight polygon by less than the percentage", 
                                   "specified below, including those with no overlap, will have a weight of one."), 
                          fluidRow(
                            column(7, uiOutput("create_ens_weights_poly_model_uiOut_selectize")), 
                            column(5, selectInput("create_ens_weights_poly_type", h5("File type"), 
                                                  choices = file.type.list2, selected = 1))
                          ), 
                          box(
                            width = 12, 
                            ##################### File type: csv
                            conditionalPanel(
                              condition = "input.create_ens_weights_poly_type == 1", 
                              ui.instructions.upload.csv(), 
                              ui.instructions.poly.csv(), 
                              fluidRow(
                                column(6, fileInput("create_ens_weights_poly_csv_file", label.csv.upload, accept = ".csv")), 
                                column(
                                  width = 4, offset = 1, 
                                  conditionalPanel(
                                    condition = "output.create_ens_weights_poly_csv_flag == false", 
                                    ui.new.line(), 
                                    ui.error.upload.csv
                                  ), 
                                  conditionalPanel(
                                    condition = "output.create_ens_weights_poly_csv_flag", 
                                    numericInput("create_ens_weights_poly_csv_weight", h5("Weight for csv polygon(s)"), 
                                                 min = 0, value = 1, step = 0.1)
                                  )
                                )
                              )
                            ), 
                            ##################### File type: raster
                            conditionalPanel(
                              condition = "input.create_ens_weights_poly_type == 2", 
                              ui.instructions.upload.raster(), 
                              fluidRow(
                                column(6, fileInput("create_ens_weights_poly_raster_file", label.raster.upload, accept = ".tif")), 
                                column(
                                  width = 5, offset = 1, 
                                  conditionalPanel(
                                    condition = "output.create_ens_weights_poly_raster_flag == false", 
                                    ui.new.line(), 
                                    ui.error.upload.raster
                                  )
                                )
                              ), 
                              fluidRow(
                                column(6, radioButtons("create_ens_weights_poly_raster_weight_type", NULL, 
                                                       choices = list("Enter single weight for area covered by raster" = 1, 
                                                                      "Select band number with weights" = 2), 
                                                       selected = 1)), 
                                column(
                                  width = 6, 
                                  conditionalPanel(
                                    condition = "input.create_ens_weights_poly_raster_weight_type == 1", 
                                    numericInput("create_ens_weights_poly_raster_weight", 
                                                 h5("Weight for area covered by raster"), 
                                                 value = 1, min = 0, step = 0.1)
                                  ), 
                                  conditionalPanel(
                                    condition = "input.create_ens_weights_poly_raster_weight_type == 2", 
                                    helpText("This functionality is not yet implemented")
                                    # numericInput("create_ens_weights_poly_raster_band", h5("Band number of weight(s)"), 
                                    #              value = 1, min = 1, step = 1)
                                  )
                                )
                              )
                            ), 
                            ##################### File type: shp
                            conditionalPanel(
                              condition = "input.create_ens_weights_poly_type == 3", 
                              ui.instructions.upload.shp(), 
                              fluidRow(
                                column(6, fileInput("create_ens_weights_poly_shp_files", label.shp.upload, multiple = TRUE)), 
                                column(
                                  width = 5, offset = 1, 
                                  conditionalPanel(
                                    condition = "output.create_ens_weights_poly_shp_flag == false", 
                                    ui.error.upload.shp
                                  )
                                )
                              ), 
                              fluidRow(
                                conditionalPanel(
                                  condition = "output.create_ens_weights_poly_shp_flag", 
                                  column(6, radioButtons("create_ens_weights_poly_shp_weight_type", NULL, 
                                                         choices = list("Enter single weight for area covered by shapefile" = 1, 
                                                                        "Select data column with weights" = 2), 
                                                         selected = 2)), 
                                  
                                  column(
                                    width = 6, 
                                    conditionalPanel(
                                      condition = "input.create_ens_weights_poly_shp_weight_type == 1", 
                                      numericInput("create_ens_weights_poly_shp_weight", 
                                                   h5("Weight for area covered by shapefile"), 
                                                   value = 1, min = 0, step = 0.1)
                                    ), 
                                    conditionalPanel(
                                      condition = "input.create_ens_weights_poly_shp_weight_type == 2", 
                                      helpText("This functionality is not yet implemented")
                                      # uiOutput("create_ens_weights_poly_shp_field_uiOut_select") 
                                    )
                                  )
                                )
                              )
                            ), 
                            ##################### File type: gdb
                            conditionalPanel(
                              condition = "input.create_ens_weights_poly_type == 4", 
                              ui.instructions.upload.gdb(), 
                              fluidRow(
                                column(6, textInput("create_ens_weights_poly_gdb_path", label.gdb.path, value = ".../folder.gdb")), 
                                column(6, textInput("create_ens_weights_poly_gdb_name", label.gdb.name, value = ""))
                              ), 
                              fluidRow(
                                column(6, actionButton("create_ens_weights_poly_gdb_load", label.gdb.upload)), 
                                column(
                                  width = 6, 
                                  conditionalPanel(
                                    condition = "output.create_ens_weights_poly_gdb_flag == false", 
                                    ui.error.upload.gdb
                                  )
                                )
                              ), 
                              fluidRow(
                                conditionalPanel(
                                  condition = "output.create_ens_weights_poly_gdb_flag", 
                                  column(6, radioButtons("create_ens_weights_poly_gdb_weight_type", NULL, 
                                                         choices = list("Enter single weight for area covered by .gdb file" = 1, 
                                                                        "Select data column with weights" = 2), 
                                                         selected = 2)), 
                                  column(
                                    width = 6, 
                                    conditionalPanel(
                                      condition = "input.create_ens_weights_poly_gdb_weight_type == 1", 
                                      numericInput("create_ens_weights_poly_gdb_weight", 
                                                   h5("Weight for area covered by file geodatabase file"), 
                                                   value = 1, min = 0, step = 0.1)
                                    ), 
                                    conditionalPanel(
                                      condition = "input.create_ens_weights_poly_gdb_weight_type == 2", 
                                      helpText("This functionality is not yet implemented")
                                      # uiOutput("create_ens_weights_poly_gdb_field_uiOut_select")
                                    )
                                  )
                                )
                              )
                            )
                          ), 
                          sliderInput("create_ens_weights_poly_coverage", 
                                      h5("Percentage of prediction polygon that must be covered by the weight polygon(s)", 
                                         "for the prediction polygon to be weighted.", 
                                         "If '0' is selected then the prediction polygon will be weighted if there is any overlap."), 
                                      min = 0, max = 100, value = 100), 
                          fluidRow(
                            column(6, actionButton("create_ens_weights_poly_add_execute", 
                                                   "Assign loaded weight polygon to selected prediction(s)")), 
                            column(4, offset = 1, textOutput("create_ens_weights_poly_add_text"))
                          )
                        )
                      )
                    )
                  )
                )
              ), 
              
              ################################## Weighting by polygon spatial weights pt 2
              conditionalPanel(
                condition = "input.create_ens_type == 2 && output.ens_overlaid_selected_flag && input.create_ens_weight_type == 4", 
                h5("Summary table of loaded polygon file(s) and weight(s) for models to be used in ensemble"), 
                fluidRow(
                  column(7, tableOutput("create_ens_weights_poly_table_out")), 
                  column(
                    width = 5, 
                    uiOutput("create_ens_weights_poly_remove_choices_uiOut_select"), 
                    actionButton("create_ens_weights_poly_remove_execute", "Remove selected weight polygons"), 
                    textOutput("create_ens_weights_poly_remove_text")
                  )
                )
                # tags$style(type="text/css", "#create_ens_weights_poly_table_out td:first-child {font-weight:bold;}")
              )
            ), 
            
            ##################################################################### Rescale predictions box
            column(
              width = 3, 
              box(
                width = 12, 
                strong("Rescaling method"), 
                conditionalPanel(
                  condition = "output.ens_rescale_none_flag == false", 
                  helpText(strong("Note: All prediction types are not \"Absolute density\", ", 
                                  "and thus the model predictions must be rescaled"))
                ), 
                uiOutput("create_ens_rescale_type_uiOut_radio"), 
                column(
                  width = 12, 
                  conditionalPanel(
                    condition = "input.create_ens_rescale_type == 1", 
                    helpText(strong("Description: Model predictions will not be changed"))
                  ), 
                  conditionalPanel(
                    condition = "input.create_ens_rescale_type == 2", 
                    numericInput("create_ens_rescale_abund", h5("Abundance to which to rescale predictions"), 
                                 value = 0, min = 0, step = 1), 
                    helpText(strong("Description: For each model, rescale predictions so that the predicted", 
                                    "abundance is the value entered above"))
                  ), 
                  conditionalPanel(
                    condition = "input.create_ens_rescale_type == 3", 
                    helpText(strong("Description: For each model, rescale predictions (X) into a range of [0, 1]", 
                                    "using the following formula:")), 
                    column(12, helpText(HTML(paste0("X", tags$sub("new")), "= (X -", 
                                             paste0("X", tags$sub("min"), ")"), "/", 
                                             paste0("(X", tags$sub("max"), " - X", tags$sub("min"), ")"))))
                  ), 
                  conditionalPanel(
                    condition = "input.create_ens_rescale_type == 4", 
                    helpText(strong("Description: For each model, rescale predictions (X) to have a mean", HTML("(&mu;)"), 
                                    "of 0 and", "standard deviation", HTML("(&sigma;)"), 
                                    "of 1 (unit variance) using the following formula:")), 
                    column(12, helpText(HTML(paste0("X", tags$sub("new")), "= (X - &mu;) / &sigma;")))
                  ), 
                  conditionalPanel(
                    condition = "input.create_ens_rescale_type == 5", 
                    helpText(strong("Description: For each model, rescale the predictions so that they sum to one"))
                  )
                )
              ), 
              conditionalPanel(
                condition = "input.create_ens_type == 2 && input.create_ens_weight_type == 4", 
                box(
                  width = 12, 
                  helpText(strong("Polygon(s) with weights method (cont)")), 
                  helpText("Preview weighted polygons for selected overlaid predictions"), 
                  fluidRow(
                    column(8, uiOutput("create_ens_weights_poly_preview_model_uiOut_select")), 
                    column(4, br(), br(), actionButton("create_ens_weights_poly_preview_execute", "Plot preview"))
                  ), 
                  shinycssloaders::withSpinner(plotOutput("create_ens_weights_poly_preview_plot"), type = 1)
                )
              )
            ), 
            ##################################################################### Create ensemble button
            box(
              width = 2, 
              actionButton("create_ens_create_action", "Create ensemble"), 
              ui.new.line(), 
              uiOutput("ens_create_ensemble_text")
            )
          )
        )
      ), 
      
      ############################################################################### Created ensemble models
      conditionalPanel(
        condition = "output.ens_display_ens_flag",  # This flag also checks if any ensemble predictions have been created
        fluidRow(
          box(
            title = "Created Ensemble Predictions", status = "warning", solidHeader = FALSE, width = 6, collapsible = TRUE, 
            DT::dataTableOutput("ens_datatable_ensembles"), 
            column(12, helpText("Click on row(s) to select model(s) to perform an action")), 
            column(3, radioButtons("ens_select_action", h5("Select action to perform with selected ensemble predictions"), 
                                   choices = list("Plot preview" = 1, "Download preview" = 2, "Remove from app" = 3, 
                                                  "Calculate predicted abundance" = 4), 
                                   selected = 1)), 
            column(
              width = 8, offset = 1, 
              br(), 
              h5("Action option(s)"), 
              fluidRow(
                box(
                  width = 12, 
                  ####################################### Preview ensemble(s)
                  conditionalPanel(
                    condition = "input.ens_select_action == 1", 
                    column(3, radioButtons("ens_preview_perc", h5("Units"), choices = list("Percentages" = 1, "Values" = 2), 
                                           selected = 1)), 
                    column(3, ui.new.line(), actionButton("ens_preview_execute", "Preview selected ensemble predictions"))
                  ), 
                  ####################################### Download preview of ensemble(s)
                  conditionalPanel(
                    condition = "input.ens_select_action == 2", 
                    fluidRow(
                      column(3, radioButtons("ens_download_preview_perc", h5("Units"), 
                                             choices = list("Percentages" = 1, "Values" = 2), 
                                             selected = 1)), 
                      column(3, radioButtons("ens_download_preview_res", h5("Resolution"), 
                                             choices = list("High (300 ppi)" = 1, "Low (72 ppi)" = 2), 
                                             selected = 2)), 
                      column(3, radioButtons("ens_download_preview_format", h5("Image file format"), 
                                             choices = list("JPEG" = 1, "PDF" = 2, "PNG" = 3), 
                                             selected = 3)), 
                      column(3, br(), br(), br(), downloadButton("ens_download_preview_execute", "Download"))
                    ), 
                    fluidRow(
                      column(9, uiOutput("ens_download_preview_name_uiOut_text"))
                    )
                  ), 
                  ####################################### Remove ensemble(s)
                  conditionalPanel(
                    condition = "input.ens_select_action == 3", 
                    column(
                      width = 3, 
                      actionButton("ens_remove_execute", "Remove selected ensemble predictions"), 
                      uiOutput("ens_remove_text")
                    )
                  ), 
                  ####################################### Calculate abundance of ensemble(s)
                  conditionalPanel(
                    condition = "input.ens_select_action == 4", 
                    uiOutput("ens_calc_abund_execute_uiOut_button"), 
                    br(), 
                    br(), 
                    tableOutput("ens_abund_table_out"), 
                    tags$style(type="text/css", "#ens_abund_table_out td:first-child {font-weight:bold;}")
                    #tr:first-child for first row
                  )
                )
              )
            )
          ), 
          ################################################# Preview of ensemble(s)
          box(
            title = "Ensemble Preview", status = "primary", solidHeader = TRUE, width = 6, collapsible = TRUE, 
            shinycssloaders::withSpinner(plotOutput("ens_pix_preview_plot"), type = 1)
          )
        )
      )
    )
  )
}