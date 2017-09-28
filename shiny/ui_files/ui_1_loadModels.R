### UI code for the 'Load Model Predictions' tab

ui.loadModels <- function() {
  tabItem(
    tabName = "loadModels", 
    fluidRow(
      column(
        width = 5, 
        fluidRow(
          box(
            title = "Load and Save App Environment", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE, 
            fluidRow(
              column(
                width = 6, 
                fileInput("load_app_envir_file", h5("Load saved app environment"), accept = ".RDATA"), 
                textOutput("load_envir_text")
              ), 
              column(
                width = 6, 
                textInput("save_app_envir_name", h5("Filename with which to save environment"), value = "Ens_App_Save_Envir.RDATA"), 
                downloadButton("save_app_envir", "Save current app environment")
              )
            )
          ), 
          box(
            title = "Load Model Predictions", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE, 
            fluidRow(
              column(6, selectInput("model_load_type", h5("Data file type"),  choices = file.type.list2, selected = 1)), 
              column(
                width = 5, offset = 1, 
                conditionalPanel(
                  condition = "input.model_load_type == 1", 
                  selectInput("model_csv_pt_loc", h5("Location of point in grid cell"), 
                              choices = list("Center" = 1, "Top left" = 2, "Top right" = 3, "Bottom right" = 4, "Bottom left" = 5), 
                              selected = 1)
                ), 
                conditionalPanel(
                  condition = "input.model_load_type == 2", 
                  numericInput("model_gis_raster_band", h5("Band number of prediction data"), value = 1, min = 1, step = 1)
                )
              )
            ), 
            conditionalPanel(
              condition = "input.model_load_type == 1", 
              ui.instructions.upload.csv(), 
              ui.instructions.pred.csv(), 
              fileInput("model_csv_file", label.csv.upload, accept = ".csv"), 
              conditionalPanel(
                condition = "output.read_model_csv_flag == false", 
                box(width = 12, ui.error.upload.csv)
              ), 
              conditionalPanel(
                condition = "output.read_model_csv_flag", 
                box(
                  width = 12, 
                  fluidRow(
                    column(
                      width = 6, 
                      uiOutput("model_csv_names_lon_uiOut_select"), 
                      uiOutput("model_csv_names_pred_uiOut_select"), 
                      # uiOutput("model_csv_names_error_uiOut_select"), 
                      uiOutput("model_csv_names_weight_uiOut_select"), 
                      br(), 
                      uiOutput("model_create_csv_uiOut_button"), 
                      br()
                    ), 
                    column(
                      width = 6, 
                      uiOutput("model_csv_names_lat_uiOut_select"), 
                      uiOutput("model_csv_pred_type_uiOut_select"), 
                      br(), 
                      br(), 
                      uiOutput("model_csv_NA_idx_uiOut_message")
                    ), 
                    column(12, textOutput("create_spdf_csv_text"))
                  )
                )
              )
            ), 
            
            conditionalPanel(
              condition = "input.model_load_type == 2", 
              ui.instructions.upload.raster(), 
              ui.instructions.pred.raster(), 
              fileInput("model_gis_raster_file", label.raster.upload, accept = ".tif"), 
              conditionalPanel(
                condition = "output.read_model_gis_raster_flag == false", 
                box(width = 12, ui.error.upload.raster)
              ), 
              conditionalPanel(
                condition = "output.read_model_gis_raster_flag", 
                helpText("Since loaded rasters can only have one data layer, that data is loaded as the prediction data", 
                         "and there are no error or weight columns to select"), 
                box(
                  width = 12, 
                  fluidRow(
                    column(
                      width = 6, 
                      uiOutput("model_gis_raster_pred_type_uiOut_select"), 
                      br(), 
                      uiOutput("model_create_gis_raster_uiOut_button"), 
                      br()
                    ), 
                    column(
                      width = 6, 
                      ui.new.line(), 
                      uiOutput("model_gis_raster_NA_idx_uiOut_message"), 
                      br()
                    ), 
                    column(12, textOutput("create_spdf_gis_raster_text"))
                  )
                )
              )
            ), 
            conditionalPanel(
              condition = "input.model_load_type == 3", 
              ui.instructions.upload.shp(), 
              ui.instructions.pred.shp.gdb(), 
              fileInput("model_gis_shp_files", label.shp.upload, multiple = TRUE), 
              conditionalPanel(
                condition = "output.read_model_gis_shp_flag == false", 
                box(width = 12, ui.error.upload.shp)
              ), 
              conditionalPanel(
                condition = "output.read_model_gis_shp_flag", 
                box(
                  width = 12, 
                  fluidRow(
                    column(
                      width = 6, 
                      uiOutput("model_gis_shp_names_pred_uiOut_select"), 
                      # uiOutput("model_gis_shp_names_error_uiOut_select"), 
                      uiOutput("model_gis_shp_names_weight_uiOut_select"), 
                      br(), 
                      uiOutput("model_create_gis_shp_uiOut_button"), 
                      br()
                    ), 
                    column(
                      width = 6, 
                      uiOutput("model_gis_shp_pred_type_uiOut_select"), 
                      br(), 
                      br(), 
                      uiOutput("model_gis_shp_NA_idx_uiOut_message")
                    ), 
                    column(
                      width = 12, 
                      textOutput("create_spdf_gis_shp_text"), 
                      textOutput("create_spdf_gis_shp_res_text")
                    )
                  )
                )
              )
            ), 
            conditionalPanel(
              condition = "input.model_load_type == 4", 
              fluidRow(
                column(
                  width = 12, 
                  ui.instructions.upload.gdb(), 
                  ui.instructions.pred.shp.gdb(), 
                  textInput("model_gis_gdb_path", label.gdb.path, value = ".../folder.gdb")
                ), 
                column(6, textInput("model_gis_gdb_name", label.gdb.name, value = "")), 
                column(6, br(), br(), actionButton("model_gis_gdb_load", label.gdb.upload)
                )
              ), 
              br(), 
              conditionalPanel(
                condition = "output.read_model_gis_gdb_flag == false", 
                box(width = 12, ui.error.upload.gdb)
              ), 
              conditionalPanel(
                condition = "output.read_model_gis_gdb_flag", 
                box(
                  width = 12, 
                  fluidRow(
                    column(
                      width = 6, 
                      uiOutput("model_gis_gdb_names_pred_uiOut_select"), 
                      # uiOutput("model_gis_gdb_names_error_uiOut_select"), 
                      uiOutput("model_gis_gdb_names_weight_uiOut_select"), 
                      br(), 
                      uiOutput("model_create_gis_gdb_uiOut_button"), 
                      br()
                    ), 
                    column(
                      width = 6, 
                      uiOutput("model_gis_gdb_pred_type_uiOut_select"), 
                      br(), 
                      br(), 
                      uiOutput("model_gis_gdb_NA_idx_uiOut_message")
                    ), 
                    column(12, textOutput("create_spdf_gis_gdb_text"))
                  )
                )
              )
            )
          )
        )
      ), 
      
      column(width = 7, 
             conditionalPanel(
               condition = "output.loadModels_display_flag", 
               fluidRow(
                 box(
                   title = "Loaded Model Predictions", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE, 
                   conditionalPanel("input.models_loaded_table_stats != true", DT::dataTableOutput("models_loaded_table")), 
                   conditionalPanel("input.models_loaded_table_stats", DT::dataTableOutput("models_loaded_table_stats")), 
                   column(
                     width = 12, 
                     fluidRow(
                       column(4, checkboxInput("models_loaded_table_stats", "Display additional information")), 
                       column(
                         width = 8, 
                         conditionalPanel(
                           condition = "input.models_loaded_table_stats != true", 
                           helpText("Click on row(s) to select model predictions to perform an action", br(), 
                                    "If multiple rows are selected and the 'Preview' button is clicked, " , 
                                    "then the app will generate a multiplot of all selected predictions")
                         ), 
                         conditionalPanel(
                           condition = "input.models_loaded_table_stats", 
                           helpText("Rows can only be selected if 'Display additional information' is unchecked", br(), 
                                    "'Resolution' information is approximate; please note any errors in the feedback form")
                         )
                       )
                     ), 
                     br(), 
                     fluidRow(
                       column(3, radioButtons("model_select_action", h5("Action to perform with selected model predictions"), 
                                              choices = list("Plot preview" = 1, "Download preview" = 2, "Remove from app" = 3), 
                                              selected = 1)
                       ), 
                       
                       column(
                         width = 8, offset = 1, 
                         h5("Action option(s)"), 
                         fluidRow(
                           box(
                             width = 12, 
                             conditionalPanel(
                               condition = "input.model_select_action == 1", 
                               column(3, radioButtons("model_preview_perc", h5("Preview model predictions using"), 
                                                      choices = list("Percentages" = 1, "Values" = 2), 
                                                      selected = 1)), 
                               column(
                                 width = 3, 
                                 ui.new.line(), 
                                 actionButton("model_pix_preview_execute", "Preview selected model predictions")
                               )
                             ), 
                             conditionalPanel(
                               condition = "input.model_select_action == 2", 
                               fluidRow(
                                 column(3, radioButtons("model_download_preview_perc", h5("Units"), 
                                                        choices = list("Percentages" = 1, "Values" = 2), 
                                                        selected = 1)), 
                                 column(3, radioButtons("model_download_preview_res", h5("Resolution"), 
                                                        choices = list("High (300 ppi)" = 1, "Low (72 ppi)" = 2), 
                                                        selected = 2)), 
                                 column(3, radioButtons("model_download_preview_format", h5("Image file format"), 
                                                        choices = list("JPEG" = 1, "PDF" = 2, "PNG" = 3), 
                                                        selected = 3))
                               ), 
                               fluidRow(
                                 column(9, uiOutput("model_download_preview_name_uiOut_text")), 
                                 column(3, downloadButton("model_download_preview_execute", "Download"))
                               )
                             ), 
                             conditionalPanel(
                               condition = "input.model_select_action == 3", 
                               column(3, actionButton("model_remove_execute", "Remove selected model predictions"))
                             )
                           )
                         )
                       )
                     )
                   )
                 ), 
                 
                 box(
                   title = "Preview", status = "primary", solidHeader = TRUE,  width = 12, collapsible = TRUE, 
                   shinycssloaders::withSpinner(plotOutput("model_pix_preview_plot"), type = 1)
                 )
               )
             )
      )
    )
  )
}