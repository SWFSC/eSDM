### UI code for the 'Import Predictions' tab

ui.loadModels <- function() {
  tabItem(
    tabName = "loadModels",
    fluidRow(
      column(
        width = 5,
        fluidRow(
          box(
            title = "Import Predictions", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
            selectInput("model_load_type", tags$h5("Data file type"),  choices = file.type.list2, selected = 1, width = "50%"),
            ###############################################
            conditionalPanel(
              condition = "input.model_load_type == 1",
              ui.instructions.upload.csv(),
              ui.instructions.pred.csv(),
              fluidRow(
                column(6, fileInput("model_csv_file", label.csv.upload, accept = ".csv")),
                column(
                  width = 5, offset = 1,
                  selectInput("model_csv_pt_loc", tags$h5("Location of point in grid cell"),
                              choices = list("Center" = 1, "Top left" = 2, "Top right" = 3,
                                             "Bottom right" = 4, "Bottom left" = 5),
                              selected = 1)
                )
              ),
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
                      uiOutput("model_csv_names_var_uiOut_select")
                    ),
                    column(
                      width = 6,
                      uiOutput("model_csv_names_lat_uiOut_select"),
                      uiOutput("model_csv_pred_type_uiOut_select"),
                      uiOutput("model_csv_var_type_uiOut_select")
                    )
                  ),
                  fluidRow(
                    column(
                      width = 6,
                      uiOutput("model_csv_names_weight_uiOut_select"),
                      tags$br(),
                      uiOutput("model_create_csv_uiOut_button"),
                      tags$br()
                    ),
                    column(6, tags$h5(uiOutput("model_csv_NA_idx_uiOut_message"), style = "color: red;"))
                  ),
                  tags$span(textOutput("create_sf_csv_text"), style = "color: blue")
                )
              )
            ),
            ###############################################
            conditionalPanel(
              condition = "input.model_load_type == 2",
              ui.instructions.upload.raster(),
              ui.instructions.pred.raster(),
              fluidRow(
                column(6, fileInput("model_gis_raster_file", label.raster.upload, accept = c(".tif", ".img"))),
                column(
                  width = 5, offset = 1,
                  numericInput("model_gis_raster_band", tags$h5("Band number of prediction data"), value = 1, min = 1, step = 1)
                )
              ),
              conditionalPanel(
                condition = "output.read_model_gis_raster_flag == false",
                box(width = 12, ui.error.upload.raster)
              ),
              conditionalPanel(
                condition = "output.read_model_gis_raster_flag",
                helpText("Imported rasters can only have one data layer, and that layer",
                         "is treated as the prediction data. Thus, there is no weight data"),
                box(
                  width = 12,
                  fluidRow(
                    column(
                      width = 6,
                      uiOutput("model_gis_raster_pred_type_uiOut_select"),
                      tags$br(),
                      uiOutput("model_create_gis_raster_uiOut_button"),
                      tags$br()
                    ),
                    column(6, tags$h5(uiOutput("model_gis_raster_NA_idx_uiOut_message"), style = "color: red;"))
                  ),
                  tags$span(textOutput("create_sf_gis_raster_text"), style = "color: blue")
                )
              )
            ),
            ###############################################
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
                      uiOutput("model_gis_shp_names_var_uiOut_select")
                    ),
                    column(
                      width = 6,
                      uiOutput("model_gis_shp_pred_type_uiOut_select"),
                      uiOutput("model_gis_shp_var_type_uiOut_select")
                    )
                  ),
                  fluidRow(
                    column(
                      width = 6,
                      uiOutput("model_gis_shp_names_weight_uiOut_select"),
                      tags$br(),
                      uiOutput("model_create_gis_shp_uiOut_button"),
                      tags$br()
                    ),
                    column(6, tags$h5(uiOutput("model_gis_shp_NA_idx_uiOut_message"), style = "color: red;"))
                  ),
                  tags$span(textOutput("create_sf_gis_shp_text"), style = "color: blue")
                )
              )
            ),
            ###############################################
            conditionalPanel(
              condition = "input.model_load_type == 4",
              fluidRow(
                column(
                  width = 12,
                  ui.instructions.upload.gdb(),
                  ui.instructions.pred.shp.gdb(),
                  textInput("model_gis_gdb_path", label.gdb.path, value = ".../Sample_predictions_5_gdb.gdb")
                ),
                column(6, textInput("model_gis_gdb_name", label.gdb.name, value = "Sample_predictions_5_gdb")),
                column(6, tags$br(), tags$br(), actionButton("model_gis_gdb_load", label.gdb.upload))
              ),
              tags$br(),
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
                      uiOutput("model_gis_gdb_names_var_uiOut_select")
                    ),
                    column(
                      width = 6,
                      uiOutput("model_gis_gdb_pred_type_uiOut_select"),
                      uiOutput("model_gis_gdb_var_type_uiOut_select")
                    )
                  ),
                  fluidRow(
                    column(
                      width = 6,
                      uiOutput("model_gis_gdb_names_weight_uiOut_select"),
                      tags$br(),
                      uiOutput("model_create_gis_gdb_uiOut_button"),
                      tags$br()
                    ),
                    column(6, tags$h5(uiOutput("model_gis_gdb_NA_idx_uiOut_message"), style = "color: red;")
                    )
                  ),
                  tags$span(textOutput("create_sf_gis_gdb_text"), style = "color: blue")
                )
              )
            )
          )
        )
      ),

      column(
        width = 7,
        conditionalPanel(
          condition = "output.loadModels_display_flag",
          fluidRow(
            box(
              title = "Imported Original Predictions", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              ui.instructions.table.select(text.pre = "original", text.in = "with which to perform an action:"),
              conditionalPanel("input.models_loaded_table_stats != true", DTOutput("models_loaded_table")),
              conditionalPanel("input.models_loaded_table_stats", DTOutput("models_loaded_table_stats")),
              column(
                width = 12,
                checkboxInput("models_loaded_table_stats",
                              paste("Display additional information - NOTE that you can only select",
                                    "or deselect row(s) when this box is unchecked")),
                fluidRow(
                  column(3, radioButtons("model_select_action", tags$h5("Action to perform with selected original predictions"),
                                         choices = list("Plot interactive preview" = 1, "Plot static preview" = 2,
                                                        "Download static preview" = 3, "Remove from GUI" = 4),
                                         selected = 1)
                  ),
                  column(
                    width = 8, offset = 1,
                    conditionalPanel(
                      condition = "output.loaded_models_selected_flag == false",
                      tags$h5("Select at least one set of original predictions to perform an action", style = "color: red")
                    ),
                    conditionalPanel(
                      condition = "output.loaded_models_selected_flag",
                      tags$h5("Action option(s)"),
                      fluidRow(
                        box(
                          width = 12,
                          ###################################### Plot interactive preview
                          ui_interactive_preview("model"),

                          ###################################### Plot static preview
                          ui_static_preview("model"),

                          ###################################### Download static preview
                          ui_download_preview("model"),

                          ###################################### Remove predictions
                          ui_remove("model")
                        )
                      )
                    )
                  )
                )
              )
            ),

            conditionalPanel(
              condition = "input.model_select_action == 1",
              box(
                title = "Interactive Preview", status = "primary", solidHeader = TRUE,  width = 12, collapsible = TRUE,
                # shinycssloaders::withSpinner(leafletOutput("model_preview_interactive_plot", height = 500), type = 1)
                leafletOutput("model_preview_interactive_plot", height = 500)
              )
            ),
            conditionalPanel(
              condition = "input.model_select_action == 2",
              box(
                title = "Static Preview", status = "primary", solidHeader = TRUE,  width = 12, collapsible = TRUE,
                # shinycssloaders::withSpinner(plotOutput("model_preview_plot", height = 500), type = 1)
                plotOutput("model_preview_plot", height = 500)
              )
            )
          )
        )
      )
    )
  )
}
