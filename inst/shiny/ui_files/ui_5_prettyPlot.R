### UI code for the 'High Quality Maps' tab

ui.prettyPlot <- function() {
  tabItem(
    tabName = "prettyPlot",
    conditionalPanel(condition = "output.pretty_display_flag == false", ui.notice.no.pred.original()),
    conditionalPanel(
      condition = "output.pretty_display_flag",
      fluidRow(
        box(
          title = "High Quality Maps", solidHeader = TRUE, status = "primary", height = 570, width = 12, align = "center",
          shinycssloaders::withSpinner(plotOutput("pretty_plot_plot_out", height = 500), type = 1)
        )
      ),

      ################################################################# Map Control
      fluidRow(
        box(
          title = "Map Control", solidHeader = FALSE, status = "warning", width = 12, collapsible = TRUE,
          fluidRow(
            column(
              width = 2,
              tags$strong("1) Select map control option"),
              fluidRow(
                box(
                  width = 12,
                  radioButtons("pretty_plot_mapcontrol", NULL,
                               choices = list("Add new map to to-plot list" = 1, "Update parameters of map in to-plot list" = 2,
                                              "Plot or download map(s) in to-plot list" = 3),
                               selected = 1)
                )
              )
            ),

            #-----------------------------------------------
            conditionalPanel(
              condition = "input.pretty_plot_mapcontrol == 1",
              column(
                width = 6,
                tags$strong("2) Select predictions to add to the to-plot list and specify parameters below"),
                fluidRow(
                  box(
                    width = 12,
                    ui.instructions.multipletables.select(
                      text.in = "add to the to-plot list:", sel.num = 1,
                      text.other = "Map parameters will appear after you select a set of predictions."
                    ),
                    DTOutput("pretty_table_orig_out"),
                    tags$br(),
                    DTOutput("pretty_table_over_out"),
                    tags$br(),
                    DTOutput("pretty_table_ens_out")
                  )
                )
              ),
              column(
                width = 4,
                tags$strong("3) Specify map ID and add map to the to-plot list"),
                fluidRow(
                  box(
                    width = 12,
                    uiOutput("pretty_plot_toplot_add_id_uiOut_text"),
                    tags$br(), tags$br(),
                    uiOutput("pretty_plot_toplot_add_execute_uiOut_button"),
                    textOutput("pretty_plot_toplot_add_text")
                  )
                )
              )
            ),
            #-----------------------------------------------
            conditionalPanel(
              condition = "input.pretty_plot_mapcontrol == 2",
              column(
                width = 6,
                tags$strong("2) Select map from to-plot list and update parameters below"),
                fluidRow(
                  box(
                    width = 12,
                    tags$h5(tags$strong("Select a map from the to-plot list to update:"),
                            "Click on a row in the table below to select or deselect items.",
                            "When you select a map from the to-plot list, the parameters will appear and will",
                            "reflect the current saved parameters of that map."),
                    DTOutput("pretty_plot_update_table_out")
                  )
                )
              ),
              column(
                width = 4,
                tags$strong("3) Click button to update saved parameters"),
                fluidRow(
                  box(
                    width = 12,
                    actionButton("pretty_plot_update_exectue", "Update map parameters"),
                    textOutput("pretty_plot_update_text"),
                    helpText("todo")
                  )
                )
              )
            ),
            #-----------------------------------------------
            conditionalPanel(
              condition = "input.pretty_plot_mapcontrol == 3",
              column(
                width = 6,
                tags$strong("2) Select map(s) in to-plot list to plot"),
                fluidRow(
                  box(
                    width = 12,
                    tags$h5(tags$strong("Select map(s) from the to-plot list to plot:"),
                            "Click on a row in the table below to select or deselect items.",
                            "Maps will be plotted left to right, top to bottom, in order you select them"),
                    DTOutput("pretty_plot_toplot_table_out")
                  )
                )
              ),
              column(
                width = 4,
                tags$strong("3) Specify plot dimensions and plot map"),
                fluidRow(
                  box(
                    width = 12,
                    fluidRow(
                      column(4, numericInput("pretty_plot_nrow", tags$h5("Number of rows"), value = 1, step = 1, min = 0)),
                      column(4, numericInput("pretty_plot_ncol", tags$h5("Number of columns"), value = 1, step = 1, min = 0))
                    ),
                    fluidRow(
                      column(
                        width = 12,
                        actionButton("pretty_plot_plot_event", "Plot map"),
                        textOutput("pretty_plot_plot_text"),
                        helpText("Note that plotting may take several minutes depending on map size and the number of maps being plotted")
                      )
                    )
                  )
                )
              )
            )

            #   ################################################ Download map
            #   tags$strong("3) Download map"),
            #   fluidRow(
            #     box(
            #       width = 12,
            #       tags$h5("The map displayed above will be the map that is downloaded"),
            #       fluidRow(
            #         column(3, radioButtons("pretty_plot_download_res", tags$h5("Resolution"),
            #                                choices = list("High (300 ppi)" = 1, "Low (72 ppi)" = 2),
            #                                selected = 1)),
            #         column(2, radioButtons("pretty_plot_download_format", tags$h5("Image file format"),
            #                                choices = list("JPEG" = 1, "PDF" = 2, "PNG" = 3),
            #                                selected = 3)),
            #         column(6, uiOutput("pretty_plot_download_name_uiOut_text"))
            #       ),
            #       tags$br(),
            #       downloadButton("pretty_plot_download_execute", "Download map")
            #     )
            #   )
            # )
          )
        )
      ),

      conditionalPanel(
        condition = "output.pretty_params_display_flag",
        ################################################################# Map Parameters - Section 1
        fluidRow(
          box(
            title = "Map Parameters - Section 1", solidHeader = FALSE, status = "warning", width = 12, collapsible = TRUE,
            fluidRow(
              ################################################## Map coordinate system and range
              column(
                width = 4,
                tags$strong("Map coordinate system and range"),
                fluidRow(
                  box(
                    width = 12,
                    checkboxInput("pretty_plot_proj_ll", "Generate the map in WGS 84 geographic coordinates (decimal degrees)",
                                  value = TRUE),
                    conditionalPanel(
                      condition = "input.pretty_plot_proj_ll == false",
                      box(
                        width = 12,
                        radioButtons("pretty_plot_proj_method", NULL, #tags$h5("Overlay coordinate system"),
                                     choices = list("Generate map in the native coordinate system of the selected SDM" = 1,
                                                    "Select SDM with desired coordinate system" = 2,
                                                    "Enter numeric EPSG code" = 3),
                                     selected = 1),
                        column(
                          width = 12,
                          conditionalPanel("input.pretty_plot_proj_method == 2", uiOutput("pretty_plot_proj_idx_uiOut_select")),
                          conditionalPanel(
                            condition = "input.pretty_plot_proj_method == 3",
                            numericInput("pretty_plot_proj_epsg", tags$h5("EPSG code"), value = 4326, step = 1)
                          )
                        )
                      )
                    ),
                    helpText("Map range values have the same units as the specified coordinate system, e.g.",
                             "if the specified coordinate system is WGS 84 geographic coordinates then the values are",
                             "decimal degrees and must have a longitude range of [-180, 180] and a latitude range of [-90, 90]"),
                    fluidRow(
                      column(
                        width = 6,
                        uiOutput("pretty_plot_range_xmin_uiOut_num"),
                        uiOutput("pretty_plot_range_ymin_uiOut_num")
                      ),
                      column(
                        width = 6,
                        uiOutput("pretty_plot_range_xmax_uiOut_num"),
                        uiOutput("pretty_plot_range_ymax_uiOut_num")
                      )
                    )
                  )
                )
              ),
              ################################################## Color scheme of predictions
              column(
                width = 4,
                tags$strong("Background color and prediction color scheme"),
                fluidRow(
                  box(
                    width = 12,
                    colourpicker::colourInput("pretty_plot_background_color", tags$h5("Click to select background color"),
                                              showColour = "background"),
                    tags$br(), tags$br(),
                    fluidRow(
                      column(
                        width = 8,
                        radioButtons("pretty_plot_color_perc", tags$h5("Color-code predictions by:"),
                                     choices = list("Relative percentage" = 1, "Numerical value" = 2)),
                        uiOutput("pretty_plot_color_palette_uiOut_select"),
                        uiOutput("pretty_plot_color_num_uiOut_num")
                      ),
                      column(
                        width = 4,
                        tags$span(tags$h5("Color scheme preview"), style = "text-align: right"),
                        plotOutput("pretty_plot_color_preview_plot", width = "85px", height =  "250px")
                      )
                    )#,
                    # checkboxInput("pretty_plot_color_na_transparent", "Color NA predictions as transparent", value = TRUE),
                    # conditionalPanel(
                    #   condition = "input.pretty_plot_color_na_transparent == false",
                    #   fluidRow(
                    #     column(
                    #       width = 6,
                    #       colourpicker::colourInput("pretty_plot_color_na", tags$h5("Click to select color for NA predictions"),
                    #                                 showColour = "background", value = "grey")),
                    #     column(
                    #       width = 6,
                    #       tags$br(), tags$br(),
                    #       actionButton("pretty_plot_color_na_reset_execute", "Reset NA color to white")
                    #     )
                    #   )
                    # )
                  )
                )
              ),
              ################################################## Legend
              column(
                width = 4,
                tags$strong("Legend"),
                fluidRow(
                  box(
                    width = 12,
                    checkboxInput("pretty_plot_legend", "Include legend with the map", value = TRUE),
                    conditionalPanel(
                      condition = "input.pretty_plot_legend",
                      box(
                        width = 12,
                        fluidRow(
                          column(6, radioButtons("pretty_plot_legend_inout", tags$h5("Place legend:"),
                                                 choices = list("Inside map frame" = 1, "Outside map frame" = 2), selected = 1)),
                          column(6, uiOutput("pretty_plot_legend_pos_uiOut_select"))
                        ),
                        conditionalPanel(
                          condition = "input.pretty_plot_legend_inout == 2",
                          helpText("Legend width is the horizontal proportion of the plot window taken up by the legend")
                        ),
                        fluidRow(
                          column(6, uiOutput("pretty_plot_legend_width_uiOut_numeric")),
                          column(6, numericInput("pretty_plot_legend_size", tags$h5("Legend text size"),
                                                 value = 1.0, min = 0.1, step = 0.1))
                        ),
                        fluidRow(
                          column(6, checkboxInput("pretty_plot_legend_frame", "Include frame around legend", value = TRUE)),
                          column(
                            width = 6,
                            conditionalPanel(
                              condition = "input.pretty_plot_color_perc == 2",
                              numericInput("pretty_plot_legend_round", tags$h5("Legend labels: number of decimals"),
                                           value = 3, min = 1, step = 1)
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),

        ################################################################# Map Parameters - Section 2
        fluidRow(
          box(
            title = "Map Parameters - Section 2", solidHeader = FALSE, status = "warning", width = 12, collapsible = TRUE,
            fluidRow(
              ################################################## Title and axis labels
              column(
                width = 4,
                tags$strong("Title and axis labels"),
                fluidRow(
                  box(
                    width = 12,
                    helpText("Delete the text in the input boxes to remove the title or axis labels"),
                    uiOutput("pretty_plot_title_uiOut_text"),
                    fluidRow(
                      column(6, textInput("pretty_plot_xlab", tags$h5("X-axis label"), value = "Longitude")),
                      column(6, textInput("pretty_plot_ylab", tags$h5("Y-axis label"), value = "Latitude"))
                    ),
                    helpText("Size values are relative to 1 (the default size)"),
                    fluidRow(
                      column(6, numericInput("pretty_plot_title_cex", tags$h5("Title size (value is relative to 1)"),
                                             value = 1.3, step = 0.1)),
                      column(6, numericInput("pretty_plot_lab_cex", tags$h5("Axis label size (value is relative to 1)"),
                                             value = 1, step = 0.1))
                    )
                  )
                )
              ),
              ################################################## Tick marks and tick labels
              column(
                width = 4,
                tags$strong("Coordinate grid lines and labels"),
                fluidRow(
                  box(
                    width = 12,
                    checkboxInput("pretty_plot_tick", "Include coordinate grid lines", value = TRUE),
                    conditionalPanel(
                      condition = "input.pretty_plot_tick",
                      box(
                        width = 12,
                        helpText("Size and width values are relative to 1 (the default size)"),
                        fluidRow(
                          column(
                            width = 6,
                            uiOutput("pretty_plot_tick_lon_start_uiOut_numeric"),
                            uiOutput("pretty_plot_tick_lat_start_uiOut_numeric"),
                            numericInput("pretty_plot_tick_lw", tags$h5("Grid line width"), value = 1.0, min = 0.1, step = 0.1),
                            colourpicker::colourInput("pretty_plot_tick_color", tags$h5("Click to select color for coordinate grid lines"),
                                                      value = "#D6D6D6", showColour = "background")
                          ),
                          column(
                            width = 6,
                            uiOutput("pretty_plot_tick_lon_interval_uiOut_numeric"),
                            uiOutput("pretty_plot_tick_lat_interval_uiOut_numeric"),
                            numericInput("pretty_plot_tick_alpha", tags$h5("Grid line transparency (alpha value)"),
                                         value = 1.0, min = 0, max = 1, step = 0.1)
                          )
                        )
                      )
                    ),
                    conditionalPanel(
                      condition = "input.pretty_plot_tick",
                      checkboxInput("pretty_plot_tick_label_inc", tags$h5("Include coordinate labels"), value = TRUE),
                      conditionalPanel(
                        condition = "input.pretty_plot_tick_label_inc",
                        box(
                          width = 12,
                          fluidRow(
                            column(6, radioButtons("pretty_plot_tick_label_inout", tags$h5("Coordinate label location"),
                                                   choices = list("Inside frame" = 1, "Outside frame" = 2), selected = 1)),
                            column(6, numericInput("pretty_plot_tick_label_size", tags$h5("Coordinate label size"),
                                                   value = 1.0, min = 0.1, step = 0.1))
                          )
                        )
                      )
                    )
                  )
                )
              ),
              ################################################## Additional polygons and points
              column(
                width = 4,
                tags$strong("Additional objects (points or polygons)"),
                fluidRow(
                  box(
                    width = 12,
                    checkboxInput("pretty_plot_addobj",
                                  "Include additional objects in map; uncheck this box to remove all additional objects",
                                  value = FALSE),
                    conditionalPanel(
                      condition = "input.pretty_plot_addobj",

                      #----------------------------------------
                      column(12, tags$h5("Loaded additional object(s)")),
                      box(
                        width = 12,
                        DTOutput("pretty_plot_addobj_table_out"),
                        tags$br(), tags$br(),
                        uiOutput("pretty_plot_addobj_remove_execute_uiOut_button"),
                        textOutput("pretty_plot_addobj_remove_out")
                      ),

                      #----------------------------------------
                      column(12, tags$h5("New additional object")),
                      box(
                        width = 12,
                        uiOutput("pretty_plot_addobj_which_uiOut_select"),
                        conditionalPanel(
                          condition = "input.pretty_plot_addobj_which == 4",
                          box(
                            width = 12,
                            selectInput("pretty_plot_addobj_own_type", tags$h5("File type"),
                                        choices = file.type.list1, selected = 1),
                            ############## File type: csv
                            conditionalPanel(
                              condition = "input.pretty_plot_addobj_own_type == 1",
                              ui.instructions.upload.csv(),
                              helpText("The first column must contain the longitude values, and the second column must",
                                       "contain the latitude values. The longitudes and latitudes must",
                                       "be in WGS 84 geographic coordinates.",
                                       "For polygons objects, multiple polygons may be demarcated rows with blank cells",
                                       "or cells with 'NA' entries"),
                              fileInput("pretty_plot_addobj_own_csv_file", label.csv.upload, accept = ".csv"),
                              conditionalPanel("output.pretty_plot_addobj_own_csv_flag == false", ui.error.upload.csv)
                            ),
                            # ############## File type: raster
                            # conditionalPanel(
                            #   condition = "input.pretty_plot_addobj_own_type == 2",
                            #   ui.instructions.upload.raster(),
                            #   fileInput("pretty_plot_addobj_own_raster_file", label.raster.upload, accept = ".tif")
                            #   # conditionalPanel("output.create_ens_weights_poly_raster_flag == false", ui.error.upload.raster)
                            # ),
                            ############## File type: shp
                            conditionalPanel (
                              condition = "input.pretty_plot_addobj_own_type == 2",
                              ui.instructions.upload.shp(),
                              fileInput("pretty_plot_addobj_own_shp_files", label.shp.upload, multiple = TRUE)
                              # conditionalPanel("output.create_ens_weights_poly_shp_flag == false", ui.error.upload.shp)
                            ),
                            ############## File type: gdb
                            conditionalPanel(
                              condition = "input.pretty_plot_addobj_own_type == 3",
                              ui.instructions.upload.gdb(),
                              fluidRow(
                                column(6, textInput("create_ens_weights_poly_gdb_path", label.gdb.path, value = ".../folder.gdb")),
                                column(6, textInput("create_ens_weights_poly_gdb_name", label.gdb.name, value = ""))
                              ),
                              fluidRow(
                                column(6, actionButton("create_ens_weights_poly_gdb_load", "Upload")),
                                column(6, conditionalPanel("output.create_ens_weights_poly_gdb_flag == false", ui.error.upload.gdb))
                              )
                            )
                          )
                        ),
                        fluidRow(
                          column(6, uiOutput("pretty_plot_addobj_type_uiOut_radio")),
                          column(6, radioButtons("pretty_plot_addobj_order", tags$h5("Draw object before or after SDM:"),
                                                 choices = list("Before" = 1, "After" = 2), selected = 2))
                        ),
                        fluidRow(
                          column(6, uiOutput("pretty_plot_addobj_color_ptfillcheck_uiOut_check")),
                          column(6, uiOutput("pretty_plot_addobj_color_absbordercheck_uiOut_check"))
                        ),
                        fluidRow(
                          column(6, column(12, uiOutput("pretty_plot_addobj_color_ptfill_uiOut_colour"))),
                          column(6, column(12, uiOutput("pretty_plot_addobj_color_absborder_uiOut_colour")))
                        ),
                        fluidRow(
                          column(6, uiOutput("pretty_plot_addobj_pchlty_uiOut_select")),
                          column(6, uiOutput("pretty_plot_addobj_cexlwd_uiOut_numeric"))
                        ),
                        actionButton("pretty_plot_addobj_add_execute", "Add additional object"),
                        textOutput("pretty_plot_addobj_add_out")
                      )
                    )
                  )
                )
              )
            )
          )
        )
        ################################################################# End of Map Parameters - Section 2

      )
    )
  )
}
