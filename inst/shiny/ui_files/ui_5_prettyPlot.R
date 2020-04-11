### UI code for the 'High Quality Maps' tab

ui.prettyPlot <- function() {
  tabItem(
    tabName = "prettyPlot",
    conditionalPanel(condition = "output.pretty_display_flag == false", ui.notice.no.pred.general()),
    conditionalPanel(
      condition = "output.pretty_display_flag",
      fluidRow(
        uiOutput("pretty_display")
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
                  radioButtons("pretty_mapcontrol", NULL,
                               choices = list("Save new map" = 1, "Update saved map parameters" = 2,
                                              "Plot or download saved map(s)" = 3),
                               selected = 1)
                )
              )
            ),

            #--------------------------------------------------------
            conditionalPanel(
              condition = "input.pretty_mapcontrol == 1",
              column(
                width = 6,
                tags$strong("2) Select predictions and specify parameters below"),
                fluidRow(
                  box(
                    width = 12,
                    ui.instructions.multipletables.select(
                      text.in = "map:", sel.num = 1,
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
                tags$strong("3) Specify map ID and save map"),
                fluidRow(
                  # shinycssloaders::withSpinner(uiOutput("pretty_save_map"), type = 3, color.background = "white")
                  uiOutput("pretty_save_map")
                  # output$pretty_save_map is in 'server_render.R'
                )
              )
            ),
            #--------------------------------------------------------
            conditionalPanel(
              condition = "input.pretty_mapcontrol == 2",
              column(
                width = 5,
                tags$strong("2) Select saved map to update"),
                fluidRow(
                  box(
                    width = 12,
                    tags$h5(tags$strong("Select saved map to update"),
                            "Click on a row in the table below to select or deselect a saved map.",
                            "Then click the 'Update saved map parameters' button in the next box."),
                    DTOutput("pretty_update_table_out")
                  )
                )
              ),
              column(
                width = 5,
                tags$strong("3) Update saved map parameters"),
                fluidRow(
                  box(
                    width = 12,
                    conditionalPanel(
                      condition = "output.pretty_display_toplot_flag == false",
                      tags$h5("You must have saved at least one map to use this section", style = "color: red;")
                    ),
                    conditionalPanel(
                      condition = "output.pretty_display_toplot_flag",
                      fluidRow(
                        column(6, actionButton("pretty_update_toplot_show", "Update saved map parameters")),
                        column(6, actionButton("pretty_toplot_remove_execute", "Remove saved map"))
                      ),
                      tags$br(),
                      tags$span(textOutput("pretty_update_message"), style = "color: blue;"),
                      textOutput("pretty_toplot_remove_text") #in "_toplot.R"
                    )
                  )
                )
              )
            ),
            #--------------------------------------------------------
            conditionalPanel(
              condition = "input.pretty_mapcontrol == 3",
              column(
                width = 5,
                tags$strong("2) Select saved map(s) to plot or download"),
                fluidRow(
                  box(
                    width = 12,
                    tags$h5(tags$strong("Select saved map(s) to plot or download"),
                            "Click on a row in the table below to select or deselect saved maps.",
                            "Maps will be plotted left to right, top to bottom, in order you select them"),
                    DTOutput("pretty_toplot_table_out"),
                    helpText(NULL)
                  )
                )
              ),
              column(
                width = 5,
                tags$strong("3) Specify plot dimensions and plot or download map(s)"),
                fluidRow(
                  box(
                    width = 12,
                    conditionalPanel(
                      condition = "output.pretty_display_toplot_flag == false",
                      tags$h5("You must have saved at least one map to use this section", style = "color: red;")
                    ),
                    conditionalPanel(
                      condition = "output.pretty_display_toplot_flag",
                      fluidRow(
                        column(3, tags$h5("Number of rows")),
                        column(3, tags$h5("Number of columns")),
                        column(3, tags$h5("Plot width (inches)")),
                        column(3, tags$h5("Plot height (inches)"))
                      ),
                      fluidRow( # to keep all input boxes on the same line
                        column(3, numericInput("pretty_nrow", NULL, value = 1, step = 1, min = 1)),
                        column(3, numericInput("pretty_ncol", NULL, value = 1, step = 1, min = 1)),
                        column(3, numericInput("pretty_width_inch", NULL, value = 8, step = 1, min = 1)),
                        column(3, numericInput("pretty_height_inch", NULL, value = 8, step = 1, min = 1))
                      ),
                      tags$br(),
                      tags$h5("If not running the GUI locally, save your workspace in case of a server timeout",
                              "while plotting or downloading map(s)",
                              style = "color: blue;"),
                      fluidRow(
                        box(
                          width = 6,
                          helpText("The plot will likely have the dimensions specified above; the plotting functions used may change",
                                   "the number of rows or columns depending on the plot width and height. See the manual for more details.",
                                   tags$br(), tags$br(),
                                   "Plotting may take several minutes depending on the number of maps and their size."),
                          tags$span(textOutput("pretty_plot_dim_warnings_out"), style = "color: red;"),
                          actionButton("pretty_plot_event", "Plot map(s)"),
                          textOutput("pretty_plot_text")
                        ),
                        box(
                          width = 6,
                          helpText("The downloaded image will have the dimensions specified above."),
                          fluidRow(
                            column(6, radioButtons("pretty_download_res", tags$h5("Resolution"),
                                                   choices = list("High (300 ppi)" = 1, "Low (72 ppi)" = 2),
                                                   selected = 1)),
                            column(6, radioButtons("pretty_download_format", tags$h5("File format"),
                                                   choices = list("JPEG" = 1, "PDF" = 2, "PNG" = 3),
                                                   selected = 3))
                          ),
                          uiOutput("pretty_download_name_uiOut_text"),
                          tags$br(),
                          uiOutput("pretty_download_execute_uiOut_download")
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
                    checkboxInput("pretty_proj_ll", "Generate the map in WGS 84 geographic coordinates (decimal degrees)",
                                  value = TRUE),
                    conditionalPanel(
                      condition = "input.pretty_proj_ll == false",
                      box(
                        width = 12,
                        radioButtons("pretty_proj_method", NULL, #tags$h5("Overlay coordinate system"),
                                     choices = list("Generate map in the native coordinate system of the selected SDM" = 1,
                                                    "Select predictions with desired coordinate system" = 2,
                                                    "Enter numeric EPSG code" = 3),
                                     selected = 1),
                        column(
                          width = 12,
                          conditionalPanel("input.pretty_proj_method == 2", uiOutput("pretty_proj_idx_uiOut_select")),
                          conditionalPanel(
                            condition = "input.pretty_proj_method == 3",
                            numericInput("pretty_proj_epsg", tags$h5("EPSG code"), value = 4326, step = 1)
                          )
                        )
                      )
                    ),
                    helpText("Map range values have the same units as the specified coordinate system. For example,",
                             "if the specified coordinate system is WGS 84 geographic coordinates then the values are",
                             "decimal degrees and must have a longitude range of [-180, 180] and a latitude range of [-90, 90]."),
                    uiOutput("pretty_range_360_uiOut_text"),
                    fluidRow(
                      column(
                        width = 6,
                        uiOutput("pretty_range_xmin_uiOut_num"),
                        uiOutput("pretty_range_ymin_uiOut_num")
                      ),
                      column(
                        width = 6,
                        uiOutput("pretty_range_xmax_uiOut_num"),
                        uiOutput("pretty_range_ymax_uiOut_num")
                      )
                    ),
                    helpText("Click 'Update range values' to use the same map range values as the last saved map"),
                    uiOutput("pretty_range_lastmap_uiOut_button")
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
                    fluidRow(
                      column(6, colourpicker::colourInput("pretty_background_color", tags$h5("Click to select background color"),
                                                          showColour = "background")),
                      column(
                        width = 6,
                        checkboxInput("pretty_na_color_check", "Make NA predictions transparent", value = TRUE),
                        conditionalPanel(
                          condition = "input.pretty_na_color_check == false",
                          column(12, uiOutput("pretty_na_color_uiOut_colour"))
                        )
                      )
                    ),
                    tags$br(), tags$br(),
                    fluidRow(
                      column(
                        width = 8,
                        radioButtons("pretty_color_perc", tags$h5("Color-code predictions by:"),
                                     choices = list("Relative percentage" = 1, "Numerical value" = 2)),
                        uiOutput("pretty_color_palette_uiOut_select"),
                        uiOutput("pretty_color_num_uiOut_num")
                      ),
                      column(
                        width = 4,
                        tags$span(tags$h5("Color scheme preview"), style = "text-align: right"),
                        plotOutput("pretty_color_preview_plot", width = "85px", height =  "250px")
                      )
                    )
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
                    checkboxInput("pretty_legend", "Include legend with the map", value = TRUE),
                    conditionalPanel(
                      condition = "input.pretty_legend",
                      fluidRow(
                        column(6, radioButtons("pretty_legend_inout", tags$h5("Legend location"),
                                               choices = list("Inside map frame" = 1, "Outside map frame" = 2), selected = 1)),
                        column(6, uiOutput("pretty_legend_pos_uiOut_select"))
                      ),
                      conditionalPanel(
                        condition = "input.pretty_legend_inout == 2",
                        helpText("'Legend width' is the horizontal proportion of the plot window taken up by the legend")
                      ),
                      fluidRow(
                        column(6, uiOutput("pretty_legend_width_uiOut_numeric")),
                        column(6, numericInput("pretty_legend_size", tags$h5("Legend text size"),
                                               value = 1.0, min = 0.1, step = 0.1))
                      ),
                      fluidRow(
                        column(6, checkboxInput("pretty_legend_frame", "Include black frame around legend", value = TRUE)),
                        column(
                          width = 6,
                          conditionalPanel(
                            condition = "input.pretty_color_perc == 2",
                            numericInput("pretty_legend_round", tags$h5("Legend labels: number of decimals"),
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
        ),

        ################################################################# Map Parameters - Section 2
        fluidRow(
          box(
            title = "Map Parameters - Section 2", solidHeader = FALSE, status = "warning", width = 12, collapsible = TRUE,
            fluidRow(
              ################################################## Title, axis labels, and margins
              column(
                width = 4,
                tags$strong("Title, axis labels, and margins"),
                fluidRow(
                  box(
                    width = 12,
                    helpText("Delete the text in the input boxes to remove the title or axis labels"),
                    uiOutput("pretty_title_uiOut_text"),
                    fluidRow(
                      column(6, textInput("pretty_xlab", tags$h5("X-axis label"), value = "Longitude")),
                      column(6, textInput("pretty_ylab", tags$h5("Y-axis label"), value = "Latitude"))
                    ),
                    helpText("Size values are relative to 1 (the default size)"),
                    fluidRow(
                      column(6, numericInput("pretty_title_cex", tags$h5("Title size (value is relative to 1)"),
                                             value = 1.3, step = 0.1)),
                      column(6, numericInput("pretty_lab_cex", tags$h5("Axis label size (value is relative to 1)"),
                                             value = 1, step = 0.1))
                    ),
                    tags$hr(),
                    helpText("'Inner margin' refers to the space between the map and the map frame.",
                             "'Outer margin' refers to the space between the map frame and the plot window;",
                             "'Outer margins' will be overwritten if more than one map is being plotted.",
                             "Margins can be used for creating whitespace for coordinate labels",
                             "or the legend if it is inside the map frame"),
                    fluidRow(
                      column(
                        width = 6,
                        numericInput("pretty_margin_in1", tags$h5("Inner margin - bottom"), value = 0.02, min = 0, step = 0.01),
                        numericInput("pretty_margin_in3", tags$h5("Inner margin - top"), value = 0.00, min = 0, step = 0.01),
                        numericInput("pretty_margin_out", tags$h5("Outer margin"), value = 0.02, min = 0, step = 0.01)
                      ),
                      column(
                        width = 6,
                        numericInput("pretty_margin_in2", tags$h5("Inner margin - left"), value = 0.02, min = 0, step = 0.01),
                        numericInput("pretty_margin_in4", tags$h5("Inner margin - right"), value = 0.00, min = 0, step = 0.01)
                      )
                    )
                  )
                )
              ),
              ################################################## Coordinate grid marks and labels
              column(
                width = 4,
                tags$strong("Coordinate grid marks and labels"),
                fluidRow(
                  box(
                    width = 12,
                    checkboxInput("pretty_tick", "Include coordinate grid marks and labels", value = TRUE),
                    conditionalPanel(
                      condition = "input.pretty_tick",
                      tags$hr(),
                      checkboxGroupInput("pretty_tick_which", label = NULL,
                                         choices = list("Include grid lines" = 1, "Include tick marks" = 2),
                                         selected = 2),
                      helpText("Grid mark start and interval units are the same as the units of the specified coordinate system.",
                               "The range of these values must adhere to the requirements specified in the map range section.",
                               tags$br(),
                               "Size and width values are relative to 1 (the default size)"),
                      fluidRow(
                        column(
                          width = 6,
                          uiOutput("pretty_tick_lon_start_uiOut_numeric"),
                          uiOutput("pretty_tick_lat_start_uiOut_numeric"),
                          numericInput("pretty_tick_lw", tags$h5("Grid mark width"), value = 1.0, min = 0.1, step = 0.1),
                          colourpicker::colourInput("pretty_tick_color", tags$h5("Click to select color for coordinate grid marks"),
                                                    value = "black", showColour = "background")
                        ),
                        column(
                          width = 6,
                          uiOutput("pretty_tick_lon_interval_uiOut_numeric"),
                          uiOutput("pretty_tick_lat_interval_uiOut_numeric"),
                          numericInput("pretty_tick_alpha", tags$h5("Grid mark transparency (1: solid; 0: transparent)"),
                                       value = 1.0, min = 0, max = 1, step = 0.1)
                        )
                      ),
                      tags$hr(),
                      tags$span(textOutput("pretty_tick_label_message"), style = "color: red;"),
                      fluidRow(
                        column(6, radioButtons("pretty_tick_label_inout", tags$h5("Coordinate label location"),
                                               choices = list("Inside frame" = 1, "Outside frame" = 2), selected = 2)),
                        column(6, numericInput("pretty_tick_label_size", tags$h5("Coordinate label size"),
                                               value = 1.0, min = 0.1, step = 0.1))
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
                    checkboxInput("pretty_addobj", "Include additional objects in map", value = FALSE),
                    conditionalPanel(
                      condition = "input.pretty_addobj",
                      column(12, tags$h5("Uncheck the above checkbox to remove all loaded additional objects")),
                      #----------------------------------------
                      box(
                        width = 12,
                        tags$strong("Loaded additional object(s)"),
                        tags$h5("Select loaded object to update its parameters or remove it from the GUI"),
                        DTOutput("pretty_addobj_table_out"),
                        tags$br(), tags$br(),
                        uiOutput("pretty_addobj_update_show_uiOut_button"),
                        tags$br(), tags$br(),
                        uiOutput("pretty_addobj_remove_execute_uiOut_button"),
                        textOutput("pretty_addobj_remove_out")
                      ),

                      #----------------------------------------
                      box(
                        width = 12,
                        tags$strong("New additional object"),
                        uiOutput("pretty_addobj_which_uiOut_select"),
                        conditionalPanel(
                          condition = "input.pretty_addobj_which == 4",
                          box(
                            width = 12,
                            selectInput("pretty_addobj_own_type", tags$h5("File type"), choices = file.type.list1, selected = 1),
                            ############## File type: csv
                            conditionalPanel(
                              condition = "input.pretty_addobj_own_type == 1",
                              ui.instructions.upload.csv(),
                              helpText("The first column must contain the longitude values, and the second column must",
                                       "contain the latitude values. The longitudes and latitudes must",
                                       "be in WGS 84 geographic coordinates.",
                                       "For polygons objects, multiple polygons may be demarcated rows with blank cells",
                                       "or cells with 'NA' entries"),
                              fileInput("pretty_addobj_own_csv_file", label.csv.upload, accept = ".csv"),
                              conditionalPanel("output.pretty_addobj_own_csv_flag == false", ui.error.upload.csv),
                              conditionalPanel(
                                condition = "output.pretty_addobj_own_csv_flag",
                                tags$span(tags$h5("Object uploaded"), style = "color: blue")
                              )
                            ),
                            ############## File type: shp
                            conditionalPanel (
                              condition = "input.pretty_addobj_own_type == 2",
                              ui.instructions.upload.shp(),
                              fileInput("pretty_addobj_own_shp_files", label.shp.upload, multiple = TRUE),
                              conditionalPanel("output.pretty_addobj_own_shp_flag == false", ui.error.upload.shp),
                              conditionalPanel(
                                condition = "output.pretty_addobj_own_shp_flag",
                                tags$span(tags$h5("Object uploaded"), style = "color: blue")
                              )
                            ),
                            ############## File type: gdb
                            conditionalPanel(
                              condition = "input.pretty_addobj_own_type == 3",
                              ui.instructions.upload.gdb(),
                              textInput("pretty_addobj_own_gdb_path", label.gdb.path, value = ".../folder.gdb"),
                              textInput("pretty_addobj_own_gdb_name", label.gdb.name, value = ""),
                              actionButton("pretty_addobj_own_gdb_load", "Upload feature class"),
                              conditionalPanel("output.pretty_addobj_own_gdb_flag == false", ui.error.upload.gdb),
                              conditionalPanel(
                                condition = "output.pretty_addobj_own_gdb_flag",
                                tags$span(tags$h5("Object uploaded"), style = "color: blue")
                              )
                            )
                          )
                        ),

                        fluidRow(
                          column(6, uiOutput("pretty_addobj_type_uiOut_radio")),
                          column(6, radioButtons("pretty_addobj_order", tags$h5("Object draw order:"),
                                                 choices = list("Draw object behind SDM" = 1,
                                                                "Draw object in front of SDM" = 2),
                                                 selected = 2))
                        ),
                        fluidRow(
                          column(6, uiOutput("pretty_addobj_color_ptfillcheck_uiOut_check")),
                          column(6, uiOutput("pretty_addobj_color_absbordercheck_uiOut_check"))
                        ),
                        fluidRow(
                          column(6, column(12, uiOutput("pretty_addobj_color_ptfill_uiOut_colour"))),
                          column(6, column(12, uiOutput("pretty_addobj_color_absborder_uiOut_colour")))
                        ),
                        fluidRow(
                          column(6, uiOutput("pretty_addobj_pchlty_uiOut_select")),
                          column(6, uiOutput("pretty_addobj_cexlwd_uiOut_numeric"))
                        ),
                        actionButton("pretty_addobj_add_execute", "Load additional object"),
                        textOutput("pretty_addobj_add_out"),
                        helpText("Loaded additional objects will appear in the 'Loaded additional polygons' table above")
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
