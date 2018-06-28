### UI code for the 'High Quality Maps' tab

ui.prettyPlot <- function() {
  tabItem(
    tabName = "prettyPlot",
    conditionalPanel(condition = "output.pretty_display_flag == false", ui.notice.no.pred.original()),
    conditionalPanel(
      condition = "output.pretty_display_flag",
      fluidRow(
        box(
          solidHeader = FALSE, status = "primary", height = 550, width = 6, align = "center",
          shinycssloaders::withSpinner(plotOutput("pretty_plot_plot", height = 400), type = 1)
        ),
        box(
          title = "Select Predictions to Map", solidHeader = FALSE, status = "warning", width = 6, collapsible = TRUE,
          ui.instructions.multipletables.select(text.in = "map:"),
          DTOutput("pretty_table_orig_out"),
          tags$br(),
          DTOutput("pretty_table_over_out"),
          tags$br(),
          DTOutput("pretty_table_ens_out")
        )
      ),

      conditionalPanel(
        condition = "output.pretty_pred_selected_flag == 0 || output.pretty_pred_selected_flag == 2",
        fluidRow(
          box(
            width = 12,
            helpText(tags$strong("Please select exactly one set of model predictions to plot.",
                                 "The app can only generate one high quality map at a time for now."))
          )
        )
      ),

      conditionalPanel(
        condition = "output.pretty_pred_selected_flag == 1",
        ################################################################# Map Control
        fluidRow(
          box(
            title = "Map Control", solidHeader = FALSE, status = "warning", width = 12, collapsible = TRUE,
            fluidRow(
              ################################################ Generate map
              column(
                width = 6,
                tags$strong("Generate map in-app"),
                fluidRow(
                  box(
                    width = 12,
                    tags$h5("Creating or downloading a map of a large set of model predictions may take several minutes",
                            tags$br(),
                            "Note that the GUI will make the map fill the entire space above, and thus depending on the shape",
                            "of the map there may be extra white space around the colored predictions/background"),
                    tags$br(),
                    fluidRow(
                      column(2, actionButton("pretty_plot_execute", "Generate map")),
                      column(10, textOutput("pretty_plot_values_event_text"))
                    )
                  )
                )
              ),

              ################################################ Download map
              column(
                width = 6,
                tags$strong("Download map"),
                fluidRow(
                  box(
                    width = 12,
                    conditionalPanel(
                      condition = "output.pretty_display_download == false",
                      tags$h5("The image that will be downloaded is the image displayed in the box above.",
                              "Thus, you must click 'Generate map' to generate a map before you can download that map.")
                    ),
                    conditionalPanel(
                      condition = "output.pretty_display_download",
                      tags$h5("The map displayed above will be the map that is downloaded"),
                      fluidRow(
                        column(3, radioButtons("pretty_plot_download_res", tags$h5("Resolution"),
                                               choices = list("High (300 ppi)" = 1, "Low (72 ppi)" = 2),
                                               selected = 2)),
                        column(2, radioButtons("pretty_plot_download_format", tags$h5("Image file format"),
                                               choices = list("JPEG" = 1, "PDF" = 2, "PNG" = 3),
                                               selected = 3)),
                        column(6, uiOutput("pretty_plot_download_name_uiOut_text"))
                      ),
                      tags$br(),
                      downloadButton("pretty_plot_download_execute", "Download map")
                    )
                  )
                )
              )
            )
          )
        ),

        ################################################################# Map Parameters - Section 1
        fluidRow(
          box(
            title = "Map Parameters - Section 1", solidHeader = FALSE, status = "warning", width = 12, collapsible = TRUE,
            fluidRow(
              ################################################## Map projection and range
              column(
                width = 4,
                tags$strong("Map projection and range"),
                fluidRow(
                  box(
                    width = 12,
                    checkboxInput("pretty_plot_proj_ll", "Generate the map in WGS 84 geographic coordinates (decimal degrees)",
                                  value = TRUE),
                    conditionalPanel(
                      condition = "input.pretty_plot_proj_ll == false",
                      uiOutput("pretty_plot_proj_idx_uiOut_select")
                    ),
                    helpText("Map range values must have the same units as the specified coordinate system.",
                             "If the specified coordinate system is WGS 84 geographic coordinates, then the values must be",
                             "decimal degrees with a range of [-180, 180] for longitudes and [-90, 90] for latitudes"),
                    fluidRow(
                      column(6, uiOutput("pretty_plot_range_xmin_uiOut_num")),
                      column(6, uiOutput("pretty_plot_range_xmax_uiOut_num"))
                    ),
                    fluidRow(
                      column(6, uiOutput("pretty_plot_range_ymin_uiOut_num")),
                      column(6, uiOutput("pretty_plot_range_ymax_uiOut_num"))
                    )
                  )
                )
              ),
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
                tags$strong("Tick marks and labels"),
                fluidRow(
                  box(
                    width = 12,
                    checkboxInput("pretty_plot_tick", "Include tick marks and labels on the map", value = TRUE),
                    conditionalPanel(
                      condition = "input.pretty_plot_tick",
                      helpText("Tick marks will be generated at default locations.", tags$br(),
                               "Length and size values are relative to 1 (the default size)"),
                      fluidRow(
                        column(6, numericInput("pretty_plot_tick_length", tags$h5("Tick mark length"),
                                               value = 1.0, min = 0, step = 0.1)),
                        column(6, numericInput("pretty_plot_tick_label_size", tags$h5("Tick label size"),
                                               value = 1.0, min = 0.1, max = 3, step = 0.1)
                        )
                      )
                    )
                  )
                )
              )
              # column(
              #   width = 4,
              #   tags$strong("Tick marks and tick labels"),
              #   fluidRow(
              #     box(
              #       width = 12,
              #       fluidRow(
              #         column(6, checkboxInput("pretty_plot_tick", "Include tick marks in the map", value = TRUE)),
              #         column(
              #           width = 6,
              #           tags$br(),
              #           conditionalPanel("input.pretty_plot_tick", helpText("Length and size values are relative to 1"))
              #         )
              #       ),
              #       conditionalPanel(
              #         condition = "input.pretty_plot_tick",
              #         fluidRow(
              #           column(6, radioButtons("pretty_plot_tick_manual", tags$h5("Tick location options"),
              #                                  choices = list("Use default tick locations" = 1, "Enter tick locations manually" = 2),
              #                                  selected = 1)),
              #           column(6, numericInput("pretty_plot_tick_length", tags$h5("Tick length"),
              #                                  value = 1.0, min = 0, step = 0.1))
              #         )
              #       ),
              #       fluidRow(
              #         conditionalPanel(
              #           condition = "input.pretty_plot_tick && input.pretty_plot_tick_manual == 2",
              #           column(12, helpText("Enter locations as '#, #, ..., #'.",
              #                               "Tick location values must have the same units as the specified coordinate system.",
              #                               "If the specified coordinate system is WGS 84 geographic coordinates,",
              #                               "then the values must be",
              #                               "decimal degrees with a range of [-180, 180] for longitudes and [-90, 90] for latitudes")),
              #           column(6, textInput("pretty_plot_tick_manual_lon", tags$h5("Longitude tick locations"), value = "")),
              #           column(6, textInput("pretty_plot_tick_manual_lat", tags$h5("Latitude tick locations"), value = ""))
              #         )
              #       ),
              #       fluidRow(
              #         conditionalPanel(
              #           condition = "input.pretty_plot_tick == false",
              #           column(12, helpText("The map must include tick marks to include tick labels"))
              #         ),
              #         conditionalPanel(
              #           condition = "input.pretty_plot_tick",
              #           column(
              #             width = 6,
              #             checkboxInput("pretty_plot_tick_label", "Include tick labels in the map", value = TRUE),
              #             conditionalPanel(
              #               condition = "input.pretty_plot_tick_label",
              #               helpText("Tick labels will be generated at all tick marks")
              #             )
              #           ),
              #           column(
              #             width = 6,
              #             conditionalPanel(
              #               condition = "input.pretty_plot_tick_label",
              #               numericInput("pretty_plot_tick_label_size", tags$h5("Tick label size"),
              #                            value = 1.0, min = 0.1, max = 3, step = 0.1)
              #             )
              #           )
              #         )
              #       )
              #     )
              #   )
              # )
            )
          )
        ),

        ################################################################# Map Parameters - Section 2
        fluidRow(
          box(
            title = "Map Parameters - Section 2", solidHeader = FALSE, status = "warning", width = 12, collapsible = TRUE,
            fluidRow(
              ################################################## Color scheme of predictions
              column(
                width = 4,
                tags$strong("Color scheme of predictions"),
                fluidRow(
                  box(
                    width = 12,
                    fluidRow(
                      column(
                        width = 8,
                        radioButtons("pretty_plot_color_perc", tags$h5("Prediction color scheme option"),
                                     choices = list("Color-code predictions by relative percentage" = 1,
                                                    "Color-code predictions by numerical value" = 2)),
                        uiOutput("pretty_plot_color_palette_uiOut_select"),
                        uiOutput("pretty_plot_color_num_uiOut_num")
                      ),
                      column(
                        width = 3,
                        tags$h5("Color scheme preview"),
                        plotOutput("pretty_plot_color_preview_plot", height =  "250px")
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
                tags$strong("Background color and legend"),
                fluidRow(
                  box(
                    width = 12,
                    fluidRow(
                      column(6, colourpicker::colourInput("pretty_plot_background_color", tags$h5("Click to select background color"),
                                                          showColour = "background")),
                      column(
                        width = 6,
                        tags$br(), tags$br(),
                        actionButton("pretty_plot_background_reset_execute", "Reset background color to white")
                      )
                    ),
                    tags$br(),
                    checkboxInput("pretty_plot_legend", "Include legend with the map", value = TRUE),
                    conditionalPanel(
                      condition = "input.pretty_plot_legend",
                      fluidRow(
                        column(6, selectInput("pretty_plot_legend_pos", tags$h5("Legend position"),
                                              choices = list("Right" = 1),
                                              #list("Right" = 1, "Bottom" = 2, "Left" = 3, "Top" = 4),
                                              selected = 1)),
                        column(
                          width = 6,
                          conditionalPanel(
                            condition = "input.pretty_plot_color_perc == 2",
                            numericInput("pretty_plot_legend_round", tags$h5("Legend labels: number of decimals"),
                                         value = 10, min = 1, step = 1)
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
                tags$strong("Additional polygons and points"),
                fluidRow(
                  box(
                    width = 12,
                    checkboxInput("pretty_plot_addobj", "Include additional polygons in map", value = FALSE),
                    conditionalPanel(
                      condition = "input.pretty_plot_addobj",
                      uiOutput("pretty_plot_addobj_which_uiOut_message"),
                      box(
                        width = 12,
                        tags$h5("Loaded object(s)"),
                        tableOutput("pretty_plot_addobj_table_out")
                      ),
                      box(
                        width = 12,
                        tags$h5("Add object"),
                        uiOutput("pretty_plot_addobj_which_uiOut_select"),
                        fluidRow(
                          column(6, uiOutput("pretty_plot_addobj_color_uiOut_colour")),
                          column(6, uiOutput("pretty_plot_addobj_cex_uiOut_numeric"))
                        ),
                        fluidRow(
                          column(6, uiOutput("pretty_plot_addobj_order_uiOut_radio")),
                          column(6, uiOutput("pretty_plot_addobj_execute_uiOut_button"))
                        )
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
