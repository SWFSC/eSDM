ui.prettyPlot <- function() {
  tabItem(tabName = "prettyPlot",
          conditionalPanel(condition = "output.pretty_display_flag == false", ui.no.model.pred.loaded1()), 
          conditionalPanel(
            condition = "output.pretty_display_flag", 
            fluidRow(
              box(
                solidHeader = FALSE, status = "primary", width = 6, height = 500, 
                withSpinner(plotOutput("pretty_plot"), type = 1)
              ),
              box(
                title = "Select Predictions to Plot", solidHeader = FALSE, status = "warning", collapsible = TRUE, width = 6, 
                DT::dataTableOutput("pretty_table_orig_out"), 
                br(), 
                DT::dataTableOutput("pretty_table_over_out"), 
                br(), 
                DT::dataTableOutput("pretty_table_ens_out")
              )
            ), 
            fluidRow(
              box(
                title = "Plot Parameters", solidHeader = FALSE, status = "warning", width = 12, collapsible = TRUE, 
                conditionalPanel(
                  condition = "output.pretty_pred_selected_flag == 0", 
                  helpText(strong("Please select at least one set of model predictions to plot")), 
                  br(), br()
                ), 
                conditionalPanel(
                  condition = "output.pretty_pred_selected_flag == 2",
                  helpText(strong("Can only handle plotting one pretty plot for now")), 
                  br(), br()
                ),
                fluidRow(
                  box(width = 6, 
                      br(), 
                      fluidRow(
                        column(2, actionButton("pretty_plot_execute", "Generate map")), 
                        column(10, textOutput("pretty_plot_values_event_text"))  
                      ), 
                      br(), br(), 
                      h5("Plotting or downloading a large set of predictions may take several minutes")
                  ),
                  box(width = 6, 
                      fluidRow(
                        column(3, radioButtons("pretty_plot_download_res", h5("Resolution"),
                                               choices = list("High (300 ppi)" = 1, "Low (72 ppi)" = 2),
                                               selected = 2)),
                        column(2, radioButtons("pretty_plot_download_format", h5("File format"),
                                               choices = list("jpeg" = 1, "pdf" = 2, "png" = 3),
                                               selected = 3)), 
                        column(6, uiOutput("pretty_plot_download_name_uiOut_text"))
                      ), 
                      br(), 
                      downloadButton("pretty_plot_download_execute", "Download map")
                  )
                ), 
                conditionalPanel("output.pretty_pred_selected_flag == 1", 
                                 fluidRow(
                                   column(3, 
                                          strong("Map range"), 
                                          fluidRow(
                                            box(width = 12, 
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
                                   column(3, 
                                          strong("Title and axis labels"), 
                                          fluidRow(
                                            box(width = 12, 
                                                uiOutput("pretty_plot_title_uiOut_text"), 
                                                fluidRow(
                                                  column(6, textInput("pretty_plot_xlab", h5("X-axis label"), value = "Longitude")), 
                                                  column(6, textInput("pretty_plot_ylab", h5("Y-axis label"), value = "Latitude"))
                                                ), 
                                                fluidRow(
                                                  column(6, numericInput("pretty_plot_title_cex", h5("Title size"), value = 1.4, step = 0.1)), 
                                                  column(6, numericInput("pretty_plot_lab_cex", h5("Axis label size"), value = 1, step = 0.1))
                                                )
                                            )
                                          )
                                   ), 
                                   column(3, 
                                          strong("Tick marks and tick labels"), 
                                          fluidRow(
                                            box(width = 12, 
                                                checkboxInput("pretty_plot_tick", "Plot tick marks and/or their labels", value = TRUE), 
                                                conditionalPanel(
                                                  condition = "input.pretty_plot_tick==true",
                                                  h5(em("None of the below has been implemented")), 
                                                  fluidRow(
                                                    column(6, checkboxInput("pretty_plot_tick_notall", "Include ticks", value = TRUE)),
                                                    column(6, checkboxInput("pretty_plot_label", "Include tick labels", value = TRUE))
                                                  ), 
                                                  fluidRow(
                                                    column(width = 6,
                                                           # checkboxInput("tick.left", label = "Left", value = TRUE),
                                                           # checkboxInput("tick.bot", label = "Bottom", value = TRUE),
                                                           numericInput("pretty_plot_tick_interval_major", h5("Units between each major tick"),
                                                                        value = 5, min = 0, step = 5)
                                                           # selectInput("tick.style", label = h5("Tick label style"),
                                                           #             choices = list("120" = 1, "120W" = 2,
                                                           #                            "120°" = 3, "120°W" = 4),
                                                           #             selected = 4)
                                                    ),
                                                    column(width = 6,
                                                           # checkboxInput("tick.right", label = "Right", value = TRUE),
                                                           # checkboxInput("tick.top", label = "Top", value = TRUE),
                                                           numericInput("pretty_plot_tick_interval_minor", h5("Minor ticks between each major tick"),
                                                                        value = 4, min = 0, max = 45, step = 1),
                                                           numericInput("pretty_plot_tick_length", h5("Tick length"),
                                                                        value = 1.0, min = 0, step = 0.1)
                                                    )
                                                  ), 
                                                  fluidRow(
                                                    column(width = 6,
                                                           # checkboxInput("tick.left.lab", label = "Left", value = TRUE),
                                                           # checkboxInput("tick.bot.lab", label = "Bottom", value = TRUE),
                                                           numericInput("pretty_plot_label_lon_start", h5("Start longitude tick labels at"),
                                                                        value = 0)
                                                           # selectInput("label.tick.font", label = h5("Tick label font"),
                                                           #             choices = font.family,
                                                           #             selected = 1)
                                                    ),
                                                    column(width = 6,
                                                           # checkboxInput("tick.right.lab", label = "Right", value = TRUE),
                                                           # checkboxInput("tick.top.lab", label = "Top", value = TRUE),
                                                           numericInput("pretty_plot_label_lat_start", h5("Start latitude tick labels at"),
                                                                        value = 0),
                                                           numericInput("pretty_plot_label_tick_size", h5("Tick label size"),
                                                                        value = 1.0, min = 0.1, max = 3, step = 0.1)
                                                    )
                                                  )
                                                )
                                            )
                                          )
                                   ), 
                                   column(3, 
                                          strong("Color scheme of predictions"), 
                                          fluidRow(
                                            box(width = 12, 
                                                checkboxInput("pretty_plot_perc", "Plot prediction percentage rather than value", value = TRUE),
                                                fluidRow(
                                                  column(7, selectInput("pretty_plot_colorscheme", h5("Color scheme"), 
                                                                        choices = list("Not yet implemented"), 
                                                                        selected = 1)), 
                                                  column(5, numericInput("pretty_plot_colorscheme_num", h5("Number of colors"), 
                                                                         value = 5, step = 1, min = 0))
                                                ), 
                                                fluidRow(
                                                  column(7, checkboxInput("pretty_plot_legend", "Include legend in plot", value = TRUE)), 
                                                  column(5, conditionalPanel(
                                                    condition = "input.pretty_plot_legend", 
                                                    selectInput("pretty_plot_legend_pos", h5("Legend position"), 
                                                                choices = list("Right" = 1, "Bottom" = 2, "Left" = 3, "Top" = 4), 
                                                                selected = 1)
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
          )
  )
}