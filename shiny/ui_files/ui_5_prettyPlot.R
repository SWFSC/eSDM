### UI code for the 'High Quality Maps' tab

ui.prettyPlot <- function() {
  tabItem(tabName = "prettyPlot",
          conditionalPanel(condition = "output.pretty_display_flag == false", ui.no.model.pred.loaded1()), 
          conditionalPanel(
            condition = "output.pretty_display_flag", 
            fluidRow(
              box(
                solidHeader = FALSE, status = "primary", width = 6, height = 500, 
                shinycssloaders::withSpinner(plotOutput("pretty_plot_plot"), type = 1)
              ),
              box(
                title = "Select Predictions to Map", solidHeader = FALSE, status = "warning", collapsible = TRUE, width = 6, 
                DT::dataTableOutput("pretty_table_orig_out"), 
                br(), 
                DT::dataTableOutput("pretty_table_over_out"), 
                br(), 
                DT::dataTableOutput("pretty_table_ens_out")
              )
            ), 
            
            conditionalPanel(
              condition = "output.pretty_pred_selected_flag == 0 || output.pretty_pred_selected_flag == 2", 
              fluidRow(
                box(width = 12, 
                    helpText(strong("Please select exactly one set of model predictions to plot.", 
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
                    box(width = 6, 
                        br(), 
                        fluidRow(
                          column(2, actionButton("pretty_plot_execute", "Generate map")), 
                          column(10, textOutput("pretty_plot_values_event_text"))  
                        ),
                        br(), 
                        h5("Plotting or downloading a large set of predictions may take several minutes")
                    ),
                    
                    ################################################ Download map
                    box(width = 6, 
                        fluidRow(
                          column(3, radioButtons("pretty_plot_download_res", h5("Resolution"),
                                                 choices = list("High (300 ppi)" = 1, "Low (72 ppi)" = 2),
                                                 selected = 2)),
                          column(2, radioButtons("pretty_plot_download_format", h5("Image file format"),
                                                 choices = list("JPEG" = 1, "PDF" = 2, "PNG" = 3),
                                                 selected = 3)), 
                          column(6, uiOutput("pretty_plot_download_name_uiOut_text"))
                        ), 
                        br(), 
                        downloadButton("pretty_plot_download_execute", "Download map")
                    )
                  )
                  
                )
              ), 
              
              ################################################################# Map Parameters
              fluidRow(
                box(
                  title = "Map Parameters", solidHeader = FALSE, status = "warning", width = 12, collapsible = TRUE, 
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
                                 fluidRow(
                                   column(6, 
                                          checkboxInput("pretty_plot_tick", "Plot ticks", value = TRUE), 
                                          conditionalPanel(
                                            condition = "input.pretty_plot_tick",
                                            numericInput("pretty_plot_tick_length", h5("Tick length"),
                                                         value = 1.0, min = 0, step = 0.1)
                                          )
                                   ),
                                   column(6, 
                                          conditionalPanel(
                                            condition = "input.pretty_plot_tick",
                                            radioButtons("pretty_plot_tick_manual", label = NULL, 
                                                         choices = list("Use default tick locations" = 1, "Enter tick locations manually" = 2), 
                                                         selected = 1)
                                          )
                                   )
                                 ), 
                                 fluidRow(
                                   conditionalPanel(
                                     condition = "input.pretty_plot_tick && input.pretty_plot_tick_manual == 2",
                                     column(6, textInput("pretty_plot_tick_manual_lon", h5("Longitude tick locations"), value = "")), 
                                     column(6, textInput("pretty_plot_tick_manual_lat", h5("Latitude tick locations"), value = ""))
                                   )
                                 ), 
                                 fluidRow(
                                   conditionalPanel(
                                     condition = "input.pretty_plot_tick == false",
                                     column(12, helpText("Must plot ticks to plot tick labels"))
                                   ), 
                                   conditionalPanel(
                                     condition = "input.pretty_plot_tick",
                                     column(6, 
                                            checkboxInput("pretty_plot_tick_label", "Plot tick labels", value = TRUE), 
                                            conditionalPanel(
                                              condition = "input.pretty_plot_tick_label",
                                              helpText("Tick labels will be generated at all tick locations")
                                            )
                                     ), 
                                     column(6, 
                                            conditionalPanel(
                                              condition = "input.pretty_plot_tick_label",
                                              numericInput("pretty_plot_tick_label_size", h5("Tick label size"),
                                                           value = 1.0, min = 0.1, max = 3, step = 0.1)
                                            )
                                     )
                                   )
                                 )
                                 
                                 # numericInput("pretty_plot_tick_interval_major", h5("Units between each tick"),
                                 #              value = 5, min = 0, step = 5)
                                 # numericInput("pretty_plot_label_lon_start", h5("Start longitude tick labels at"),
                                 #              value = 0), 
                                 # numericInput("pretty_plot_label_lat_start", h5("Start latitude tick labels at"),
                                 #              value = 0),
                                 # checkboxInput("tick.right.lab", label = "Right", value = TRUE),
                                 # checkboxInput("tick.top.lab", label = "Top", value = TRUE),
                                 # checkboxInput("tick.left.lab", label = "Left", value = TRUE),
                                 # checkboxInput("tick.bot.lab", label = "Bottom", value = TRUE),
                                 # selectInput("label.tick.font", label = h5("Tick label font"),
                                 #             choices = font.family,
                                 #             selected = 1)
                                 # checkboxInput("tick.right", label = "Right", value = TRUE),
                                 # checkboxInput("tick.top", label = "Top", value = TRUE),
                                 # numericInput("pretty_plot_tick_interval_minor", h5("Minor ticks between each major tick"),
                                 #              value = 4, min = 0, max = 45, step = 1),
                                 # selectInput("tick.style", label = h5("Tick label style"),
                                 #             choices = list("120" = 1, "120W" = 2,
                                 #                            "120°" = 3, "120°W" = 4),
                                 #             selected = 4)
                                 # checkboxInput("tick.left", label = "Left", value = TRUE),
                                 # checkboxInput("tick.bot", label = "Bottom", value = TRUE),
                                 
                             )
                           )
                    ), 
                    column(3, 
                           strong("Color scheme of predictions"), 
                           fluidRow(
                             box(width = 12, 
                                 checkboxInput("pretty_plot_perc", "Plot prediction percentage rather than numerical values", value = TRUE),
                                 fluidRow(
                                   column(7, selectInput("pretty_plot_colorscheme", h5("Color scheme"), 
                                                         choices = list("Current default blue to red" = 1, 
                                                                        "RColorBrewer: Spectral (rainbow)" = 2, 
                                                                        "RColorBrewer: YlGnBu" = 3, 
                                                                        "viridis: viridis" = 4, "viridis: inferno" = 5), 
                                                         selected = 1)), 
                                   column(5, numericInput("pretty_plot_colorscheme_num", h5("Number of colors"), 
                                                          value = 10, step = 1, min = 1))
                                 ), 
                                 fluidRow(
                                   column(7, checkboxInput("pretty_plot_legend", "Include legend in plot", value = TRUE)), 
                                   column(5, 
                                          conditionalPanel(
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