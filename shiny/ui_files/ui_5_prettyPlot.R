### UI code for the 'High Quality Maps' tab

ui.prettyPlot <- function() {
  tabItem(
    tabName = "prettyPlot", 
    conditionalPanel(condition = "output.pretty_display_flag == false", ui.notice.no.pred.original()), 
    conditionalPanel(
      condition = "output.pretty_display_flag", 
      fluidRow(
        box(
          solidHeader = FALSE, status = "primary", width = 6, height = 500, 
          shinycssloaders::withSpinner(plotOutput("pretty_plot_plot"), type = 1)
        ), 
        box(
          title = "Select Predictions to Map", solidHeader = FALSE, status = "warning", width = 6, collapsible = TRUE, 
          ui.instructions.multipletables.select(text.in = "map:"), 
          DT::dataTableOutput("pretty_table_orig_out"), 
          tags$br(), 
          DT::dataTableOutput("pretty_table_over_out"), 
          tags$br(), 
          DT::dataTableOutput("pretty_table_ens_out")
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
              box(
                width = 6, 
                tags$br(), 
                fluidRow(
                  column(2, actionButton("pretty_plot_execute", "Generate map")), 
                  column(10, textOutput("pretty_plot_values_event_text"))  
                ), 
                tags$br(), 
                tags$h5("Plotting or downloading a large set of model predictions may take several minutes")
              ), 
              
              ################################################ Download map
              box(
                width = 6, 
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
                    checkboxInput("pretty_plot_proj_ll", "Generate map in WGS 84 geographic coordinates (decimal degrees)", 
                                  value = TRUE), 
                    conditionalPanel(
                      condition = "input.pretty_plot_proj_ll == false", 
                      uiOutput("pretty_plot_proj_idx_uiOut_select")
                    ),
                    helpText("Map range values must be in WGS 84 geographic coordinates,", 
                             "with a range of [-180, 180] for longitudes and [-90, 90] for latitudes"), 
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
                    uiOutput("pretty_plot_title_uiOut_text"), 
                    fluidRow(
                      column(6, textInput("pretty_plot_xlab", tags$h5("X-axis label"), value = "Longitude")), 
                      column(6, textInput("pretty_plot_ylab", tags$h5("Y-axis label"), value = "Latitude"))
                    ), 
                    helpText("Size values are relative to 1, which is the default size"), 
                    fluidRow(
                      column(6, numericInput("pretty_plot_title_cex", tags$h5("Title size (value is relative to 1)"), 
                                             value = 1.4, step = 0.1)), 
                      column(6, numericInput("pretty_plot_lab_cex", tags$h5("Axis label size (value is relative to 1)"), 
                                             value = 1, step = 0.1))
                    )
                  )
                )
              ), 
              ################################################## Tick marks and tick labels
              column(
                width = 4, 
                tags$strong("Tick marks and tick labels"), 
                fluidRow(
                  box(
                    width = 12, 
                    fluidRow(
                      column(6, checkboxInput("pretty_plot_tick", "Include tick marks in map", value = TRUE)),
                      column(6, tags$br(), helpText("Length and size values are relative to 1"))
                    ), 
                    fluidRow(
                      column(
                        width = 6, 
                        conditionalPanel(
                          condition = "input.pretty_plot_tick", 
                          radioButtons("pretty_plot_tick_manual", tags$h5("Tick location options"), 
                                       choices = list("Use default tick locations" = 1, "Enter tick locations manually" = 2), 
                                       selected = 1)
                        )
                      ), 
                      column(
                        width = 6, 
                        conditionalPanel(
                          condition = "input.pretty_plot_tick", 
                          numericInput("pretty_plot_tick_length", tags$h5("Tick length"), 
                                       value = 1.0, min = 0, step = 0.1)
                        )
                      )
                    ), 
                    fluidRow(
                      conditionalPanel(
                        condition = "input.pretty_plot_tick && input.pretty_plot_tick_manual == 2", 
                        column(12, helpText("Tick location values must be in WGS 84 geographic coordinates (decimal degrees),", 
                                            "with a range of [-180, 180] for longitudes and [-90, 90] for latitudes.", 
                                            "Enter locations as '#, #, ..., #'")), 
                        column(6, textInput("pretty_plot_tick_manual_lon", tags$h5("Longitude tick locations"), value = "")), 
                        column(6, textInput("pretty_plot_tick_manual_lat", tags$h5("Latitude tick locations"), value = ""))
                      )
                    ), 
                    fluidRow(
                      conditionalPanel(
                        condition = "input.pretty_plot_tick == false", 
                        column(12, helpText("The map must include tick marks to have tick labels"))
                      ), 
                      conditionalPanel(
                        condition = "input.pretty_plot_tick", 
                        column(
                          width = 6, 
                          checkboxInput("pretty_plot_tick_label", "Plot tick labels", value = TRUE), 
                          conditionalPanel(
                            condition = "input.pretty_plot_tick_label", 
                            helpText("Tick labels will be generated at all tick locations")
                          )
                        ), 
                        column(
                          width = 6, 
                          conditionalPanel(
                            condition = "input.pretty_plot_tick_label", 
                            numericInput("pretty_plot_tick_label_size", tags$h5("Tick label size"), 
                                         value = 1.0, min = 0.1, max = 3, step = 0.1)
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
              ################################################## Color scheme of predictions
              column(
                width = 4, 
                tags$strong("Color scheme of predictions"), 
                fluidRow(
                  box(
                    width = 12, 
                    fluidRow(
                      column(
                        width = 7, 
                        radioButtons("pretty_plot_color_perc", tags$h5("Prediction display option"), 
                                     choices = list("Plot relative percentages of predictions" = 1, 
                                                    "Plot numerical values of predictions" = 2)), 
                        uiOutput("pretty_plot_color_palette_uiOut_select"), 
                        uiOutput("pretty_plot_color_num_uiOut_num")
                      ), 
                      column(3, offset = 1, plotOutput("pretty_plot_color_preview_plot", height =  "250px"))
                    )
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
                        width = 6, #offset = 1, 
                        tags$br(), tags$br(), 
                        actionButton("pretty_plot_background_reset_execute", "Reset background color to white")
                      )
                    ), 
                    tags$br(), 
                    checkboxInput("pretty_plot_legend", "Include legend with map", value = TRUE), 
                    conditionalPanel(
                      condition = "input.pretty_plot_legend", 
                      fluidRow(
                        column(6, selectInput("pretty_plot_legend_pos", tags$h5("Legend position"), 
                                              choices = list("Right" = 1, "Bottom" = 2, "Left" = 3, "Top" = 4), 
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
              ################################################## Additional polygons
              column(
                width = 4, 
                tags$strong("Additional polygons"), 
                fluidRow(
                  box(
                    width = 12, 
                    checkboxInput("pretty_plot_other_obj", "Include additional polygons in map", value = FALSE), 
                    conditionalPanel(
                      condition = "input.pretty_plot_other_obj", 
                      helpText("Only the study area polygon and/or the land polygon can be included at this time.", 
                               "Note that after overlaying models, you can back and upload new 'study area' and/or 'land' polygons", 
                               "so that you can include them in a high quality map."), 
                      helpText("The order in which the polygons are selected controls the order in which they are drawn.", 
                               "This means that if the land polygon is selected before the study area polygon,", 
                               "then the study area polygon will be drawn on top of the land polygon. The reverse is also true.", 
                               "This pattern does not hold if one polygon is drawn before the model predictions", 
                               "and the other is drawn after."), 
                      uiOutput("pretty_plot_other_obj_which_uiOut_selectize"), 
                      conditionalPanel(
                        condition = "input.pretty_plot_other_obj_which == null", 
                        helpText("No additional polygons will be plotted")
                      ), 
                      fluidRow(
                        column(
                          width = 6, 
                          conditionalPanel(
                            condition = "input.pretty_plot_other_obj_which != null && input.pretty_plot_other_obj_which.includes('1')", 
                            radioButtons("pretty_plot_bound_poly_col", tags$h5("Color of study area border"),
                                         choices = list("Black" = "black", "Grey" = "grey", "Red" = "red"),
                                         selected = "black")
                          )
                        ),
                        column(
                          width = 6, 
                          conditionalPanel(
                            condition = "input.pretty_plot_other_obj_which != null && input.pretty_plot_other_obj_which.includes('2')", 
                            radioButtons("pretty_plot_land_poly_fill", tags$h5("Color of land"),
                                         choices = list("Tan" = "tan", "Grey" = "grey"),
                                         selected = "tan")
                          )
                        )
                      ), 
                      fluidRow(
                        column(
                          width = 6, 
                          conditionalPanel(
                            condition = "input.pretty_plot_other_obj_which != null && input.pretty_plot_other_obj_which.includes('1')", 
                            numericInput("pretty_plot_bound_poly_lwd", tags$h5("Line width of study area border"),
                                         value = 1.5, min = 0, step = 0.1), 
                            checkboxInput("pretty_plot_bound_poly_first", "Draw study area polygon before model predictions", value = FALSE)
                          )
                        ),
                        column(
                          width = 6, 
                          conditionalPanel(
                            condition = "input.pretty_plot_other_obj_which != null && input.pretty_plot_other_obj_which.includes('2')", 
                            numericInput("pretty_plot_land_poly_lwd", tags$h5("Line width of land outline"),
                                         value = 0.3, min = 0, step = 0.1),
                            checkboxInput("pretty_plot_bound_poly_first", "Draw land polygon before model predictions", value = TRUE)
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
        
        ################################################################# End of Map Parameters - Section 2
      )
    )
  )
}