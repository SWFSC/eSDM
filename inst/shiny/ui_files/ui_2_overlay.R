### UI code for the 'Overlay Predictions' tab

ui.overlay <- function() {
  tabItem(
    tabName = "overlay",
    conditionalPanel("output.overlay_display_flag == false", ui.notice.no.pred.original()),
    conditionalPanel(
      condition = "output.overlay_display_flag",
      fluidRow(
        column(
          width = 4,
          fluidRow(
            box(
              title = "Import Study Area Polygon", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              checkboxInput("overlay_bound", "Clip the base geometry to a study area polygon in the overlay process", value = FALSE),
              conditionalPanel(
                condition = "input.overlay_bound == true",
                column(
                  width = 12,
                  tags$h5("Uncheck the above checkbox to remove an imported study area polygon"),
                  selectInput("overlay_bound_file_type", tags$h5("File type"), choices = file.type.list1, selected = 1, width = "70%")
                ),
                box(
                  width = 12,
                  conditionalPanel(
                    condition = "input.overlay_bound_file_type == 1",
                    ui.instructions.upload.csv(),
                    ui.instructions.poly.csv.single(),
                    fileInput("overlay_bound_csv_file", label.csv.upload, accept = ".csv"),
                    textOutput("overlay_bound_csv_text")
                  ),
                  conditionalPanel(
                    condition = "input.overlay_bound_file_type == 2",
                    ui.instructions.upload.shp(),
                    fileInput("overlay_bound_gis_shp_files", label.shp.upload, multiple = TRUE),
                    textOutput("overlay_bound_gis_shp_text")
                  ),
                  conditionalPanel(
                    condition = "input.overlay_bound_file_type == 3",
                    ui.instructions.upload.gdb(),
                    textInput("overlay_bound_gis_gdb_path", label.gdb.path, value = ".../folder.gdb"),
                    textInput("overlay_bound_gis_gdb_name", label.gdb.name, value = ""),
                    actionButton("overlay_bound_gis_gdb_load", label.gdb.upload),
                    textOutput("overlay_bound_gis_gdb_text")
                  )
                ),
                column(12, tags$span(textOutput("overlay_bound_message"), style = "color: blue;"))
              )
            ),

            box(
              title = "Import Erasing Polygon", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              checkboxInput("overlay_land", "Erase area from the base geometry in the overlay process", value = FALSE),
              conditionalPanel(
                condition = "input.overlay_land == true",
                column(
                  width = 12,
                  tags$h5("Uncheck the above checkbox to remove an imported erasing polygon"),
                  radioButtons("overlay_land_load_type", NULL,
                               choices = list("Use provided erasing polygon" = 1, "Upload personal erasing polygon" = 2),
                               selected = 1),
                  conditionalPanel(
                    condition = "input.overlay_land_load_type == 2 ",
                    selectInput("overlay_land_file_type", tags$h5("File type"), choices = file.type.list1, selected = 1, width = "70%")
                  )
                ),
                box(
                  width = 12,
                  conditionalPanel(
                    condition = "input.overlay_land_load_type == 1",
                    helpText(
                      "The provided erasing polygon is from the Global Self-consistent, Hierarchical, ",
                      "High-resolution Geography (GSHHG) Database. It is a low-resolution polygon that",
                      "represents the land of all continents, including Antarctica,",
                      "but not lakes, rivers, or islands within those continents.",
                      tags$br(),
                      "See the", tags$a("GSHHG website", href = "http://www.soest.hawaii.edu/pwessel/gshhg/"),
                      "for more information about the provided erasing polygon,",
                      "or to download polygons with higher resolutions."
                    ),
                    actionButton("overlay_land_provided", "Import provided erasing polygon"),
                    tags$span(textOutput("overlay_land_prov_message"), style = "color: blue"),
                    textOutput("overlay_land_prov_text")
                  ),
                  conditionalPanel(
                    condition = "input.overlay_land_load_type == 2",
                    conditionalPanel(
                      condition = "input.overlay_land_file_type == 1",
                      ui.instructions.upload.csv(),
                      ui.instructions.poly.csv(),
                      fileInput("overlay_land_csv_file", label.csv.upload, accept = ".csv"),
                      textOutput("overlay_land_csv_text")
                    ),
                    conditionalPanel(
                      condition = "input.overlay_land_file_type == 2",
                      ui.instructions.upload.shp(),
                      fileInput("overlay_land_gis_shp_files", label.shp.upload, multiple = TRUE),
                      textOutput("overlay_land_gis_shp_text")
                    ),
                    conditionalPanel(
                      condition = "input.overlay_land_file_type == 3",
                      ui.instructions.upload.gdb(),
                      textInput("overlay_land_gis_gdb_path", label.gdb.path, value = ".../folder.gdb"),
                      textInput("overlay_land_gis_gdb_name", label.gdb.name, value = ""),
                      actionButton("overlay_land_gis_gdb_load", label.gdb.upload),
                      textOutput("overlay_land_gis_gdb_text")
                    )
                  )
                ),
                column(12, tags$span(textOutput("overlay_land_message"), style = "color: blue;"))
              )
            )
          )
        ),

        #######################################################################
        column(
          width = 8,
          fluidRow(
            box(
              title = "Imported Original Predictions", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              ui.instructions.table.select(text.pre = "original", text.in = "to use as the base geometry:", sel.num = 1),
              conditionalPanel("input.overlay_loaded_table_stats != true", DTOutput("overlay_loaded_table")),
              conditionalPanel("input.overlay_loaded_table_stats", DTOutput("overlay_loaded_stats_table")),
              column(12, checkboxInput("overlay_loaded_table_stats", paste("Display additional information - NOTE that you can only",
                                                                           "select or deselect a row when this box is unchecked")))
            )
          ),
          fluidRow(
            box(
              title = "Overlay Predictions", status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE,
              fluidRow(
                column(
                  width = 6,
                  fluidRow(
                    box(
                      width = 12,
                      tags$strong("1) Overlay options: study area and erasing polygons"),
                      tags$h5("Import these polygons in their respecitve sections: 'Import Study Area Polygon' and",
                              "'Import Erasing Polygon'."),
                      helpText("Note that the study area polygon performs the same function as the 'clip feature' in the",
                               tags$a(href = "http://pro.arcgis.com/en/pro-app/tool-reference/analysis/clip.htm", "clip tool"),
                               "in ArcGIS, while the erasing polygon performs the same function as the 'erase feature' in the",
                               tags$a(href = "http://pro.arcgis.com/en/pro-app/tool-reference/analysis/erase.htm", "erase tool"),
                               "."),
                      tags$br(),
                      tags$strong("2) Overlay options: base geometry"),
                      tags$h5("Specify the base geometry in the 'Imported Original Predictions' table.")
                    ),
                    box(
                      width = 12,
                      tags$strong("3) Overlay options: coordinate system"),
                      helpText("The overlay process involves calculating the intersection and area of polygons,",
                               "and thus the coordinate system in which the overlay is performed will have an effect on the results."),
                      checkboxInput("overlay_proj_native",
                                    "Perform the overlay in the native coordinate system of the specified base geometry",
                                    value = TRUE),
                      conditionalPanel(
                        condition = "input.overlay_proj_native == false",
                        radioButtons("overlay_proj_method", NULL,
                                     choices = list("Perform overlay in WGS 84 geographic coordinates" = 1,
                                                    "Select predictions with desired coordinate system" = 2,
                                                    "Enter numeric EPSG code" = 3),
                                     selected = 1),
                        conditionalPanel(
                          condition = "input.overlay_proj_method == 1",
                          helpText("When calculating area using WGS 84 geographic coordinates, the following assumptions are made:",
                                   "1) 'Equatorial axis of ellipsoid' = 6378137 and",
                                   "2) 'Inverse flattening of ellipsoid' = 1/298.257223563.", tags$br(),
                                   "See", tags$a("this article", href = "https://link.springer.com/article/10.1007%2Fs00190-012-0578-z"),
                                   "for more details about assumptions that must be made when calculating the area",
                                   "using WGS 84 geographic coordinates.")
                        ),
                        uiOutput("overlay_proj_sdm_uiOut_select"),
                        conditionalPanel(
                          condition = "input.overlay_proj_method == 3",
                          numericInput("overlay_proj_epsg", tags$h5("EPSG code"), value = 4326, step = 1),
                          helpText("See", tags$a("epsg.io", href = "http://epsg.io/"), "or the",
                                   tags$a("EPSG home page", href = "http://www.epsg.org/"), "for more information about EPSG codes")
                        )
                      )
                    )
                  )
                ),
                column(
                  width = 6,
                  fluidRow(
                    box(
                      width = 12,
                      tags$strong("4) Overlay options: percent overlap threshold"),
                      tags$h5("Specify the percent overlap threshold."),
                      helpText("The percent overlap threshold is the minimum percentage of a base geometry polygon",
                               "that must overlap with overlaid prediction polygons.",
                               "All base geometry polygons with an overlap percentage less than this threshold",
                               "will be assigned an overlaid prediction value of 'NA'.",
                               tags$br(),
                               "A threshold of '0' means that base geometry polygons will not be assigned a prediction value of 'NA'",
                               "if they overlap with any original predictions."),
                      sliderInput("overlay_grid_coverage", label = NULL, min = 0, max = 100, value = 50)
                    ),
                    box(
                      width = 12,
                      tags$strong("5) Perform overlay"),
                      helpText(tags$strong("Reminder: imported study area and erasing polygons will be used during",
                                           "will overwrite previously created overlaid predictions.")),
                      actionButton("overlay_create_overlaid_models_modal", "Overlay all predictions onto the specified base geometry"),
                      textOutput("overlay_overlay_all_text"),
                      tags$br(),
                      tags$span(uiOutput("overlay_overlaid_models_message"), style = "color: blue")
                    )
                  )
                )
              )
            )
          ),
          fluidRow(
            box(
              title = "Base Geometry and Overlaid Predictions Previews", status = "primary", solidHeader = TRUE, width = 12,
              collapsible = TRUE,
              fluidRow(
                column(3, radioButtons("overlay_preview_which", NULL,
                                       choices = list("Base geometry preview" = 1, "Overlaid predictions preview" = 2))),
                column(
                  width = 9,
                  fluidRow(
                    box(
                      width = 12,
                      conditionalPanel(
                        condition = "input.overlay_preview_which == 1",
                        helpText("The base geometry preview will be displayed in the Leaflet default coordinate system:",
                                 "EPSG:3857 Web Mercator.",
                                 "The base geometry will be outlined in black while, if applicable, the erasing and study area",
                                 "polygons will be filled in tan with no outline and outlined in red with no fill, respectively.",
                                 "The erasing polygon will be clipped to the extent of the base geometry,",
                                 "plus two degrees in each direction.",
                                 tags$br(), tags$br(),
                                 "Any base geometry polygon that spans the antimeridian (i.e. 180 decimal degrees or the",
                                 "equivalent in the base geometry coordinate system) will appear to be split at the antimeridian.",
                                 "However, it will still be treated as a single polygon for the overlay."),
                        uiOutput("overlay_preview_base_execute_uiOut_button"),
                        textOutput("overlay_preview_base_create_text")
                      ),
                      conditionalPanel(
                        condition = "input.overlay_preview_which == 2",
                        conditionalPanel("output.overlay_preview_display_flag == false", uiOutput("overlay_preview_message")),
                        conditionalPanel(
                          condition = "output.overlay_preview_display_flag",
                          fluidRow(
                            column(5, uiOutput("overlay_preview_overlaid_models_uiOut_selectize")),
                            column(3, offset = 1, radioButtons("overlay_preview_overlaid_models_perc", tags$h5("Units"),
                                                               choices = preview.static.perc, selected = 1)),
                            column(3, radioButtons("overlay_preview_overlaid_models_var", tags$h5("Uncertainty"),
                                                   choices = preview.static.var, selected = 1))
                          ),
                          conditionalPanel(
                            condition = "input.overlay_preview_overlaid_models_var_preview_var == 2",
                            helpText("Uncertainty plots will have \"- SE\" in their title.",
                                     "Uncertainty plots of units type 'values' will have the same",
                                     "color scale as their assocaited predictions.")
                          ),
                          actionButton("overlay_preview_overlaid_execute", "Preview selected overlaid predictions")
                        )
                      )
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.overlay_preview_which == 1",
                # shinycssloaders::withSpinner(leaflet::leafletOutput("overlay_preview_base"), type = 1)
                leafletOutput("overlay_preview_base")
              ),
              conditionalPanel(
                condition = "input.overlay_preview_which == 2",
                # shinycssloaders::withSpinner(plotOutput("overlay_preview_overlaid"), type = 1)
                plotOutput("overlay_preview_overlaid")
              )
            )
          )
        )
      )
    )
  )
}
