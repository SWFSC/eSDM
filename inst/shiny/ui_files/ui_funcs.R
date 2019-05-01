# Functions for common eSDM UI structures

###############################################################################
# 'Actions to perform with selected ... predictions'

#------------------------------------------------------------------------------
### Interactive preview
ui_interactive_preview <- function(x) {
  conditionalPanel(
    condition = paste0("input.", x, "_select_action == 1"),
    fluidRow(
      column(4, radioButtons(paste0(x, "_preview_interactive_perc"),
                             tags$h5("Units"),
                             choices = preview.static.perc, selected = 1)),
      column(8, tags$br(), tags$br(),
             uiOutput(paste0(x, "_preview_interactive_execute_uiOut_button")))
    ),
    helpText("Note that if you are not connected to the internet",
             "then the background map will not display")
  )
}

#------------------------------------------------------------------------------
### Static preview
# This is not called for overlaid preview because ui setup is different
ui_static_preview <- function(x, over.flag = FALSE) {
  conditionalPanel(
    condition = paste0("input.", x, "_select_action == 2"),
    fluidRow(
      column(4, radioButtons(paste0(x, "_preview_perc"), tags$h5("Units"),
                             choices = preview.static.perc, selected = 1)),
      column(8, radioButtons(paste0(x, "_preview_var"), tags$h5("Uncertainty"),
                             choices = preview.static.var, selected = 1))
    ),
    conditionalPanel(
      condition = paste0("input.", x, "_preview_var == 2"),
      helpText("Uncertainty plots will have \"- SE\" in their title.",
               "Uncertainty plots of units type 'values' will have the same",
               "color scale as their assocaited predictions.")
    ),
    actionButton(paste0(x, "_preview_execute"), "Plot static preview")
  )
}
#------------------------------------------------------------------------------
### Download preview
ui_download_preview <- function(x) {
  conditionalPanel(
    condition = paste0("input.", x, "_select_action == 3"),
    fluidRow(
      column(4, radioButtons(paste0(x, "_download_preview_perc"), tags$h5("Units"),
                             choices = preview.static.perc, selected = 1)),
      column(4, radioButtons(paste0(x, "_download_preview_var"), tags$h5("Uncertainty"),
                             choices = preview.static.var, selected = 1)),
      column(4, radioButtons(paste0(x, "_download_preview_res"), tags$h5("Resolution"),
                             choices = list("High (300 ppi)" = 1, "Low (72 ppi)" = 2),
                             selected = 1))
    ),
    fluidRow(
      column(4, radioButtons(paste0(x, "_download_preview_format"), tags$h5("File format"),
                             choices = list("JPEG" = 1, "PDF" = 2, "PNG" = 3),
                             selected = 3)),
      column(8, radioButtons(paste0(x, "_download_preview_dim"), tags$h5("File dimensions"),
                             choices = preview.download.dim, selected = 1))

    ),
    uiOutput(paste0(x, "_download_preview_name_uiOut_text")),
    uiOutput(paste0(x, "_download_preview_execute_uiOut_download"))
  )
}

#------------------------------------------------------------------------------
### Revove predictions
ui_remove <- function(x) {
  conditionalPanel(
    condition = paste0("input.", x, "_select_action == 4"),
    actionButton(paste0(x, "_remove_execute"),
                 "Remove selected ensemble predictions"),
    textOutput(paste0(x, "_remove_text"))
  )
}
