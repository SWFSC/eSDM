# Download manuscript data, sample data, or manual
# NOTEs:
#   The shiny app must finish fully loading/rendering before
#   the user can download files.
#   If user clicks download button too soon, they'll get html download

###############################################################################
# Download manuscript data
output$download_data_manuscript <- downloadHandler(
  filename = function() "eSDM_data_manuscript.zip",

  content = function(file) {
    withProgress(message = "Downloading manuscript data", value = 0.6, {
      sample.try <- try(
        download.file(
          "https://github.com/smwoodman/eSDM-data/raw/master/eSDM_data_manuscript.zip",
          destfile = file, quiet = TRUE
        ),
        silent = TRUE
      )

      req(sample.try)
      incProgress(0.4)
    })
  }
)


###############################################################################
# Download sample data
output$download_data_sample <- downloadHandler(
  filename = function() "eSDM_data_sample.zip",

  content = function(file) {
    withProgress(message = "Downloading sample data", value = 0.6, {
      sample.try <- try(
        download.file(
          "https://github.com/smwoodman/eSDM-data/raw/master/eSDM_data_sample.zip",
          destfile = file, quiet = TRUE
        ),
        silent = TRUE
      )

      req(sample.try)
      incProgress(0.4)
    })
  }
)


###############################################################################
# Download manual
output$download_manual <- downloadHandler(
  filename = function() "eSDM_manual.pdf",

  content = function(file) {
    withProgress(message = "Downloading manual", value = 0.6, {
      sample.try <- try(
        download.file(
          "https://github.com/smwoodman/eSDM/raw/master/inst/shiny/www/eSDM_manual.pdf",
          destfile = file, quiet = TRUE, mode = "wb"
        ),
        silent = TRUE
      )

      req(sample.try)
      incProgress(0.4)
    })
  }
)

###############################################################################
