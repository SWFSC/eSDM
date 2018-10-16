# Download manuscript data, sample data, or manual

###############################################################################
# Download manuscript data
output$download_data_manuscript <- downloadHandler(
  filename = "eSDM_data_manuscript.zip",

  content = function(file) {
    withProgress(message = "Downloading manuscript data", value = 0.6, {
      sample.try <- try(
        download.file(
          "https://github.com/smwoodman/eSDM-data/raw/master/eSDM_data_manuscript.zip",
          destfile = file, quiet = TRUE
        ),
        silent = TRUE
      )

      req(sample.try) #validate() does nothing inside downloadHandler()
      # validate(
      #   need(sample.try,
      #        paste("The manuscript data could not be downloaded; please check",
      #              "your internet connection. If this problem persists, please",
      #              "report this issue at https://github.com/smwoodman/eSDMissues"))
      # )
      incProgress(0.4)
    })
  }
)


###############################################################################
# Download sample data
output$download_data_sample <- downloadHandler(
  filename = "eSDM_data_sample.zip",

  content = function(file) {
    withProgress(message = "Downloading sample data", value = 0.6, {
      sample.try <- try(
        download.file(
          "https://github.com/smwoodman/eSDM-data/raw/master/eSDM_data_sample.zip",
          destfile = file, quiet = TRUE
        ),
        silent = TRUE
      )

      req(sample.try) #validate() does nothing inside downloadHandler()
      # validate(
      #   need(sample.try,
      #        paste("The sample data could not be downloaded; please check",
      #              "your internet connection. If this problem persists, please",
      #              "report this issue at https://github.com/smwoodman/eSDM/issues"))
      # )
      incProgress(0.4)
    })
  }
)


###############################################################################
# Download manual
output$download_manual <- downloadHandler(
  filename = "eSDM_manual.pdf",

  content = function(file) {
    withProgress(message = "Downloading manual", value = 0.6, {
      sample.try <- try(
        download.file(
          "https://github.com/smwoodman/eSDM/raw/master/inst/shiny/www/eSDM_manual.pdf",
          destfile = file, quiet = TRUE, mode = "wb"
        ),
        silent = TRUE
      )

      req(sample.try) #validate() does nothing inside downloadHandler()
      # validate(
      #   need(sample.try,
      #        paste("The manual could not be downloaded; please check",
      #              "your internet connection. If this problem persists, please",
      #              "report this issue at https://github.com/smwoodman/eSDM/issues"))
      # )
      incProgress(0.4)
    })
  }
)


###############################################################################
