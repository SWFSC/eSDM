# Download static previews of original or ensemble predictions

##############################################################################
# Function for checking if download can be performed
# x must be a numeric vector
download_check <- function(x, perc) {
  validate(
    need(x,
         paste("The GUI cannot download a preview of the selected",
               "predictions because all of the prediction values are NA"))
  )

  if (perc == 1) {
    validate(
      need(length(unique(breaks_calc(x))) >= 11,
           paste("At least one of the selected predictions does not",
                 "have enough unique prediction values to download",
                 "a preview with a 'percentage' unit type")),
      errorClass = "validation2"
    )
  }

  TRUE
}

# x is the file extension widget value and y is the filename
download_plot_ext <- function(x, y) {
  file.ext <- switch(as.numeric(x), ".jpeg", ".pdf", ".png")
  paste0(y, file.ext)
}

###############################################################################
### Import Predictions tab download
# 'filename' arg needs 'function()' so that it will change as widgets change
output$model_download_preview_execute <- downloadHandler(
  filename = function() {
    download_plot_ext(
      input$model_download_preview_format, input$model_download_preview_name
    )
  },

  content = function(file) {
    withProgress(message = "Downloading preview", value = 0.6, {
      #-------------------------------------------------------------
      # Process relevant user inputs
      models.idx <- as.numeric(input$models_loaded_table_rows_selected)
      models.num <- length(models.idx)

      req(length(vals$models.ll) > 0, models.num > 0)

      perc.num <- as.numeric(input$model_download_preview_perc)
      models.toplot <- vals$models.ll[models.idx]
      data.name <- rep("Pred", models.num)
      plot.titles <- paste("Original", models.idx)
      var.key <- NULL

      stopifnot(models.num == length(models.toplot))

      # Add uncertainty plots if necessary
      if (input$model_download_preview_var == 2) {
        models.toplot <- c(models.toplot, models.toplot)
        data.name <- c(data.name, rep("SE", models.num))
        plot.titles <- c(plot.titles, paste("Original", models.idx, "- SE"))

        if (perc.num == 2) var.key <- c(rep(NA, models.num), seq_len(models.num))
        models.num <- models.num * 2
      }

      # Additional prep
      pal.download <- switch(perc.num, pal.esdm, NA)

      x <- session$clientData$output_model_preview_plot_width
      y <- session$clientData$output_model_preview_plot_height

      if (input$model_download_preview_dim == 2) {
        req(x, y)
        plot.dims <- multiplot_inapp(models.num)
        pf.dim1 <- x / 72
        pf.dim2 <- y / 72

      } else {
        plot.dims <- multiplot_download(models.num)
        pf.dim1 <- pf.dim2 <- 8
      }


      #-------------------------------------------------------------
      plot.res <- ifelse(input$model_download_preview_res == "1", 300, 72)
      pdf.res  <- ifelse(input$model_download_preview_res == "1", 15, 7)
      plot.format <- as.numeric(input$model_download_preview_format)


      #-------------------------------------------------------------
      # Generate and save preview plots
      if (plot.format == 1) {
        jpeg(file, width = pf.dim1, height = pf.dim2, units = 'in', res = plot.res,
             quality = 150)
        multiplot_layout(
          models.toplot, data.name, plot.titles,
          perc.num, pal.download, leg.perc.esdm, plot.dims[1], plot.dims[2],
          plot.dims[3], plot.dims[4], plot.dims[5], plot.dims[6],
          plot.dims[7:10], var.key
        )
        dev.off()

      } else if (plot.format == 2) {
        pdf(file, width = pdf.res, height = pdf.res)
        multiplot_layout(
          models.toplot, data.name, plot.titles,
          perc.num, pal.download, leg.perc.esdm, plot.dims[1], plot.dims[2],
          plot.dims[3], plot.dims[4], plot.dims[5], plot.dims[6],
          plot.dims[7:10], var.key
        )
        dev.off()

      } else if (plot.format == 3) {
        png(file, width = pf.dim1, height = pf.dim2, units = "in", res = plot.res)
        multiplot_layout(
          models.toplot, data.name, plot.titles,
          perc.num, pal.download, leg.perc.esdm, plot.dims[1], plot.dims[2],
          plot.dims[3], plot.dims[4], plot.dims[5], plot.dims[6],
          plot.dims[7:10], var.key
        )
        dev.off()
      }
      incProgress(0.4)
    })
  }
)


###############################################################################
### Create Ensemble Predictions tab download
# 'filename' arg needs 'function()' so that it will change as widgets change
output$ens_download_preview_execute <- downloadHandler(
  filename = function() {
    download_plot_ext(
      input$ens_download_preview_format, input$ens_download_preview_name
    )
  },

  content = function(file) {
    withProgress(message = "Downloading preview", value = 0.6, {
      #-------------------------------------------------------------
      # Process relevant user inputs
      models.idx <- as.numeric(input$ens_datatable_ensembles_rows_selected)
      models.num <- length(models.idx)

      req(length(vals$ensemble.models) > 0, models.num > 0)

      perc.num <- as.numeric(input$ens_download_preview_perc)
      models.toplot <- lapply(
        vals$ensemble.models[models.idx], st_sf,
        geometry = vals$overlay.base.sfc, agr = "constant"
      )
      data.name <- rep("Pred_ens", models.num)
      plot.titles <- paste("Ensemble", models.idx)
      var.key <- NULL

      stopifnot(models.num == length(models.toplot))

      # Add uncertainty plots if necessary
      if (input$ens_download_preview_var == 2) {
        models.toplot <- c(models.toplot, models.toplot)
        data.name <- c(data.name, rep("SE_ens", models.num))
        plot.titles <- c(plot.titles, paste("Original", models.idx, "- SE"))

        if (perc.num == 2) var.key <- c(rep(NA, models.num), seq_len(models.num))
        models.num <- models.num * 2
      }

      # Additional prep
      pal.download <- switch(perc.num, pal.esdm, NA)

      x <- session$clientData$output_ens_preview_plot_width
      y <- session$clientData$output_ens_preview_plot_height

      if (input$ens_download_preview_dim == 2) {
        req(x, y)
        plot.dims <- multiplot_inapp(models.num)
        pf.dim1 <- x / 72
        pf.dim2 <- y / 72

      } else {
        plot.dims <- multiplot_download(models.num)
        pf.dim1 <- pf.dim2 <- 8
      }


      #-------------------------------------------------------------
      plot.res <- ifelse(input$ens_download_preview_res == "1", 300, 72)
      pdf.res  <- ifelse(input$ens_download_preview_res == "1", 15, 7)
      plot.format <- as.numeric(input$ens_download_preview_format)


      #-------------------------------------------------------------
      # Generate and save preview plots
      if (plot.format == 1) {
        jpeg(file, width = pf.dim1, height = pf.dim2, units = 'in', res = plot.res,
             quality = 150)
        multiplot_layout(
          models.toplot, data.name, plot.titles,
          perc.num, pal.download, leg.perc.esdm, plot.dims[1], plot.dims[2],
          plot.dims[3], plot.dims[4], plot.dims[5], plot.dims[6],
          plot.dims[7:10], var.key
        )
        dev.off()

      } else if (plot.format == 2) {
        pdf(file, width = pdf.res, height = pdf.res)
        multiplot_layout(
          models.toplot, data.name, plot.titles,
          perc.num, pal.download, leg.perc.esdm, plot.dims[1], plot.dims[2],
          plot.dims[3], plot.dims[4], plot.dims[5], plot.dims[6],
          plot.dims[7:10], var.key
        )
        dev.off()

      } else if (plot.format == 3) {
        png(file, width = pf.dim1, height = pf.dim2, units = "in", res = plot.res)
        multiplot_layout(
          models.toplot, data.name, plot.titles,
          perc.num, pal.download, leg.perc.esdm, plot.dims[1], plot.dims[2],
          plot.dims[3], plot.dims[4], plot.dims[5], plot.dims[6],
          plot.dims[7:10], var.key
        )
        dev.off()
      }
      incProgress(0.4)
    })
  }
)


###############################################################################
