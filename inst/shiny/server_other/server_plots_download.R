###############################################################################
### Load Models tab preview download
output$model_download_preview_execute <- downloadHandler(
  filename = function() input$model_download_preview_name,

  content = function(file) {
    withProgress(message = "Downloading preview", value = 0.6, {
      #-------------------------------------------------------------
      perc.num <- as.numeric(input$model_download_preview_perc)
      models.idx <- as.numeric(input$models_loaded_table_rows_selected)
      models.num <- length(models.idx)

      req(length(vals$models.ll) > 0, length(models.idx) > 0)

      models.toplot <- vals$models.ll[models.idx]
      stopifnot(models.num == length(models.toplot))
      # plot.titles <- sapply(models.idx, function(i) {
      #   paste(vals$models.data.names[[i]][1], "|", vals$models.names[i])
      # })
      plot.titles <- vals$models.names[models.idx]
      plot.dims <- multiplot_download(models.num)


      #-------------------------------------------------------------
      plot.res <- ifelse(input$model_download_preview_res == "1", 300, 72)
      pdf.res  <- ifelse(input$model_download_preview_res == "1", 15, 7)
      plot.format <- as.numeric(input$model_download_preview_format)


      #-------------------------------------------------------------
      if (plot.format == 1) {
        jpeg(file, width = 4, height = 4, units = 'in', res = plot.res,
             quality = 150)
        eSDM::multiplot_layout(
          models.toplot, rep("Pred", models.num), plot.titles,
          perc.num, pal.esdm, leg.perc.esdm, plot.dims[1], plot.dims[2],
          plot.dims[3], plot.dims[4], plot.dims[5], plot.dims[6]
        )
        dev.off()

      } else if (plot.format == 2) {
        pdf(file, width = pdf.res, height = pdf.res)
        eSDM::multiplot_layout(
          models.toplot, rep("Pred", models.num), plot.titles,
          perc.num, pal.esdm, leg.perc.esdm, plot.dims[1], plot.dims[2],
          plot.dims[3], plot.dims[4], plot.dims[5], plot.dims[6]
        )
        dev.off()

      } else if (plot.format == 3) {
        # browser()
        png(file, width = 4, height = 4, units = "in", res = plot.res)
        eSDM::multiplot_layout(
          models.toplot, rep("Pred", models.num), plot.titles,
          perc.num, pal.esdm, leg.perc.esdm, plot.dims[1], plot.dims[2],
          plot.dims[3], plot.dims[4], plot.dims[5], plot.dims[6]
        )
        dev.off()
      }
      incProgress(0.4)
    })
  }
)


###############################################################################
### Create Ensemble Predictions tab preview download
output$ens_download_preview_execute <- downloadHandler(
  filename = function() input$ens_download_preview_name,

  content = function(file) {
    withProgress(message = "Downloading preview", value = 0.6, {
      #-------------------------------------------------------------
      perc.num <- as.numeric(input$ens_download_preview_perc)
      models.idx <- as.numeric(input$ens_datatable_ensembles_rows_selected)
      models.num <- length(models.idx)

      req(length(vals$ensemble.models) > 0, length(models.idx) > 0)

      models.toplot <- vals$ensemble.models[models.idx]
      stopifnot(models.num == length(models.toplot))
      plot.titles <- sapply(models.idx, function(i) {
        paste(vals$ensemble.method[i], "|", vals$ensemble.rescaling[i],
              "|", vals$ensemble.overlaid.idx[i])
      })
      plot.dims <- multiplot_download(models.num)


      #-------------------------------------------------------------
      plot.res <- ifelse(input$ens_download_preview_res == "1", 300, 72)
      pdf.res  <- ifelse(input$ens_download_preview_res == "1", 15, 7)
      plot.format <- as.numeric(input$ens_download_preview_format)


      #-------------------------------------------------------------
      if (plot.format == 1) {
        jpeg(file, width = 4, height = 4, units = 'in', res = plot.res,
             quality = 150)
        eSDM::multiplot_layout(
          models.toplot, rep("Pred.ens", models.num), plot.titles,
          perc.num, pal.esdm, leg.perc.esdm, plot.dims[1], plot.dims[2],
          plot.dims[3], plot.dims[4], plot.dims[5], plot.dims[6]
        )
        dev.off()

      } else if (plot.format == 2) {
        pdf(file, width = pdf.res, height = pdf.res)
        eSDM::multiplot_layout(
          models.toplot, rep("Pred.ens", models.num), plot.titles,
          perc.num, pal.esdm, leg.perc.esdm, plot.dims[1], plot.dims[2],
          plot.dims[3], plot.dims[4], plot.dims[5], plot.dims[6]
        )
        dev.off()

      } else if (plot.format == 3) {
        png(file, width = 4, height = 4, units = "in", res = plot.res)
        eSDM::multiplot_layout(
          models.toplot, rep("Pred.ens", models.num), plot.titles,
          perc.num, pal.esdm, leg.perc.esdm, plot.dims[1], plot.dims[2],
          plot.dims[3], plot.dims[4], plot.dims[5], plot.dims[6]
        )
        dev.off()
      }
      incProgress(0.4)
    })
  }
)


###############################################################################
