### Code for downloading previews of predictions


###############################################################################
# Save preview of model predictions
# print()'s are needed to actually display spplots stored in reac funcs

### Load Models tab preview
output$model_download_preview_execute <- downloadHandler(
  filename = function() {
    input$model_download_preview_name
  },
  
  content = function(file) {
    withProgress(message = "Downloading preview", value = 0.6, { 
      Sys.sleep(0.5)
      # plot.toprint <- model_pix_download()
      plot.res <- ifelse(input$model_download_preview_res == "1", 300, 72)
      pdf.res  <- ifelse(input$model_download_preview_res == "1", 15, 7)
      plot.format <- input$model_download_preview_format
      
      if(plot.format == 1) {
        jpeg(file, width = 4, height = 4, units = 'in', res = plot.res, 
             quality = 150)
        model_pix_download()
        dev.off()
      }
      if(plot.format == 2) {
        pdf(file, width = pdf.res, height = pdf.res)
        model_pix_download()
        dev.off()
      }
      if(plot.format == 3) {
        png(file, width = 4, height = 4, units = "in", res = plot.res)
        model_pix_download()
        dev.off()
      }
      incProgress(0.4)
    })
  }
)


###########################################################
### Create Ensemble Predictions tab preview
output$ens_download_preview_execute <- downloadHandler(
  filename = function() {
    input$ens_download_preview_name
  },
  
  content = function(file) {
    withProgress(message = "Downloading preview", value = 0.6, { 
      Sys.sleep(0.5)
      plot.toprint <- ens_pix_download()
      plot.res <- ifelse(input$ens_download_preview_res == "1", 300, 72)
      pdf.res  <- ifelse(input$ens_download_preview_res == "1", 15, 7)
      plot.format <- input$ens_download_preview_format
      
      if(plot.format == 1) {
        jpeg(file, width = 4, height = 4, units = 'in', res = plot.res,
             quality = 150)
        grid.arrange(plot.toprint)
        dev.off()
      }
      if(plot.format == 2) {
        pdf(file, width = pdf.res, height = pdf.res)
        grid.arrange(plot.toprint)
        dev.off()
      }
      if(plot.format == 3) {
        png(file, width = 4, height = 4, units = "in", res = plot.res)
        grid.arrange(plot.toprint)
        dev.off()
      }
    })
  }
)


###############################################################################
