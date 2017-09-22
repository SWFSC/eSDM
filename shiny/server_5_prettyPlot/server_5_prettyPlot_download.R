### Download plots from 'High Quality Maps' tab


###############################################################################

### Download handler - High Quality Maps
output$pretty_plot_download_execute <- downloadHandler(
  filename = function() input$pretty_plot_download_name,
  
  content = function(file) {
    withProgress(message = "Downloading high quality map", value = 0.4, { 
      plot.toprint <- pretty_plot_generate()
      plot.res <- ifelse(input$pretty_plot_download_res == "1", 300, 72)
      plot.format <- input$pretty_plot_download_format
      incProgress(0.2)
      
      if(plot.format == 1) {
        jpeg(file, width = 4, height = 4, units = 'in', res = plot.res, 
             quality = 150)
        grid.arrange(plot.toprint)
        dev.off()
      }
      if(plot.format == 2) {
        pdf(file, width = 4, height = 4)
        grid.arrange(plot.toprint)
        dev.off()
      }
      if(plot.format == 3) {
        png(file, width = 4, height = 4, units = "in", res = plot.res)
        grid.arrange(plot.toprint)
        dev.off()
      }
      incProgress(0.4)
    })
  }
)


###############################################################################