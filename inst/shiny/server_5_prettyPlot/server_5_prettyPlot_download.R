### Download plots from 'High Quality Maps' tab


###############################################################################

### Download handler - High Quality Maps
output$pretty_plot_download_execute <- downloadHandler(
  filename = function() input$pretty_plot_download_name,

  content = function(file) {
    withProgress(message = "Downloading high quality map", value = 0.4, {
      req(p.list <- vals$pretty.plot.list)

      plot.res <- ifelse(input$pretty_plot_download_res == "1", 300, 72)
      pdf.res  <- ifelse(input$pretty_plot_download_res == "1", 15, 7)
      plot.format <- input$pretty_plot_download_format
      incProgress(0.2)

      if (plot.format == 1) {
        jpeg(file, width = 12, height = 12, units = 'in', res = plot.res,
             quality = 150)
        plot_pretty_top(p.list$dims, p.list$idx.list, p.list$params.list)
        dev.off()

      } else if (plot.format == 2) {
        pdf(file, width = pdf.res, height = pdf.res)
        plot_pretty_top(p.list$dims, p.list$idx.list, p.list$params.list)
        dev.off()

      } else if (plot.format == 3) {
        png(file, width = 12, height = 12, units = "in", res = plot.res)
        plot_pretty_top(p.list$dims, p.list$idx.list, p.list$params.list)
        dev.off()

      } else {
        validate(need(FALSE, "Error: pretty plot download"))
      }
      incProgress(0.4)
    })
  }
)


###############################################################################
