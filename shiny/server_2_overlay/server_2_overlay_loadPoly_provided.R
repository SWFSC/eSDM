### Code for loading in GSHHG land polygons


# When user clicks button, load selected provided land poly(s)
overlay_land_prov <- eventReactive(input$overlay_land_provided, {
  withProgress(message = "Loading specified GSHHG land polygon", value = 0.5, {
    provided.res <- as.numeric(input$overlay_land_provided_res)

    githubURL.start <- "https://github.com/smwoodman/eSDM/raw/master/data_land_gshhg/"
    githubURL <- paste0(githubURL.start, 
                        switch(provided.res, 
                               "gshhg_f_L16.RDATA", 
                               "gshhg_h_L16.RDATA", 
                               "gshhg_i_L16.RDATA", 
                               "gshhg_l_L16.RDATA", 
                               "gshhg_c_L16.RDATA"))
    
    x <- try(load(url(githubURL)))  # Loads sfc object named 'gshhg.L16'
    validate(
      need((class(x) != "try-error") & exists("gshhg.L16"), 
           paste("Error: The eSDM was not able to access the specified GSHHG", 
                 "land polygon;", 
                 "please ensure that you have an active internet connection"))
      )
    incProgress(0.5)
  })
  
  vals$overlay.land <- gshhg.L16
  
  ""
})