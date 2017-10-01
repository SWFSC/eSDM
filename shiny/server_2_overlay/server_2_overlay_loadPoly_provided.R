### Code for loading in GSHHG land polygons


# When user clicks button, load selected provided land poly(s)
observeEvent(input$overlay_land_provided, {
  withProgress(message = "Loading specified land polygon", value = 0.5, {
    provided.res <- as.numeric(input$overlay_land_provided_res)
    
    githubURL.start <- "https://github.com/smwoodman/Ensemble-app/raw/master/data_land_gshhg/"
    githubURL <- paste0(githubURL.start, 
                        switch(provided.res, 
                               "gshhg_f_L1.RDATA", 
                               "gshhg_h_L1.RDATA", 
                               "gshhg_i_L1.RDATA", 
                               "gshhg_l_L1.RDATA", 
                               "gshhg_c_L1.RDATA"))
    
    load(url(githubURL))  # Loads object named 'gshhg.L1'
    incProgress(0.5)
  })
  
  vals$overlay.land <- gshhg.L1
})