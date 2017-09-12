### Code for loading in GSHHG land polygons


# When user clicks button, load selected provided land poly(s)
observeEvent(input$overlay_land_provided, {
  withProgress(message = "Loading specified land polygon", value = 0.5, {
    provided.res <- input$overlay_land_provided_res
    filename.provided <- paste0("data/", switch(provided.res, 
                                                "1" = "gshhg_f_L1.RDATA", 
                                                "2" = "gshhg_h_L1.RDATA", 
                                                "3" = "gshhg_i_L1.RDATA", 
                                                "4" = "gshhg_l_L1.RDATA", 
                                                "5" = "gshhg_c_L1.RDATA"))
    
    load(filename.provided) # Loads object named 'gshhg.L1'
    incProgress(0.5)
  })
  
  vals$overlay.land <- gshhg.L1
})