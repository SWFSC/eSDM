### Code for loading in GSHHG land polygons


# When user clicks button, load selected provided land poly(s)
overlay_land_prov <- eventReactive(input$overlay_land_provided, {
  withProgress(message = "Loading specified GSHHG land polygon", value = 0.5, {
    # Reset vals object here in case validate() is triggered
    vals$overlay.land <- NULL

    temp <- try(switch(as.numeric(input$overlay_land_provided_res),
                       eSDM::gshhg.f.L16,
                       eSDM::gshhg.h.L16,
                       eSDM::gshhg.i.L16,
                       eSDM::gshhg.l.L16,
                       eSDM::gshhg.c.L16),
                silent = TRUE)

    validate(
      need(!inherits(temp, "try-error") & inherits(temp, "sfc"),
           paste("Error: The eSDM was not able to access the specified GSHHG",
                 "land polygon; try reinstalling eSDM"))
    )
    incProgress(0.5)
    vals$overlay.land <- temp
  })

  ""
})
