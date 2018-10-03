### Code for loading in GSHHG land polygons


# When user clicks button, load selected provided land poly(s)
overlay_land_prov <- eventReactive(input$overlay_land_provided, {
  withProgress(message = "Importing provided erasing polygon", value = 0.5, {
    vals$overlay.land <- NULL

    temp <- try(eSDM::gshhg.l.L16, silent = TRUE)

    validate(
      need(inherits(temp, "sfc"),
           paste("Error: The GUI was not able to access the provided erasing",
                 "polygon; try reinstalling eSDM or restarting the GUI"))
    )
    incProgress(0.5)
    vals$overlay.land <- temp
  })

  ""
})
