### Code for 'High Quality Maps' tab for adding parameters to reactive values
# maps 'toplot' aka saved maps. 'toplot' used in various names


###############################################################################
### Add data to pretty plot reactive variables
pretty_toplot_add <- eventReactive(input$pretty_toplot_add_execute, {
  validate(
    need(pretty_models_idx_count() > 0,
         paste("Error: Please select at least one set of",
               "predictions to map"))
  )

  if (isTruthy(vals$pretty.toplot.idx)) {
    id.used <- pretty_toplot_table()$ID
    validate(
      need(!(input$pretty_toplot_add_id %in% id.used),
           "Error: Each map must have a unique ID")
    )
  }
  validate( #!is.null() in case values are NA
    need(!is.null(input$pretty_range_xmin) &&
           !is.null(input$pretty_tick_lon_start),
         paste("Error: Please wait until the parameter inputs below",
               "have finished loading"))
  )

  if (input$pretty_addobj) { # to get check earlier
    validate(
      need(vals$pretty.addobj,
           paste("Error: Please either load additional objects or uncheck",
                 "the 'Include addition objects box'"))
    )
  }

  validate(
    need(is.numeric(pretty_map_range()) && !anyNA(pretty_map_range()),
         "Error: All map range entries must be numbers")
  )


  # Get/set plotting variables
  withProgress(message = "Processing map parameters", value = 0.3, {
    #--------------------------------------------------------------------------
    #------------------------------------------------------
    # Simpler operations; happen first in case user clicks around
    map.range <- pretty_map_range()
    background.color <- input$pretty_background_color
    pretty.id <- input$pretty_toplot_add_id

    list.legend   <- pretty_legend_list()
    list.titlelab <- pretty_titlelab_list()
    list.margin   <- pretty_margin_list()
    list.tick     <- pretty_tick_list()
    list.idx      <- list(pretty_models_idx_list())
    incProgress(0.1)


    #------------------------------------------------------
    model.toplot <- pretty_model_toplot()
    range.poly <- pretty_range_poly_func(
      pretty_map_range(), st_crs(model.toplot)
    )
    incProgress(0.1)

    if (pretty_range_360()) {
      # This is here so incProgress() can be called
      # Don't need check_preview360() becuase check was already done above
      incProgress(0, detail = "Processing predictions that span dateline")
      model.toplot <- pretty_model_toplot360()
      incProgress(0, detail = "")
    }

    model.int <- pretty_int_func( #contains validate()
      model.toplot, range.poly, "selected predictions"
    )
    rm(model.int)
    incProgress(0.2)


    #------------------------------------------------------
    # Can be more complex operations
    incProgress(0, detail = "Processing additional objects")
    list.colorscheme <- pretty_colorscheme_list()
    list.addobj <- if (input$pretty_addobj) pretty_addobj_list() else NULL
    incProgress(0.2, detail = "")


    #--------------------------------------------------------------------------
    # Save plot parameters to reactive values
    vals$pretty.params.toplot <- c(
      vals$pretty.params.toplot,
      list(list(
        model.toplot = model.toplot, map.range = map.range,
        background.color = background.color,
        list.titlelab = list.titlelab, list.margin = list.margin,
        list.tick = list.tick,
        list.colorscheme = list.colorscheme, list.legend = list.legend,
        list.addobj = list.addobj,
        id = pretty.id, se.flag = TRUE
      ))
    )
    vals$pretty.toplot.idx <- c(vals$pretty.toplot.idx, list.idx)


    #--------------------------------------------------------------------------
    # If applicable, save a second plot of the associated SE
    if (isTruthy(input$pretty_toplot_se) & any(!is.na(model.toplot$SE))) {
      #input$pretty_toplot_se is NULL is ensemble isn't selected
      incProgress(0, detail = "Creating map of the associated SE")

      se.sf <- model.toplot

      # preview360_split() is what is used in pretty_model_toplot360()
      # if (check_360(se.sf)) se.sf <- preview360_split(se.sf)
      # Seems like don't need ^ because model.toplot is already split

      validate(
        need(identical(st_geometry(model.toplot), st_geometry(se.sf)),
             paste("Error creating map of associated SE;",
                   "please report this as an issue")
        )
      )
      incProgress(0.1)

      #--------------------------------------------------
      # Update parameters as necessary

      ### Color scheme
      list.colorscheme.var <- list.colorscheme
      list.colorscheme.var$data.name <- ifelse(
        isTruthy(pretty_models_idx_list()[[3]]), "SE_ens", "SE"
      )
      se.vals <- st_set_geometry(se.sf, NULL)[, list.colorscheme.var$data.name]

      if (list.colorscheme.var$perc) {
        list.colorscheme.var$data.breaks <- breaks_calc(se.vals)

      } else {
        # Update min and max
        tmp <- list.colorscheme.var$data.breaks
        list.colorscheme.var$data.breaks[1] <- min(
          c(se.vals, tmp), na.rm = TRUE
        )
        list.colorscheme.var$data.breaks[length(tmp)] <- max(
          c(se.vals, tmp), na.rm = TRUE
        )

        tmp.check <- dplyr::between(
          se.vals, min(list.colorscheme.var$data.breaks),
          max(list.colorscheme.var$data.breaks)
        )
        validate(
          need(all(tmp.check, na.rm = TRUE),
               paste("Error creating map of associated SE;",
                     "please report this as an issue"))
        )
        rm(tmp, tmp.check)
      }

      ### Title
      list.titlelab.var <- list.titlelab
      if (list.titlelab.var$title != "") {
        list.titlelab.var$title <- paste(list.titlelab.var$title, "SE")
      }

      ### Map ID
      pretty.id.se <- paste0(pretty.id, "_SE")

      #--------------------------------------------------
      # Save SE map
      vals$pretty.params.toplot <- c(
        vals$pretty.params.toplot,
        list(list(
          model.toplot = se.sf, map.range = map.range,
          background.color = background.color,
          list.titlelab = list.titlelab.var, list.margin = list.margin,
          list.tick = list.tick,
          list.colorscheme = list.colorscheme.var, list.legend = list.legend,
          list.addobj = list.addobj,
          id = pretty.id.se, se.flag = TRUE
        ))
      )
      vals$pretty.toplot.idx <- c(vals$pretty.toplot.idx, list.idx)

    } else {
      incProgress(0.1, detail = "")
    }
  })

  if (exists("pretty.id.se")) {
    paste0("Saved maps '", pretty.id, "'", "and ", "'", pretty.id.se, "'")
  } else {
    paste0("Saved map '", pretty.id, "'")
  }
})


###############################################################################
### Table
pretty_toplot_table <- reactive({
  validate(
    need(vals$pretty.toplot.idx, "No maps have been saved"),
    errorClass = "validation2"
  )

  data.frame(
    Predictions = sapply(vals$pretty.toplot.idx, function(i) {
      switch(
        which(!sapply(i, is.null)),
        row.names(table_orig())[i[[1]]],
        row.names(table_overlaid())[i[[2]]],
        row.names(table_ensembles())[i[[3]]]
      )
    }),
    ID = sapply(vals$pretty.params.toplot, function(i) i$id),
    stringsAsFactors = FALSE
  )
})


###############################################################################
### Remove stuff from list
pretty_toplot_remove <- eventReactive(input$pretty_toplot_remove_execute, {
  req(vals$pretty.params.toplot)

  x <- input$pretty_update_table_out_rows_selected
  validate(
    need(x, "Error: You must select a saved map to remove")
  )

  vals$pretty.params.toplot <- vals$pretty.params.toplot[-x]
  vals$pretty.toplot.idx <- vals$pretty.toplot.idx[-x]

  if (length(vals$pretty.params.toplot) == 0) vals$pretty.params.toplot <- NULL
  if (length(vals$pretty.toplot.idx) == 0) vals$pretty.toplot.idx <- NULL

  ""
})

###############################################################################
