### Reactive functions for creating base.spdf object
### Function to make polygons valid is in ...overlay_funcs.R
### All of these functions have no req() lines and thus assume...
### ...if they're called then all their inputs exist


###############################################################################
### Create base.spdf
overlay_create_base_sf <- reactive({
  #--------------------------------------------------------
  print("overlay_create_base_sf")
  base.sf <- overlay_valid_base()
  # The following are the vals$ versions to handle NULL cases
  overlay.bound <- vals$overlay.bound
  overlay.land  <- vals$overlay.land

  #--------------------------------------------------------
  # If boundary polygon and land exist, clip land by boundary extent
  if (!is.null(overlay.bound) & !is.null(overlay.land)) {
    base.sf <- overlay_clip_base_bound()
    overlay.land <- overlay_clip_land_bound()
  } else {
    # If only boundary exists, clip base with boundary
    if (!is.null(overlay.bound)) {
      base.sf <- overlay_clip_base_bound()
    }
    # If only land exists, land has already been cropped by base
    if (!is.null(overlay.land)) {
      overlay.land <- overlay_proj_land()
    }
  }

  validate(
    need(length(overlay.bound) == 1 & length(overlay.land) == 1,
         "Error in pre-overlay: bound and land polys not unioned")
  )

  #--------------------------------------------------------
  # Remove land from base polygon
  if (!is.null(overlay.land)) {
    base.sf <- try(st_difference(base.sf, overlay.land))
    # Adapted from https://github.com/r-spatial/sf/issues/346

    validate(
      need(isTruthy(base.sf),
           "Error: Unable to erase land from base. Please...")
    )
    # Report any sort of 'stats', ie area erased?
  }

  #--------------------------------------------------------
  # Make sure final version of base polygon is valid
  if (!all(st_is_valid(base.sf))) {
    base.sf <- try(polyValidCheck(base.sf), silent = TRUE)
  }

  validate(
    need(isTruthy(base.sf),
         "Error: Overlay cannot be performed with selected base polygon")
  )

  #--------------------------------------------------------
  base.sf
})


###############################################################################
# If applicable, clip base and land by bound poly to minimize later computation

### Clip land polygon by boundary using st_crop()
# st_crop is used rather than st_intersection to avoid tiny slices of coastline
overlay_clip_land_bound <- reactive({
  print("overlay_clip_land_bound")
  overlay.land <- overlay_proj_land()
  overlay.bound <- overlay_proj_bound()

  land.bound.try <- try(st_crop(overlay.land, st_bbox(overlay.bound)),
                        silent = TRUE)

  if (isTruthy(land.bound.try)) {
    land.bound.try

  } else {
    land.bound.try <- try(
      st_crop(overlay_valid_land(), st_bbox(overlay_valid_bound())),
      silent = TRUE
    )
    if (isTruthy(land.bound.try)) {
      land.bound.try
    } else {
      overlay.land
    }
  }
})

### Clip base by boundary using st_intersection()
overlay_clip_base_bound <- reactive({
  print("overlay_clip_base_bound")
  base.sf <- overlay_proj_base()
  overlay.bound <- overlay_proj_bound()

  base.bound.try <- try(st_intersection(base.sf, overlay.bound), silent = TRUE)

  if (isTruthy(base.bound.try)) {
    base.bound.try

  } else {
    base.bound.try <- try(
      st_intersection(overlay_valid_base(), overlay_valid_bound()),
      silent = TRUE
    )
  }

  validate(
    need(isTruthy(base.bound.try),
         "Error: Unable to intersect base with study area polygon. Please...")
  )

  base.bound.try
})


###############################################################################
# Ensure base, bound, and land polys are valid

### Ensure base spdf is valid
overlay_valid_base <- reactive({
  print("overlay_valid_base")
  base.sf <- overlay_proj_base()

  if (!all(st_is_valid(base.sf))) {
    polyValidCheck(base.sf)
  } else {
    base.sf
  }
})

### Ensure boundary polygon is valid
overlay_valid_bound <- reactive({
  print("overlay_valid_bound")
  overlay.bound <- overlay_proj_bound()

  if (!all(st_is_valid(overlay.bound))) {
    polyValidCheck(overlay.bound)
  } else {
    overlay.bound
  }
})

### Ensure land polygon is valid
overlay_valid_land <- reactive({
  print("overlay_valid_land")
  overlay.land <- overlay_proj_land()

  if (!all(st_is_valid(overlay.land))) {
    polyValidCheck(overlay.land)
  } else {
    overlay.land
  }
})


###############################################################################
# Project base, boundary poly, and land poly if necessary

### Output base model predictions in specified projection
overlay_proj_base <- reactive({
  print("overlay_proj_base")
  base.idx <- as.numeric(input$overlay_loaded_table_rows_selected)
  validate(
    need(length(base.idx) == 1,
         "Error: Please select exactly one model from table to preview")
  )
  base.orig <- st_geometry(vals$models.orig[[base.idx]])

  # Project if needed
  if (identical(overlay_crs(), crs.ll)) {
    overlay_base_llpre()
  } else {
    if (identical(overlay_crs(), crs(base.orig))) {
      base.orig
    } else {
      st_transform(overlay_base_llpre(), overlay_crs())
    }
  }
})

### Output boundary polygon in specified projection
overlay_proj_bound <- reactive({
  print("overlay_proj_bound")
  bound.curr <- vals$overlay.bound

  if (!identical(overlay_crs(), crs.ll)) {
    bound.curr <- st_transform(bound.curr, overlay_crs())
  }

  bound.curr
})

### Output land polygon in specified projection
overlay_proj_land <- reactive({
  print("overlay_proj_land")
  land.curr <- overlay_clip_land_llpre()

  if (!identical(overlay_crs(), crs.ll)) {
    land.curr <- st_transform(land.curr, overlay_crs())
  }

  land.curr
})


###############################################################################
# _llpre() funcs, so they use data in WGS 84 geog coordinates pre-projection

### Clip land poly to base extent (plus a bit) in crs.ll using gClipExtent...
# ...so that projecting land poly doesn't take as long
overlay_clip_land_llpre <- reactive({
  print("overlay_clip_land_llpre")
  overlay.land  <- vals$overlay.land
  overlay.bound <- vals$overlay.bound
  base.pre      <- overlay_base_llpre()

  if (!is.null(overlay.bound)) {
    overlay.land.clipll <- suppressMessages(
      try(st_crop(overlay.land,
                  st_bbox(suppressWarnings(st_buffer(overlay.bound, 5)))),
          silent = TRUE)
    )
  } else {
    overlay.land.clipll <- suppressMessages(
      try(st_crop(overlay.land,
                  st_bbox(suppressWarnings(st_buffer(base.pre, 5)))),
          silent = TRUE)
    )
  }

  if (isTruthy(overlay.land.clipll)) {
    st_union(overlay.land.clipll)
  } else {
    validate(need(FALSE, "Error: Issue with land prep for overlay"))
  }

})


### Get crs.ll version of base.spdf
overlay_base_llpre <- reactive({
  print("overlay_base_llpre")
  base.idx <- as.numeric(input$overlay_loaded_table_rows_selected)
  validate(
    need(length(base.idx) == 1,
         "Error: Please select exactly one model from table")
  )
  st_geometry(vals$models.ll[[base.idx]])
})

###############################################################################
