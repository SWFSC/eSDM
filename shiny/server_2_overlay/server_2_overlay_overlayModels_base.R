### Reactive functions for creating base.spdf object
### Function to make polygons valid is in ...overlay_funcs.R
### All of these functions have no req() lines and thus assume...
### ...if they're called all their inputs exist


###############################################################################
### Top-level: Create base.spdf
overlay_create_base_spdf <- reactive({
  base.spdf <- overlay_valid_base()
  # The following are the vals$ versions to handle NULL cases
  overlay.bound <- vals$overlay.bound
  overlay.land <- vals$overlay.land
  
  # If boundary polygon and land exist, clip land by boundary extent
  if (!is.null(overlay.bound) & !is.null(overlay.land)) {
    base.spdf <- overlay_clip_base_bound()
    overlay.land <- overlay_clip_land_bound()
  } else {
    # If only boundary exists, clip base with boundary
    if (!is.null(overlay.bound)) base.spdf <- overlay_clip_base_bound()
    # If only land exists, clip land with base
    if (!is.null(overlay.land)) overlay.land <- overlay_clip_land_base()
  }
  
  # Remove land from base polygon
  if (!is.null(overlay.land)) {
    base.spdf <- erase(base.spdf, overlay.land)
    # Report any sort of 'stats', ie area erased?
  }
  
  # Make sure final version of base polygon is valid
  if (!suppressWarnings(gIsValid(base.spdf))) {
    base.spdf <- validate.poly(base.spdf, "Base final")
  }
  
  base.spdf
})


###############################################################################
# If applicable: clip base by bound and clip land by bound or base extent...
# ...to minimize computation in later functions

### Clip land polygon by base using gClipExtent
overlay_clip_land_base <- reactive({
  overlay.land <- overlay_proj_land()
  base.spdf <- overlay_valid_base() 
  
  land.base.try <- try(gClipExtent(overlay.land, base.spdf), 
                       silent = TRUE)
  
  if (class(land.base.try) == "try-error") {
    gClipExtent(overlay_valid_land(), base.spdf)
  } else {
    if (!suppressWarnings(gIsValid(land.base.try))) {
      land.base.try <- validate.poly(land.base.try, "land.base.try")
    }
    land.base.try
  }
})

### Clip land polygon by boundary using gClipExtent()
# gClipExtent() is used rather than gIntersection so that...
# ...warnings about too few points in geometry don't happen when erasing
overlay_clip_land_bound <- reactive({
  overlay.land <- overlay_proj_land()
  overlay.bound <- overlay_proj_bound()
  
  land.bound.try <- try(gClipExtent(overlay.land, overlay.bound, 0), 
                        silent = TRUE)
  
  if (class(land.bound.try) == "try-error") {
    gClipExtent(overlay_valid_land(), overlay_valid_bound(), 0)
  } else {
    if (!suppressWarnings(gIsValid(land.bound.try))) {
      land.bound.try <- validate.poly(land.bound.try, "land.base.try")
    }
    land.bound.try
  }
})

### Clip base by boundary using intersect()
overlay_clip_base_bound <- reactive({
  base.spdf <- overlay_valid_base()
  overlay.bound <- overlay_proj_bound()
  
  base.bound.try <- try(intersect(base.spdf, overlay.bound), silent = TRUE)
  
  if (class(base.bound.try) == "try-error") {
    intersect(base.spdf, overlay_valid_bound())
  } else {
    base.bound.try
  }
})


###############################################################################
# Validate base, bound, and land polys using gBuffer (if necessary)
# Called if various intersects throw an error

### Validate base spdf
overlay_valid_base <- reactive({
  base.spdf <- overlay_proj_base()
  
  if (!suppressWarnings(gIsValid(base.spdf))) {
    validate.poly(base.spdf, "Base")
  } else {
    base.spdf
  }
})

### Validate boundary polygon
overlay_valid_bound <- reactive({
  overlay.bound <- overlay_proj_bound()
  
  if (!suppressWarnings(gIsValid(overlay.bound))) { 
    validate.poly(overlay.bound, "Boundary")
  } else {
    overlay.bound
  }
})

### Validate land polygons
overlay_valid_land <- reactive({
  overlay.land <- overlay_proj_land()
  
  if (!suppressWarnings(gIsValid(overlay.land))) {
    validate.poly(overlay.land, "Land")
  } else {
    overlay.land
  }
})


###############################################################################
# Convert base.spdf, boundary poly, and land poly to specified projection

### Output base model predictions in specified projection
overlay_proj_base <- reactive({
  base.idx <- as.numeric(input$overlay_loaded_table_rows_selected)
  validate(
    need(length(base.idx) == 1, 
         "Please select exactly one model from table to preview")
  )
  base.orig <- vals$models.orig[[base.idx]]
  
  # Project if needed
  if (identical(overlay_crs(), crs.ll)) {
    overlay_base_llpre()
  } else {
    if (identical(overlay_crs(), crs(base.orig))) {
      base.orig
    } else {
      spTransform(overlay_base_llpre(), overlay_crs())
    }
  }
})

### Output boundary polygon in specified projection
overlay_proj_bound <- reactive({
  bound.curr <- vals$overlay.bound
  
  if (!identical(overlay_crs(), crs.ll)) {
    bound.curr <- spTransform(bound.curr, overlay_crs())
  }
  
  bound.curr
})

### Output land polygon in specified projection
overlay_proj_land <- reactive({
  land.curr <- overlay_clip_land_base_llpre()
  
  if (!identical(overlay_crs(), crs.ll)) {
    land.curr <- spTransform(land.curr, overlay_crs())
  }
  
  land.curr
})


###############################################################################
# _llpre() funcs, so they use data in wgs 84 geog coordinates pre-projection

### Clip land poly to base extent (plus a bit) in crs.ll using gClipExtent...
# ...so that projecting land poly doesn't take as long
overlay_clip_land_base_llpre <- reactive({
  overlay.land <- vals$overlay.land
  base.pre <- overlay_base_llpre()
  
  overlay.land.clipll <- try(gClipExtent(overlay.land, base.pre, 2), 
                             silent = TRUE)
  if (class(overlay.land.clipll) != "try-error") {
    overlay.land.clipll      
  } else {
    warning("Could not clip land_ll by base_ll, projecting could take a while")
    overlay.land
  }
})

### Get crs.ll version of base.spdf
overlay_base_llpre <- reactive({
  base.idx <- as.numeric(input$overlay_loaded_table_rows_selected)
  validate(
    need(length(base.idx) == 1, 
         "Please select exactly one model from table to preview")
  )
  vals$models.ll[[base.idx]]
})

###############################################################################
