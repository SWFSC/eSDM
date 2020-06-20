# Reactive functions for creating base geometry (sfc object)
# Function to make polygons valid is in server_funcs.R
# All of these functions have no req() lines and thus assume
#   if they're called then all their inputs exist


###############################################################################
### Create base sf object
# Study area and erasing polys checked for length = 1 in projection functions

overlay_create_base_sf <- reactive({
  #--------------------------------------------------------
  # The following are the vals$ versions to handle NULL cases
  overlay.bound.orig <- vals$overlay.bound
  overlay.land.orig  <- vals$overlay.land

  #--------------------------------------------------------
  if (!is.null(overlay.bound.orig) & !is.null(overlay.land.orig)) {
    # If boundary polygon and land exist, clip land by boundary extent
    base.sf <- overlay_clip_base_bound()
    overlay.land <- overlay_clip_land_bound()

  } else if (!is.null(overlay.bound.orig)) {
    # If only boundary exists, clip base with boundary
    base.sf <- overlay_clip_base_bound()

  } else if (!is.null(overlay.land.orig)) {
    # If only land exists, land has already been cropped by base
    base.sf <- overlay_proj_base()
    # Redo this here without buffer since now both are in desired projection
    overlay.land <- suppressMessages(
      st_crop(overlay_proj_land(), st_bbox(base.sf))
    )

  } else {
    # No boundary or land polys, so only need base
    base.sf <- overlay_proj_base()
  }

  #--------------------------------------------------------
  # Remove land from base geometry
  if (!is.null(overlay.land.orig)) {
    # Adapted from https://github.com/r-spatial/sf/issues/346
    base.sf <- try(
      suppressMessages(st_difference(base.sf, overlay.land)), silent = TRUE
    )

    if (!isTruthy(base.sf)) {
      # Calling overlay_valid_land() means that the land isn't optimally
      # cropped, but it is worth attempting
      base.sf <- try(
        suppressMessages(
          st_difference(overlay_valid_base(), overlay_valid_land())
        ), silent = TRUE
      )
    }

    validate(
      need(isTruthy(base.sf),
           paste("Error: Unable to erase area from the base geometry;",
                 "please try using a different erasing polygon"))
    )

    st_agr(base.sf) <- "constant"
  }

  #--------------------------------------------------------
  # Ensure the final version of base geometry is valid
  if (!all(st_is_valid(base.sf))) base.sf <- check_valid(base.sf)

  validate(
    need(isTruthy(base.sf),
         "Error: Overlay cannot be performed with selected base geometry") %then%
      need(inherits(base.sf, "sf"),
           "An error occurred during the overlay; please reload the GUI and try again")
  )

  #--------------------------------------------------------
  # Ensure sfc object class is polygon or multipolygon for faster plotting
  if (!inherits(st_geometry(base.sf), c("sfc_POLYGON", "sfc_MULTIPOLYGON"))) {
    base.sf2 <- suppressWarnings(
      st_cast(base.sf, "MULTIPOLYGON", warn = FALSE)
    )

    abund1 <- eSDM::model_abundance(base.sf, "Pred")
    abund2 <- eSDM::model_abundance(base.sf2, "Pred")
    if (nrow(base.sf) == nrow(base.sf2) & unname(abund1 == abund2)) {
      base.sf <- base.sf2
    }
    rm(base.sf2)
  }

  #--------------------------------------------------------
  base.sf
})


###############################################################################
# Clip base and erasing poly by study area poly to minimize later computation

### Clip erasing polygon by study area using st_crop()
# st_crop is used rather than st_intersection to avoid tiny slices of coastline
overlay_clip_land_bound <- reactive({
  overlay.land  <- overlay_proj_land()
  overlay.bound <- overlay_proj_bound()

  temp <- suppressMessages(st_intersects(overlay.bound, overlay.land))
  validate(
    need(length(temp[[1]]) > 0,
         paste("Error: No part of the erasing polygon",
               "is within the study area polygon"))
  )
  rm(temp)


  land.bound.try <- try(
    suppressMessages(st_crop(overlay.land, st_bbox(overlay.bound))),
    silent = TRUE
  )

  if (isTruthy(land.bound.try)) {
    land.bound.try

  } else {
    land.bound.try <- try(
      suppressMessages(
        st_crop(overlay_valid_land(), st_bbox(overlay_valid_bound()))
      ),
      silent = TRUE
    )
    if (isTruthy(land.bound.try)) {
      land.bound.try
    } else {
      overlay.land
    }
  }
})

### Clip base by study area using st_intersection()
overlay_clip_base_bound <- reactive({
  base.sf <- overlay_proj_base()
  overlay.bound <- overlay_proj_bound()

  base.bound.try <- try(
    suppressMessages(st_intersection(base.sf, overlay.bound)), silent = TRUE
  )

  if (!isTruthy(base.bound.try)) {
    base.bound.try <- try(
      suppressMessages(
        st_intersection(overlay_valid_base(), overlay_valid_bound())
      ), silent = TRUE
    )

    validate(
      need(isTruthy(base.bound.try),
           paste("Error: Unable to clip base geometry to study area polygon.",
                 "Please try a different study area polygon"))
    )
  }

  st_set_agr(base.bound.try, "constant")
})


###############################################################################
# Ensure base, erasing, and study area polys are valid

### Ensure base spdf is valid
overlay_valid_base <- reactive({
  base.sf <- overlay_proj_base()

  if (!all(st_is_valid(base.sf))) {
    base.sf <- check_valid(base.sf)
    validate(
      need(isTruthy(base.sf),
           paste("Error: The GUI was unable to make the base valid",
                 "without compromising the area and predicted abundance.",
                 "Please ensure that your imported SDMs all have valid",
                 "geometries with no self-intersections and with",
                 "all polygons having at least four vertices"))
    )
  }

  base.sf
})

### Ensure study area polygon is valid
overlay_valid_bound <- reactive({
  overlay.bound <- overlay_proj_bound()

  if (!all(st_is_valid(overlay.bound))) {
    overlay.bound <- check_valid(overlay.bound)
    validate(
      need(isTruthy(overlay.bound),
           paste("Error: The GUI was unable to make the study area polygon",
                 "valid without compromising the area of the polygon.",
                 "Please ensure that your loaded study area polygon is valid",
                 "with no self-intersections and at least four vertices"))
    )
  }

  overlay.bound
})

### Ensure erasing polygon is valid
overlay_valid_land <- reactive({
  overlay.land <- overlay_proj_land()

  if (!all(st_is_valid(overlay.land))) {
    overlay.land <- check_valid(overlay.land)
    validate(
      need(isTruthy(overlay.land),
           paste("Error: The GUI was unable to make the erasing polygon valid",
                 "without compromising the area of the polygon",
                 "Please use the provided erasing polygon or",
                 "ensure that your imported erasing polygon is valid",
                 "with no self-intersections and with",
                 "all polygons having at least four vertices"))
    )
  }

  overlay.land
})


###############################################################################
# Project base, study area poly, and erasing polys if necessary

### Output base predictions in specified projection
overlay_proj_base <- reactive({
  base.ll   <- overlay_base_pre()[[1]]
  base.orig <- overlay_base_pre()[[2]]

  # Return base with desired crs/projection
  if (overlay_crs() == crs.ll) {
    base.ll
  } else {
    # Use base.orig so base obj has as few transformations as possible
    st_transform(base.orig, overlay_crs())
  }
})

### Output study area polygon in specified projection
overlay_proj_bound <- reactive({
  overlay.bound <- st_transform(vals$overlay.bound, overlay_crs())
  # overlay.bound <- vals$overlay.bound
  #
  # if (!identical(overlay_crs(), crs.ll)) {
  #   overlay.bound <- st_transform(overlay.bound, overlay_crs())
  # }

  temp <- suppressMessages(st_intersects(overlay.bound, overlay_proj_base()))
  validate(
    need(length(overlay.bound) == 1,
         "Error in base creation: the study area poly is not unioned") %then%
      need(length(temp[[1]]) != 0,
           paste("Error: The study area polygon and specified base geometry",
                 "do not intersect"))
  )

  overlay.bound
})

### Output erasing polygon in specified projection
overlay_proj_land <- reactive({
  overlay.land <- st_transform(overlay_clip_land_llpre(), overlay_crs())
  # overlay.land <- overlay_clip_land_llpre()
  #
  # if (!identical(overlay_crs(), crs.ll)) {
  #   overlay.land <- st_transform(overlay.land, overlay_crs())
  # }

  temp <- suppressMessages(st_intersects(overlay.land, overlay_proj_base()))
  validate(
    need(length(overlay.land) == 1,
         "Error in base creation: the erasing poly is not unioned") %then%
      need(length(temp[[1]]) != 0,
           paste("Error: The erasing polygon and specified base geometry",
                 "do not intersect"))
  )

  overlay.land
})


###############################################################################
# Prep functions; outputs used in multiple higher-level reactive functions

### Crop erasing poly to base or study area extent (plus 5 degrees) in crs.ll
# ...so that projecting erasing poly doesn't take as long
overlay_clip_land_llpre <- reactive({
  overlay.land  <- vals$overlay.land
  overlay.bound <- vals$overlay.bound

  if (!is.null(overlay.bound)) {
    overlay.land.clipll <- suppressMessages(
      try(st_crop(overlay.land,
                  st_bbox(suppressWarnings(st_buffer(overlay.bound, 5)))),
          silent = TRUE)
    )

    validate(
      need(overlay.land.clipll,
           "Error in base creation: Issue with erasing poly prep") %then%
        need(length(overlay.land.clipll) > 0,
             paste("Error: The erasing polygon and study area polygon",
                   "do not intersect"))
    )

  } else {
    base.ll <- overlay_base_pre()[[1]]
    overlay.land.clipll <- suppressMessages(
      try(st_crop(overlay.land,
                  st_bbox(suppressWarnings(st_buffer(base.ll, 5)))),
          silent = TRUE)
    )

    validate(
      need(overlay.land.clipll,
           "Error in base creation: Issue with erasing poly prep") %then%
        need(length(overlay.land.clipll) > 0,
             paste("Error: The erasing polygon and specified base geometry",
                   "do not intersect"))
    )
  }

  st_union(overlay.land.clipll)
})

### Return a list with the crs.ll and orig crs versions of the base
overlay_base_pre <- reactive({
  base.idx <- overlay_base_idx()

  validate(
    need(length(base.idx) == 1,
         paste("Error: Please select exactly one set of predictions from the table",
               "to use as the base geometry"))
  )

  list(vals$models.ll[[base.idx]], vals$models.orig[[base.idx]])
})


###############################################################################
