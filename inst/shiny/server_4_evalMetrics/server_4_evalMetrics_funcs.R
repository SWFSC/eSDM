### Non-reactive functions for Evaluation Metrics tab

###############################################################################
# Process evaluation metric validation data

### Process data frame (x) with long, lat, and data column;
###   processing method depends on data type (y)
eval_proc_df <- function(x, y, p.codes, a.codes) {
  #----------------------------------------------------------------------------
  stopifnot(
    is.data.frame(x),
    ncol(x) == 3,
    y %in% c(1, 2)
  )

  if (y == 1) {
    #--------------------------------------------
    # Count data
    validate(
      need(is.numeric(x[, 3]) | is.integer(x[, 3]),
           paste("Error: Selected validation data column is not numeric.",
                 "Consider importing data as 'Presence/absence' data"))
    )

    x <- x %>%
      dplyr::rename(lon = 1, lat = 2, count = 3) %>%
      dplyr::mutate(sight = as.numeric(count > 0)) %>%
      dplyr::select(1, 2, 4, 3)

  } else {
    #--------------------------------------------
    # Presence/absence data
    x <- x %>%
      dplyr::rename(lon = 1, lat = 2, sight.temp = 3) %>%
      dplyr::mutate(count = NA)

    validate(
      need(!(is.null(p.codes) & is.null(a.codes)),
           paste("Error: Please select one or more",
                 "presence codes and absence codes")),
      need(all(!(p.codes %in% a.codes)),
           paste("Error: Please ensure that no presence and",
                 "absence codes are the same")),
      need(all(unique(x$sight.temp) %in% c(p.codes, a.codes)),
           paste("Error: Please ensure that all codes are classified",
                 "as either presence or absence codes"))
    )

    x <- x %>%
      dplyr::mutate(sight = ifelse(sight.temp %in% p.codes, 1, 0)) %>%
      dplyr::select(1, 2, 5, 4)
  }

  #----------------------------------------------------------------------------
  stopifnot(
    ncol(x) == 4,
    names(x) == c("lon", "lat", "sight", "count")
  )

  if (min(x$lon, na.rm = TRUE) > 180) x$lon <- x$lon - 360

  # Sort by lat (primary) then long for bottom up sort and then create sf obj
  pts <- x %>%
    dplyr::arrange(lat, lon) %>%
    st_as_sf(coords = c("lon", "lat"), crs = crs.ll, agr = "constant")

  # Perform checks
  validate(
    need(inherits(st_geometry(pts), "sfc_POINT"),
         "Error processing validation data")
  )

  # Don't need check_valid() for pts
  check_dateline(pts)
}


###############################################################################
# Generate message detailing the number of validation pts on polygon boundaries
eval_overlap_message <- function(models.toeval, eval.data) {
  pt.over.len <- sapply(
    lapply(models.toeval, function(m) {
      eval.data <- st_transform(eval.data, st_crs(m))
      which(sapply(suppressMessages(st_intersects(eval.data, m)), length) > 1)
    }),
    length
  )

  # Make text pretty
  #--------------------------------------------------------
  if (all(pt.over.len == 0)) {
    paste(
      "The predictions being evaluated had 0 validation points",
      "that fell on the boundary between two or more prediction polygons"
    )

    #------------------------------------------------------
  } else if (length(pt.over.len) == 1) {
    paste(
      "The predictions being evaluated had", pt.over.len, "validation points",
      "that fell on the boundary between two or more prediction polygons;" ,
      "the predictions from these polygons were averaged for the evaluation.",
      "See Appendix 2 of the manual for more details."
    )

    #------------------------------------------------------
  } else {
    if (zero_range(pt.over.len)) {
      temp <- paste(
        "The predictions being evaluated each had", unique(pt.over.len),
        "validation points"
      )

    } else if (length(pt.over.len) == 2) {
      temp <- paste(
        "The predictions being evaluated had",
        paste(pt.over.len, collapse = " and "),
        "validation points, respectively,"
      )

    } else {
      temp <- paste(
        "The predictions being evaluated had",
        paste0(paste(head(pt.over.len, -1), collapse = ", "), ","),
        "and", tail(pt.over.len, 1), "validation points, respectively,"
      )
    }

    paste(
      temp,
      "that fell on the boundary between two or more prediction polygons;",
      "the predictions from these polygons were averaged for the evaluation.",
      "See Appendix 2 of the manual for more details."
    )
  }
}


###############################################################################
