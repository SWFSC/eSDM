###############################################################################
###############################################################################
# Read in a GIS shapefile from a fileInput output in a Shiny app
# From https://github.com/leonawicz/nwtapp/blob/master/mod_shpPoly.R

read.shp.shiny <- function(file.in.list) {
  infiles <- file.in.list$datapath

  # Check that .csv file was not uploaded
  if (length(infiles) == 1 & grepl(".csv", infiles[1])) {
    gis.file <- NA
  } else {
    dir <- unique(dirname(infiles))
    outfiles <- file.path(dir, file.in.list$name)
    purrr::walk2(infiles, outfiles, ~file.rename(.x, .y))

    gis.file <- try(
      st_read(dir, strsplit(file.in.list$name[1], "\\.")[[1]][1], quiet = TRUE),
      silent = TRUE
    )
  }

  if (inherits(gis.file, "sf")) {
    gis.file
  } else {
    try(stopifnot(inherits(gis.file, "sf")), silent = TRUE)
  }
}


###############################################################################
###############################################################################
# Create sfc object from data frame (from csv) with only long and lat,
#   respectively, as columns. crs set as crs.prov
pts2poly_vertices_shiny <- function(x, crs.prov, progress.detail) {
  obj.sfc <- try(eSDM::pts2poly_vertices(x, crs = crs.prov), silent = TRUE)

  validate(
    need(inherits(obj.sfc, "sfc"),
         paste("Error: The polygon could not be created",
               "from the points in the provided .csv file.",
               "Please ensure that the .csv file has the longitude points",
               "in the first column, the latitude points in the second",
               "column, and that the provided points form a closed",
               "and valid polygon"))
  )

  obj.sfc <- check_dateline(obj.sfc, 60, progress.detail)
  check_valid(obj.sfc, progress.detail)
}


###############################################################################
###############################################################################
# check_ functions: run on imported objects to ensure correct formatting

#------------------------------------------------------------------------------
### Check that x is sf object and has valid crs, then
###   return crs.ll and orig proj version of file
check_gis_crs <- function(x) {
  validate(
    need(inherits(x, "sf"),
         "Error: GIS object was not read in properly") %then%
      need(st_crs(x)$proj4string,
           "Error: GIS object does not have defined projection")
  )

  list(st_transform(x, crs.ll), x)
}


#------------------------------------------------------------------------------
### Adjust sf object from 0 - 360 range to -180 to 180 range
### Really should be check_antimeridian()
check_dateline <- function(x, wrap.offset = 10, progress.detail = FALSE) {
  stopifnot(
    inherits(x, c("sf", "sfc")),
    is.numeric(wrap.offset),
    inherits(progress.detail, "logical")
  )

  if (progress.detail) {
    on.exit(incProgress(0, detail = ""))
    incProgress(0, detail = "Checking if object spans the antimeridian")
  }

  x.orig <- x
  x.crs.orig <- st_crs(x.orig)
  dateline.flag <- FALSE
  validate(
    need(x.crs.orig$proj4string,
         "Error: The object does not have a defined coordinate system")
  )

  if (!st_is_longlat(x.orig)) x <- st_transform(x, crs.ll)

  if (st_bbox(x)[3] > 180) {
    dateline.flag <- TRUE
    if (progress.detail) {
      incProgress(0, detail = "Object spans the antimeridian; processing now")
    }
    x <- suppressWarnings(st_wrap_dateline(
      x, c("WRAPDATELINE=YES", paste0("DATELINEOFFSET=", wrap.offset))
    ))
  }

  ext <- st_bbox(x)
  validate(
    need(ext["xmax"] <= 180 && ext["xmin"] >= -180,
         paste("Error: The GUI was unable to properly process this object;",
               "please manually ensure that the longitude range of the",
               "object is the equivalent of [-180, 180] decimal degrees",
               "and then re-import the object into the GUI")),
    need(ext["ymax"] <= 90 && ext["ymin"] >= -90,
         paste("Error: The GUI was unable to process this object;",
               "please manually ensure that the latitude range of the",
               "object is the equivalent of [-90, 90] decimal degrees",
               "and then re-import the object into the GUI"))
  )

  if (dateline.flag){
    st_transform(x, x.crs.orig)
  } else {
    x.orig
  }
}


#------------------------------------------------------------------------------
### Check that x's geometry is valid
check_valid <- function(x, progress.detail = FALSE) {
  stopifnot(
    inherits(x, c("sf", "sfc")),
    is.logical(progress.detail)
  )

  if (progress.detail) {
    on.exit(incProgress(0, detail = ""))
    incProgress(0, detail = "Checking if the object's geometry is valid")
  }

  x.valid <- st_is_valid(x, reason = TRUE)

  if (!isTruthy(all(x.valid == "Valid Geometry"))) { #isTruthy() is for NA cases
    if (progress.detail) {
      incProgress(0, detail = "Making the object's geometry valid")
    }
    x.message <- x.valid[x.valid != "Valid Geometry"]

    make_geom_valid(x, message.invalid = x.message)
  } else {
    x
  }
}


#------------------------------------------------------------------------------
### Check that prediction, uncertainty, and weight data is in proper format
check_pred_var_weight <- function(x, pred.idx, var.idx, weight.idx,
                                  pred.na.idx, var.na.idx, weight.na.idx) {
  stopifnot(is.numeric(pred.idx))

  x.orig <- x
  if (inherits(x, "sf")) x <- st_set_geometry(x, NULL)

  if (!inherits(pred.na.idx, "logical")) x[pred.na.idx, pred.idx] <- NA
  if (!inherits(var.na.idx, "logical")) x[var.na.idx, var.idx] <- NA
  if (!inherits(weight.na.idx, "logical")) x[weight.na.idx, weight.idx] <- NA

  validate(
    need(is.numeric(x[, pred.idx]),
         paste("Error: Unable to process the prediction data, please",
               "ensure all values in the prediction column are numbers")),
    if (!is.na(var.idx)) {
      need(is.numeric(x[, var.idx]),
           paste("Error: Unable to process the uncertainty values, please",
                 "ensure all values in the uncertainty column are numbers"))
    },
    if (!is.na(weight.idx)) {
      need(is.numeric(x[, weight.idx]),
           paste("Error: Unable to process the weight data, please",
                 "ensure all values in the weight column are numbers"))
    }
  )

  x.orig
}


###############################################################################
###############################################################################
# Attempt to make an invalid geometry (geom.invalid) valid
# Perform checks to see if area/predicted abundance were changed much
# Called only by check_valid()

make_geom_valid <- function(geom.invalid, dens.col = NA, geom.info = NA,
                            message.invalid = NA) {
  #----------------------------------------------------------------------------
  geom.maybe <- st_make_valid(geom.invalid)

  check1 <- !all(st_is_valid(geom.maybe))
  if (inherits(geom.invalid, "sf")) {
    check2 <- !identical(
      class(st_geometry(geom.maybe)), class(st_geometry(geom.invalid))
    )
  } else {
    check2 <- !identical(class(geom.maybe), class(geom.invalid))
  }

  #----------------------------------------------------------------------------
  if (check1 || check2) {
    alert1 <- ifelse(
      is.na(geom.info),
      "The geometry of the object currently being processed is invalid.",
      paste("The geometry of", geom.info, "is invalid.")
    )
    if (!is.na(message.invalid)) {
      alert1 <- paste(
        alert1, "The error output was:<br>", message.invalid
      )
    }

    alert2 <- paste(
      "The GUI was unable to make the geometry valid using the",
      "st_make_valid() function from the sf package (see",
      tags$a("the function documentation",
             href = "https://r-spatial.github.io/sf/reference/valid.html"),
      "for more details).",
      "You may attempt to still use this object in the GUI,",
      "particularly if the invalid region will be clipped later,",
      "but this is NOT recommended as the",
      "invalid geometry likely will cause errors in the GUI."
    )

    showModal(modalDialog(
      title = paste("Important message - object geometry is invalid and",
                    "the GUI was unable to make it valid"),
      HTML(paste0(alert1, "<br><br>", alert2))
    ))

    geom.invalid

    #--------------------------------------------------------------------------
  } else {
    # Get area difference
    area1 <- as.numeric(sum(st_area(geom.invalid)))
    area.dif <- abs(as.numeric(sum(st_area(geom.maybe))) - area1)

    area.dif.char <- sprintf(as.character(round(area.dif / 1e+06, 4)), "%3")
    if (identical(area.dif.char, "0")) area.dif.char <- "0.000"

    area.dif.perc.char <- sprintf(
      as.character(round((area.dif / area1) * 100, 4)), "%3"
    )
    if (identical(area.dif.perc.char, "0")) area.dif.perc.char <- "0.000"

    # Get predicted abundance difference
    if (!is.na(dens.col)) {
      abund1 <- eSDM::model_abundance(geom.invalid, dens.col)
      abund.dif <- eSDM::model_abundance(geom.maybe, dens.col) - abund1

      abund.dif.char <- sprintf(as.character(round(abund.dif / 1e+06, 4)), "%3")
      if (identical(abund.dif.char, "0")) abund.dif.char <- "0.000"

      abund.dif.perc.char <- sprintf(
        as.character(round((abund.dif / abund1) * 100, 4)), "%3"
      )
      if (identical(abund.dif.perc.char, "0")) abund.dif.perc.char <- "0.000"
    }


    ###################################
    # Generate text to be displayed in modal
    alert1 <- ifelse(
      is.na(geom.info),
      "The geometry of the object currently being processed was invalid.",
      paste("The geometry of", geom.info, "was invalid.")
    )
    if (!anyNA(message.invalid)) {
      alert1 <- paste(
        alert1, "The error output was:<br>",
        paste0(
          "<span style=\"color: red;\">",
          paste(message.invalid, collapse = "; "),
          "</span>"
        )
      )
    }

    alert2 <- paste(
      "The GUI made the geometry valid using the st_make_valid() function",
      "from the sf package (see",
      tags$a("the function documentation",
             href = "https://r-spatial.github.io/lwgeom/reference/valid.html"),
      "for more details).",
      "You may safely continue using this object in the GUI as long as",
      "you are comfortable with the change in area reported below.",
      "You can use the preview functionality or export this geometry to",
      "ensure that no unexpected changes to the geometry occurred.",
      "See Appendix 3 of the manual for more information."
    )

    alert3 <- paste(
      "The difference between the area of the valid geometry and",
      "the area of the original geometry is",
      area.dif.char,
      "square km, which is",
      area.dif.perc.char,
      "percent different than the area of the original geometry."
    )

    if (!is.na(dens.col)) {
      alert4 <- paste(
        "The difference between the predicted abundance of the valid SDM",
        "and the predicted abundance of the original SDM is",
        abund.dif.char,
        "animals, which is",
        abund.dif.perc.char,
        "percent different than the predicted abundance of the original SDM."
      )
    } else {
      alert4 <- NULL
    }

    showModal(modalDialog(
      title = "Important message - object geometry was invalid but made valid",
      HTML(paste0(
        alert1, "<br><br>", alert2, "<br><br>", alert3, "<br><br>", alert4
      ))
    ))

    geom.maybe
  }
}


###############################################################################
###############################################################################
# GUI-specific helper functions

# ### Sort x by col1 and then (if applicable) col2
# data_sort <- function(x, col1 = 1, col2 = NA) {
#   if (!is.na(col2)) x <- x[order(x[, col2]), ]
#   x[order(x[, col1]), ]
# }


### Get the area of a sf or sfc object in km^2; return a numeric vector
esdm_area_km2 <- function(x) as.numeric(units::set_units(st_area(x), "km^2"))


### Determine whether all values in x are equal;
# From Hadley on stack overflow, simplified version of scales::zero_range()
zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}


### Get last n element(s) from string x
# From https://stackoverflow.com/questions/7963898
substr_right <- function(x, n) substr(x, nchar(x) - n + 1, nchar(x))


### Determine which elements of the x are one of invalid
# Invalid elements are "N/A", "n/a", "na", "NaN", or ""
na_which <- function(x) {
  na.char <- c("N/A", "n/a", "na", "NaN", "")

  na.idx <- suppressWarnings(
    c(which(is.na(x)), which(is.nan(x)), which(x %in% na.char), which(x < 0))
  )

  if (length(na.idx) == 0) NA else sort(unique(na.idx))
}


### Generate message reporting number of NA (prediction) values in x
na_message_pred <- function(x) {
  if (anyNA(x)) {
    "No prediction values were classified as NA"
  } else {
    len.x <- length(x)
    ifelse(len.x == 1,
           paste(len.x, "prediction value was classified as NA"),
           paste(len.x, "prediction values were classified as NA"))
  }
}


### Generate message reporting number of NA values in x and
###   number of non-NA y (pred) values with NA x values.
###   z is either "uncertainty" or "weight"
na_message <- function(x, y, z) {
  len.x <- length(x)
  if (anyNA(x)) {
    paste("No", z, "values were classified as NA")

  } else if (!all(x %in% y)) {
    paste0(
      ifelse(len.x == 1,
             paste(len.x, z, "value was classified as NA"),
             paste(len.x, z, "values were classified as NA")),
      "<br/>Some non-NA prediction values have NA ", z, " values"
    )

  } else {
    paste0(
      ifelse(len.x == 1,
             paste(len.x, z, "value was classified as NA"),
             paste(len.x, z, "values were classified as NA")),
      "<br/>No non-NA prediction values have NA ", z, " values"
    )
  }
}


# ### Generate message reporting number of NA (uncertainty) values in x and
# ###   number of non-NA y (pred) values with NA x (uncertainty) values
# na_var_message <- function(x, y, z) {
#   len.x <- length(x)
#   if (anyNA(x)) {
#     "No uncertainty values were classified as NA"
#
#   } else if (!all(x %in% y)) {
#     paste0(
#       ifelse(len.x == 1,
#              paste(len.x, "uncertainty value was classified as NA"),
#              paste(len.x, "uncertainty values were classified as NA")),
#       "<br/>Some non-NA prediction values have NA uncertainty values"
#     )
#
#   } else {
#     paste0(
#       ifelse(len.x == 1,
#              paste(len.x, "uncertainty value was classified as NA"),
#              paste(len.x, "uncertainty values were classified as NA")),
#       "<br/>No non-NA prediction values have NA uncertainty values"
#     )
#   }
# }
#
#
# ### Generate message reporting number of NA (weight) values in x and
# ###   number of non-NA y (pred) values with NA x (weight) values
# na_weight_message <- function(x, y) {
#   len.x <- length(x)
#   if (anyNA(x)) {
#     "No weight values were classified as NA"
#
#   } else if (!all(x %in% y)) {
#     paste0(
#       ifelse(len.x == 1,
#              paste(len.x, "weight value was classified as NA"),
#              paste(len.x, "weight values were classified as NA")),
#       "<br/>Some non-NA prediction values have NA weight values"
#     )
#
#   } else {
#     paste0(
#       ifelse(len.x == 1,
#              paste(len.x, "weight value was classified as NA"),
#              paste(len.x, "weight values were classified as NA")),
#       "<br/>No non-NA prediction values have NA weight values"
#     )
#   }
# }


### Round 'x' to nearest 'base' value
mround <- function(x, base, floor.use = FALSE, ceiling.use = FALSE) {
  if (floor.use) {
    base * floor(x / base)

  } else if (ceiling.use) {
    base * ceiling(x / base)

  } else {
    base * round(x / base)
  }
}

### Capitalize first letter of first element of string
esdm_simple_cap <- function(x, all = FALSE) {
  if (all) {
    s <- strsplit(x, " ")[[1]]
    paste0(toupper(substring(s, 1,1)), substring(s, 2), collapse = " ")
  } else {
    paste0(toupper(substring(x, 1,1)), substring(x, 2), collapse = " ")
  }
}

### Parse string wiht numbers in it, e.g. "3, 1/3, 4.5"
esdm_parse_num <- function(x) {
  temp <- try(unname(
    vapply(strsplit(x, ",")[[1]], function(i) eval(parse(text = i)), 0.1)
  ), silent = TRUE)

  if (isTruthy(temp)) temp else NA
}

###############################################################################
