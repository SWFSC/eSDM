# Functions specific to 'Import predictions' section

###############################################################################
# Generate info message about NA values for renderUIs
model_NA_info_func <- function(pred.type, pred.na.idx,
                               var.idx, var.na.idx,
                               weight.idx, weight.na.idx) {
  temp <- ifelse(
    pred.type != 3, NA,
    paste("Abundance value type: All prediction values will be divided",
          "by their prediction polygon area")
  )

  #--------------------------------------------------------
  if (as.numeric(weight.idx) > 1 & as.numeric(var.idx) > 1) {
    if (is.na(temp)) {
      HTML(
        na_message_pred(pred.na.idx), "<br/>", "<br/>",
        na_message(var.na.idx, pred.na.idx, "uncertainty"), "<br/>", "<br/>",
        na_message(weight.na.idx, pred.na.idx, "weight")
      )
    } else {
      HTML(
        temp, "<br/>", "<br/>",
        na_message_pred(pred.na.idx), "<br/>", "<br/>",
        na_message(var.na.idx, pred.na.idx, "uncertainty"), "<br/>", "<br/>",
        na_message(weight.na.idx, pred.na.idx, "weight")
      )
    }

    #------------------------------------------------------
  } else if (as.numeric(weight.idx) > 1) {
    if (is.na(temp)) {
      HTML(
        na_message_pred(pred.na.idx), "<br/>", "<br/>",
        na_message(weight.na.idx, pred.na.idx, "weight")
      )
    } else {
      HTML(
        temp, "<br/>", "<br/>",
        na_message_pred(pred.na.idx), "<br/>", "<br/>",
        na_message(weight.na.idx, pred.na.idx, "weight")
      )
    }

    #------------------------------------------------------
  } else if (as.numeric(var.idx) > 1) {
    if (is.na(temp)) {
      HTML(
        na_message_pred(pred.na.idx), "<br/>", "<br/>",
        na_message(var.na.idx, pred.na.idx, "uncertainty")
      )
    } else {
      HTML(
        temp, "<br/>", "<br/>",
        na_message_pred(pred.na.idx), "<br/>", "<br/>",
        na_message(var.na.idx, pred.na.idx, "uncertainty")
      )
    }

    #------------------------------------------------------
  } else {
    if (is.na(temp)) {
      HTML(na_message_pred(pred.na.idx))
    } else {
      HTML(temp, "<br/>", "<br/>", na_message_pred(pred.na.idx))
    }
  }
}

###############################################################################
# Attempt to determine the resolution of provided GIS predictions
gis_res_calc <- function(sf.ll, sf.orig) {
  #----------------------------------------------------------------------------
  # Checks
  validate(
    need(inherits(sf.ll, "sf") & inherits(sf.orig, "sf"),
         "Error: gis.res.calc(): inputs must be sf objects"),
    need(identical(st_crs(sf.ll), crs.ll),
         "Error: gis.res.calc(): first input must have crs = crs.ll")
  )

  #----------------------------------------------------------------------------
  ### Get extents of individual polys in original projection and units if apl
  crs.orig <- st_crs(sf.orig)$proj4string
  crs.orig.m  <- grepl("+units=m", crs.orig)
  crs.orig.ll <- st_is_longlat(crs.orig)

  if ((crs.orig.m | crs.orig.ll) & (crs.orig.m != crs.orig.ll) &
      !identical(st_crs(sf.orig), crs.ll)) {
    res.orig <- sapply(list(sf.orig), function(sf.curr, div.val) {
      sf.bbox <- lapply(st_geometry(sf.curr), st_bbox)
      sf.lon.diff <- sapply(sf.bbox, function(i) round(i["xmax"] - i["xmin"], 3)) / div.val
      sf.lat.diff <- sapply(sf.bbox, function(j) round(j["ymax"] - j["ymin"], 3)) / div.val

      sf.table.lon <- table(sf.lon.diff)
      sf.check1 <- (max(sf.table.lon) / nrow(sf.curr)) > 0.8
      sf.lon.val <- as.numeric(names(sf.table.lon)[which.max(sf.table.lon)])

      sf.table.lat <- table(sf.lat.diff)
      sf.check2 <- (max(sf.table.lat) / nrow(sf.curr)) > 0.8
      sf.lat.val <- as.numeric(names(sf.table.lat)[which.max(sf.table.lat)])

      if (sf.check1 & sf.check2 & (sf.lon.val == sf.lat.val)) {
        paste(sf.lon.val, ifelse(crs.orig.m, "km", "degrees"))
      } else {
        NA
      }
    }, div.val = ifelse(crs.orig.m, 1e+03, 1))

  } else {
    res.orig <- NA
  }

  #----------------------------------------------------------------------------
  ### Get extents of individual polygons in original crs.ll (WGS84)
  res.ll <- sapply(list(sf.ll), function(sf.curr) {
    sf.bbox <- lapply(st_geometry(sf.curr), st_bbox)
    sf.lon.diff <- sapply(sf.bbox, function(i) round(i["xmax"] - i["xmin"], 3))
    sf.lat.diff <- sapply(sf.bbox, function(j) round(j["ymax"] - j["ymin"], 3))

    sf.table.lon <- table(sf.lon.diff)
    sf.check1 <- (max(sf.table.lon) / nrow(sf.curr)) > 0.8
    sf.lon.val <- as.numeric(names(sf.table.lon)[which.max(sf.table.lon)])

    sf.table.lat <- table(sf.lat.diff)
    sf.check2 <- (max(sf.table.lat) / nrow(sf.curr)) > 0.8
    sf.lat.val <- as.numeric(names(sf.table.lat)[which.max(sf.table.lat)])

    if (sf.check1 & sf.check2 & (sf.lon.val == sf.lat.val)) {
      paste(sf.lon.val, "degrees")
    } else {
      NA
    }
  })

  #----------------------------------------------------------------------------
  ### Return appropriate object
  if (is.na(res.orig) & !is.na(res.ll)) {
    res.ll

  } else if (!is.na(res.orig) & is.na(res.ll)) {
    res.orig

  } else {
    "Unk"
  }
}

###############################################################################
