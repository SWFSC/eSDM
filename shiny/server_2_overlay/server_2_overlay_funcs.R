### Functions used in Overlay tab




###############################################################################
### Check that provided SPoly has a valid crs, and return crs.ll version

overlay.gis.crs <- function(gis.loaded) {
  validate(
    need(class(gis.loaded)[1] == "SpatialPolygons", 
         "Error: Object passed to overlay.gis.crs() is not a SPoly")
  )
  
  crs.curr <- crs(gis.loaded)
  validate(
    need(!is.na(crs.curr), "Error: GIS file does not have defined projection")
  )
  
  if (identical(crs.curr, crs.ll)) { 
    gis.loaded
  } else {
    spTransform(gis.loaded, crs.ll)
  }
  
  return(gis.loaded)
}


###############################################################################
### Attempt to make an invalid polygon (poly.invalid) valid

valid.poly <- function(poly.invalid, description = NULL) {
  # print(paste("Validating", description))
  poly.maybe <- suppressWarnings(gBuffer(poly.invalid, byid = TRUE, width = 0))
  # Warnings suppressed for if poly.invalid has crs.ll
  
  # Is gBuffer method didn't work, then try clgeo_Clean()
  if (!(suppressWarnings(gIsValid(poly.maybe)) | 
        (sum(area(poly.maybe)) - sum(area(poly.invalid))) < 1e+06)) {
    # print("Using clgeo_Clean() on polygon", description)
    poly.maybe <- cleangeo::clgeo_Clean(poly.invalid)
  }
  
  # Is gBuffer method didn't work, then try clgeo_Clean()
  validate(
    need(suppressWarnings(gIsValid(poly.maybe)), 
         paste("Error: Could not make polygon", description, "valid")),
    need((sum(area(poly.maybe)) - sum(area(poly.invalid))) < 1e+06, 
         paste("Error: While being made valid, the area of the polygon", 
               description, "changed by more than 1 square kilometer")
    )
  )
  
  poly.maybe
}


###############################################################################
### Function to overlay models onto base polygons
# pol.base is base polygon
# pol.spdf is preds being overlaid
# overlap.perc is the percentage of each base polygon that must be...
# ...overlapped by pol.spdf poly(s) for the new density to not be NA

overlay.func <- function(pol.base, pol.spdf, overlap.perc) {
  #########################################################
  ### Prep and get intersecting overlap
  pol.sp <- as(pol.spdf, "SpatialPolygons")

  over.base <- over(pol.base, pol.sp, returnList = TRUE)
  over.base.idx <- sapply(over.base, unname)
  
  res.df <- lapply(seq_along(over.base.idx), function(i) {
    curr.idx <- over.base.idx[[i]]
    if (length(curr.idx) != 0) {
      temp <- intersect(pol.base[i,], pol.spdf[curr.idx,])
      if (!is.null(temp)) temp
      else NA
    } 
    else NA
  })
  
  
  #########################################################
  ### Remove poly info if pol.spdf did not overlap at least overlap.perc of it
  area.res <- sapply(res.df, function(j) {
    if (class(j) != "logical") {
      area(j)
    } else {
      NA
    }
  })
  area.res.sum <- sapply(area.res, sum)
  
  area.base <- area(pol.base)
  area.res.base.ratio <- round(area.res.sum / area.base, 5) 
  # 5 makes area.which.na == area.which.diff for overlap.perc = 1
  
  area.which.na <- which(area.res.base.ratio < overlap.perc)
  if (length(area.which.na) > 0) res.df[area.which.na] <- NA
  
  
  #########################################################
  ### Overlay data calculations
  area.res.km <- sapply(area.res, function(i) {i / 1e+06})
  
  overlaid.data <- mapply(function(r, a.rea) {
    if (class(r) != "logical") {
      c(sum(a.rea * r$Pred),                # Abundance
        sum(a.rea * r$Error) / sum(a.rea),  # Error
        sum(a.rea * r$Weight) / sum(a.rea)) # Weights for pixels
    } else {
      c(NA, NA, NA)
    }
  }, res.df, area.res.km)
  
  overlaid.data.df <- as.data.frame(t(overlaid.data))
  names(overlaid.data.df) <- c("Abund", "Error.overlaid", "Weight.overlaid")
  
  abund.sum <- sum(overlaid.data.df$Abund, na.rm = TRUE)
  
  
  #########################################################
  ### Create and return new spdf
  overlaid.data.df$Pixel <- 1:nrow(overlaid.data.df)
  area.base.km <- area.base / 1e+06
  overlaid.data.df[,1] <- overlaid.data.df$Abund / area.base.km
  names(overlaid.data.df)[1] <- "Pred.overlaid"
  
  spdf.overlaid <- SpatialPolygonsDataFrame(pol.base, overlaid.data.df, 
                                            match.ID = FALSE)
  
  return(spdf.overlaid)
}

###############################################################################