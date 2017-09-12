### ensOverlay_OverlayModels_overlay_func
## Function to overlay models on base polygons
# By Sam Woodman


###############################################################################

overlay.func <- function(pol.base, pol.spdf, overlap.perc)
{
  # print("Beginning overlay of model")
  
  pol.sp <- as(pol.spdf, "SpatialPolygons")
  
  over.base <- over(pol.base, pol.sp, returnList = TRUE)
  over.base.idx <- sapply(over.base, unname)
  
  res.df <- lapply(seq_along(over.base.idx), function(i) { #EAB onto JVR 85s
    curr.idx <- over.base.idx[[i]]
    if(length(curr.idx) != 0) {
      temp <- intersect(pol.base[i,], pol.spdf[curr.idx,])
      if(!is.null(temp)) temp
      else NA
    } 
    else NA
  })

  
  #########################################################
  ### Remove poly info if pol.spdf did not overlap at least overlap.perc of it
  # print("Overlaying data")
  
  area.res <- sapply(res.df, function(j) { # JVR base 10s ## Efficiency???
    if(class(j) != "logical") area(j)
    else NA
  })
  area.res.sum <- sapply(area.res, sum)

  area.base <- area(pol.base)
  area.res.base.ratio <- round(area.res.sum / area.base, 5) 
  # 5 makes area.which.na == area.which.diff for overlap.perc = 1
  
  area.which.na <- which(area.res.base.ratio < overlap.perc)
  if(length(area.which.na) > 0) res.df[area.which.na] <- NA
  
  
  #########################################################
  ### Overlay data calculations
  area.res.km <- sapply(area.res, function(i) {i / 1e+06})
  
  overlaid.data <- mapply(function(r, a.rea) {
    if(class(r) != "logical") {
      c(sum(a.rea * r$Pred),                # Abundance
        sum(a.rea * r$Error) / sum(a.rea),  # Error
        sum(a.rea * r$Weight) / sum(a.rea)) # Weights for pixels
    }
    else c(NA, NA, NA)
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
  
  
  # return(list(spdf.overlaid, abund.sum)) # other things?
  
  return(spdf.overlaid)
}

###############################################################################