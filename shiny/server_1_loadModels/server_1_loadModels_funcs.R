### Non-reactive functions used in ensLoadModel and its subjects
### Area calculation code in lines 92 - 107 is based primarily on raster::area()


###############################################################################
### Take in a csv file data of polygon centers with range 0 to 360 deg
# Return SPolyDF with range -180 to 180 deg
dateline.process <- function(data.all, lat.idx, lon.idx) {
  dateline.line <- Line(coords = cbind(c(180, 180), c(90, -90)))
  dateline <- SpatialLines(list(Lines(list(dateline.line), ID = "l1")))
  
  proj4string(dateline) <- crs.ll
  dateline.poly <- suppressWarnings(gBuffer(dateline, width = 1e-8))
  # warning is because of lat/long coords; ok because width is so small
  
  sp.orig <- data.all
  lat.name <- names(sp.orig)[lat.idx]
  lon.name <- names(sp.orig)[lon.idx]
  
  which.g180 <- which(sp.orig[,lon.name] > 180)
  sp.orig[,lon.name] <- ifelse(sp.orig[,lon.name] > 180, sp.orig[,lon.name] - 360, sp.orig[,lon.name])
  
  # l.180
  sp1 <- sp.orig[-which.g180,]
  coordinates(sp1) <- c(lon.name,lat.name)
  proj4string(sp1) <- crs.ll
  spixdf1 <- as(sp1, "SpatialPixelsDataFrame")
  spdf1 <- as(spixdf1, "SpatialPolygonsDataFrame")
  rm(sp1, spixdf1)
  
  # g.180
  sp2 <- sp.orig[which.g180,]
  coordinates(sp2) <- c(lon.name,lat.name)
  proj4string(sp2) <- crs.ll
  spixdf2 <- as(sp2, "SpatialPixelsDataFrame")
  spdf2 <- as(spixdf2, "SpatialPolygonsDataFrame")
  rm(sp2, spixdf2)
  
  # gArea(spdf1) + gArea(spdf2)
  
  # Split polys that overlap with 180 deg
  spdf1.split <- erase(spdf1, dateline.poly)
  spdf1.fix <- spdf1.split[sapply(spdf1.split@polygons, function(i) bbox(i)[1,2] > 180),]
  
  spdf1.fix.disag <- disaggregate(spdf1.fix)
  l <- length(spdf1.fix.disag)
  toadd.l180 <- spdf1.fix.disag[seq(1, l, by = 2),]
  tofix.g180 <- spdf1.fix.disag[seq(2, l, by = 2),]
  # gArea(toadd.l180); gArea(tofix.g180)
  # tofix.g180@polygons[[109]]@Polygons[[1]]@area
  
  # Fix corrdinates > 180
  n.coords <- coordinates(tofix.g180)
  n.coords[,1] <- n.coords[,1] - 360
  n.pix <- SpatialPixelsDataFrame(n.coords, tofix.g180@data)
  proj4string(n.pix) <- crs.ll
  n.pix.width <- suppressWarnings(gArea(tofix.g180) / length(tofix.g180) / n.pix@grid@cellsize[2])
  n.pix@grid@cellsize[1] <- n.pix.width
  n.spdf <- as(n.pix, "SpatialPolygonsDataFrame")
  
  # Put spdfs together
  l.180 <- rbind(spdf1[is.na(over(spdf1, dateline.poly)),], toadd.l180)
  g.180 <- rbind(spdf2, n.spdf) 
  
  # gArea(l.180) + gArea(g.180)
  
  spdf.new <- rbind(l.180, g.180)
  # gArea(spdf.new) 
  
  return(spdf.new)
}


###############################################################################
### Create a SPixDF object from a SPolyDF through rasterizing
# rasterize takes the value of polygon that overlaps center of raster cell
gis.rasterize.poly <- function(spdf.poly.ll) {
  r <- raster(spdf.poly.ll, nrow = 60, ncol = 60) # up for discussion
  model.raster <- rasterize(spdf.poly.ll, r, field = "Pred")
  
  spdf.pix <- as(model.raster, "SpatialPixelsDataFrame")
  names(spdf.pix) <- "Pred"
  
  spdf.pix
}

###############################################################################
### Determine the resolution of provided gis model
#     Thus, also determine whether model was made lat/long or ea regular
gis.res.calc <- function(spdf.ll, spdf.orig) {
  validate( #Is validate() within non-reactive func acceptable?
    need(length(spdf.ll) == length(spdf.orig), 
         "Error: gis.res.calc(): model lengths are different"),
    need(crs(spdf.ll)@projargs == crs.ll@projargs,
         "Error: gis.res.calc(): first input must have crs = crs.ll")
  )
  
  ### Get areas of individual polygons in original projection and units
  #Basic code structure from raster::area()
  spdf.orig.polys <- spdf.orig@polygons
  spdf.orig.origarea <- vector(length = length(spdf.orig.polys))
  for (i in 1:length(spdf.orig.polys)) {
    parts <- length(spdf.orig.polys[[i]]@Polygons)
    sumarea <- 0
    for (j in 1:parts) {
      ar <- spdf.orig.polys[[i]]@Polygons[[j]]@area
      if (spdf.orig.polys[[i]]@Polygons[[j]]@hole) {
        sumarea <- sumarea - ar
      }
      else {
        sumarea <- sumarea + ar
      }
    }
    spdf.orig.origarea[i] <- sumarea
  }
  browser()
  
  ### Do checks
  # Check 1: Are at least 50% of the polygons the same area?
  spdf.orig.table.max <- tail(table(spdf.orig.origarea), 1)
  spdf.check.1 <- (spdf.orig.table.max / length(spdf.orig.origarea)) > 0.5
  
  # Check 2: Is (area rescaled to 0-1) ~ (lat/long latitude) less than 0.01
  spdf.lat.coords <- coordinates(spdf.ll)[, 2]
  spdf.orig.origarea.ratio <- spdf.orig.origarea / max(spdf.orig.origarea)
  spdf.orig.lm <- lm(spdf.orig.origarea.ratio ~ spdf.lat.coords)
  
  spdf.check.2 <- abs(spdf.orig.lm$coefficients[2]) < 0.001

  ### If applicable, calculate and return resultion
  proj.orig <- proj4string(spdf.orig)
  if (spdf.check.1 & spdf.check.2) {
    # Loaded polygons are regular: return resolution
    if (grepl("+proj=longlat", proj.orig)) {
      # Lat long coordinates/projection
      spdf.val <- round(sqrt(as.numeric(names(spdf.orig.table.max))), 4)
      spdf.res <- paste(spdf.val, "degrees")
    } else if (grepl("+units=m", proj.orig)) {
      # Equal area projection
      spdf.val <- round(sqrt(as.numeric(names(spdf.orig.table.max)) / 1e+06), 3)
      spdf.res <- paste(spdf.val, "km")
    } else {
      spdf.res <- "Unk"
    }
  } else {
    # Loaded polygons are irregular: return approx resolution if possible
    if (grepl("+proj=longlat", proj.orig)) {
      origarea.table <- table(spdf.orig.origarea)
      approx.max <- origarea.table[which.max(origarea.table)]
      
      # Lat long coordinates/projection
      spdf.val <- round(sqrt(as.numeric(names(approx.max))), 4)
      spdf.res <- paste0("~", spdf.val, " degrees")
    } else if (grepl("+units=m", proj.orig)) {
      # Equal area projection
      spdf.val <- round(sqrt(as.numeric(names(approx.max)) / 1e+06), 3)
      spdf.res <- paste0("~", spdf.val, " km")
    } else {
      spdf.res <- "~Unk"
    }
  }
  
  return(spdf.res)
}

# warning(paste("Loaded GIS polygon does not appear to be regular in its",
#               "provided geographic coordinates or projection. This will ", 
#               "NOT have any effect on the functionality of the app"))


###############################################################################
# gis.model.check() is in server_funcs+vals.R

###############################################################################