#' Title
#'
#' Details
#'
#' @export

dateline.process <- function(data.all, lat.idx, lon.idx) {
  # sp.crs.ll <- sp::CRS("+init=epsg:4326")
  browser()
  dateline.line <- Line(coords = cbind(c(180, 180), c(90, -90)))
  dateline <- SpatialLines(list(Lines(list(dateline.line), ID = "l1")))

  proj4string(dateline) <- sp.crs.ll
  dateline.poly <- suppressWarnings(gBuffer(dateline, width = 1e-8))
  # warning is because of lat/long coords; ok because width is so small

  sp.orig <- data.all
  lat.name <- names(sp.orig)[lat.idx]
  lon.name <- names(sp.orig)[lon.idx]

  which.g180 <- which(sp.orig[,lon.name] > 180)
  sp.orig[, lon.name] <- ifelse(sp.orig[,lon.name] > 180, sp.orig[, lon.name] - 360, sp.orig[, lon.name])

  # l.180
  sp1 <- sp.orig[-which.g180, ]
  coordinates(sp1) <- c(lon.name, lat.name)
  proj4string(sp1) <- crs.ll
  spixdf1 <- as(sp1, "SpatialPixelsDataFrame")
  spdf1 <- as(spixdf1, "SpatialPolygonsDataFrame")
  rm(sp1, spixdf1)

  # g.180
  sp2 <- sp.orig[which.g180, ]
  coordinates(sp2) <- c(lon.name, lat.name)
  proj4string(sp2) <- crs.ll
  spixdf2 <- as(sp2, "SpatialPixelsDataFrame")
  spdf2 <- as(spixdf2, "SpatialPolygonsDataFrame")
  rm(sp2, spixdf2)

  # gArea(spdf1) + gArea(spdf2)

  # Split polys that overlap with 180 deg
  spdf1.split <- erase(spdf1, dateline.poly)
  spdf1.fix <- spdf1.split[sapply(spdf1.split@polygons, function(i) bbox(i)[1, 2] > 180), ]

  spdf1.fix.disag <- disaggregate(spdf1.fix)
  l <- length(spdf1.fix.disag)
  toadd.l180 <- spdf1.fix.disag[seq(1, l, by = 2), ]
  tofix.g180 <- spdf1.fix.disag[seq(2, l, by = 2), ]
  # gArea(toadd.l180); gArea(tofix.g180)
  # tofix.g180@polygons[[109]]@Polygons[[1]]@area

  # Fix corrdinates > 180
  n.coords <- coordinates(tofix.g180)
  n.coords[, 1] <- n.coords[, 1] - 360
  n.pix <- SpatialPixelsDataFrame(n.coords, tofix.g180@data)
  proj4string(n.pix) <- crs.ll
  n.pix.width <- suppressWarnings(gArea(tofix.g180) / length(tofix.g180) / n.pix@grid@cellsize[2])
  n.pix@grid@cellsize[1] <- n.pix.width
  n.spdf <- as(n.pix, "SpatialPolygonsDataFrame")

  # Put spdfs together
  l.180 <- rbind(spdf1[is.na(over(spdf1, dateline.poly)), ], toadd.l180)
  g.180 <- rbind(spdf2, n.spdf)

  # gArea(l.180) + gArea(g.180)

  spdf.new <- rbind(l.180, g.180)
  # gArea(spdf.new)

  return(spdf.new)
}


#' Title
#'
#' Determine the resolution of provided gis model
#'
#' @export

gis.res.calc <- function(sf.ll, sf.orig) {
  validate(
    need(identical(class(sf.ll), c("sf", "data.frame")) &
           identical(class(sf.orig), c("sf", "data.frame")),
         "Error: gis.res.calc(): inputs must be sf objects"),
    need(identical(st_crs(sf.ll), crs.ll),
         "Error: gis.res.calc(): first input must have crs = crs.ll")
  )

  ### Get extents of individual polys in original projection and units if apl
  crs.orig <- st_crs(sf.orig)$proj4string
  crs.orig.m  <- grepl("+units=m", crs.orig)
  crs.orig.ll <- grepl("+proj=longlat", crs.orig)

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

  ## Return appropriate object
  if (is.na(res.orig) & !is.na(res.ll)) {
    return(res.ll)
  } else if (!is.na(res.orig) & is.na(res.ll)) {
    return(res.orig)
  } else {
    return("Unk")
  }
}
