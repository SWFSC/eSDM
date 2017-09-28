### Most non-reactive functions for Ensemble Shiny App
### Some functions are in tab-specific '..._func.R' files


###############################################################################
# For General Use

### Clip shp by extent of bb (plus buf)
# Adapted from http://robinlovelace.net/r/2014/07/29/clipping-with-r.html
gClipExtent <- function(shp, bb, buf = NULL) {
  validate(
    need(identicalCRS(shp, bb), "gClipExtent(): CRS arguments are not equal")
  )
  if (class(bb) == "matrix") {
    b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  } else {
    b.ext <- extent(bb)
    if (!is.null(buf)) {
      b.ext <- extent(b.ext@xmin - buf, b.ext@xmax + buf, 
                      b.ext@ymin - buf, b.ext@ymax + buf)
    }
    b_poly <- as(b.ext, "SpatialPolygons")
  }
  proj4string(b_poly) <- crs(bb)
  
  gIntersection(shp, b_poly, byid = TRUE)
}


### Determine whether all values in x are equal, aka have a zero range
# From Hadley Wickham
zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}


### Get last n element from string x
# From https://stackoverflow.com/questions/7963898
substrRight <- function(x, n){
  substr(x, nchar(x) - n + 1, nchar(x))
}


### Determine which elements of the vector data.vec are 
# one of "N/A", "n/a", "na", "NaN", or ""
na.which <- function(data.vec) {
  na.char <- c("N/A", "n/a", "na", "NaN", "")
  
  na.idx <- suppressWarnings(c(which(is.na(data.vec)),
                               which(is.nan(data.vec)),
                               which(data.vec %in% na.char),
                               which(data.vec < 0)))
  na.idx <- sort(unique(na.idx))
  if (length(na.idx) == 0) na.idx <- NA
  
  return(na.idx)
}

### Generate message reporting length of na.which.out
# This message was built to refer to prediction values
na.which.message <- function(na.which.out) {
  x <- na.which.out
  
  if (anyNA(x)) {
    na.len <- "No prediction values were classified as NA"
  } else {
    len.x <- length(x)
    na.len <- ifelse(len.x == 1, 
                     paste(len.x, "prediction value was classified as NA"), 
                     paste(len.x, "prediction values were classified as NA"))
  }
}

###############################################################################
# For plotting

### Round 'x' to nearest 'base' value
mround <- function(x, base){ 
  base * round(x / base) 
} 


### Calculate brak points for density intervals
#   Breaks at top 2%, 5%, 10%, 15%, 20%, 25%, 30%, 35%, 40%
breaks.calc <- function(sp.data) {
  breaks <- rev(c(0.02, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40))
  # if (any(is.na(sp.data))) warning("NA's removed")
  # if (any(sp.data == 0, na.rm = TRUE)) warning("Densities contain 0's")
  # if (any(sp.data < 0, na.rm = TRUE)) warning("Densities contain values < 0")
  
  sp.data <- sp.data[!is.na(sp.data)] 
  
  data.len <- length(sp.data)
  data.max <- max(sp.data)
  data.min <- min(sp.data)
  
  sp.data.sort <- sort(sp.data, decreasing = TRUE)
  data.breaks.mid <- sapply(breaks, function(i) {
    sp.data.sort[ceiling(i * data.len)]
  })
  data.breaks <- c(data.min, data.breaks.mid, data.max)
  
  return(data.breaks)
}

###############################################################################
# Functions for spdfs with prediction data

### Normalize vector of model predictions, 'x'
normalize <- function(x) {
  num <- (x - min(x, na.rm = TRUE))
  denom <- (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  
  return (num / denom)
}


### Sort data by one or two specified column(s)
#   column.1 is primary sort and column.2 is secondary sort
#   column.2 is sorted first (if necessary), then column.1
data.sort <- function(data.in, column.1 = 1, column.2 = NA) {
  data.sort <- data.in
  if (!is.na(column.2)) data.sort <- data.sort[order(data.sort[, column.2]), ]
  data.sort <- data.sort[order(data.sort[, column.1]), ]
  
  return(data.sort)
}


### Rescale spdf.orig columns cols.data to data in new.abundances
models.rescale <- function(spdf.list, abund.new) {
  spdf.list.rescaled <- lapply(spdf.list, function(s) {
    abund.orig <- model.abundance(s, cols.data = "Pred.overlaid")
    frac <- abund.orig / abund.new
    s$Pred.overlaid <- s$Pred.overlaid / frac
    
    s
  })
  
  return(spdf.list.rescaled)
}


### Calculate abundances for each of cols.data
# Assumes that all cols.data have NAs at same place
# Abundance depends on crs code of provided spdf
model.abundance <- function(spdf, cols.data = "Pred") {
  # Remove NAs, assumes NAs are consistent across cols.data
  spdf.nona <- spdf[-which(is.na(spdf@data[,cols.data[1]])), ]
  if (length(spdf.nona) == 0) spdf.nona <- spdf
  
  # Calculate areas of polygons with no NAs
  spdf.area <- raster::area(spdf.nona) / 1e+06
  abunds <- sapply(cols.data, function(j) sum(spdf.nona@data[, j] * spdf.area))
  
  return(abunds)
}


###############################################################################
##### Other

### Load csv file from given shiny file input
read.csv.in <- function(file.in) {
  req(file.in)
  
  list.out <- list(file.in$name, 
                   read.csv(file.in$datapath, stringsAsFactors = FALSE))
  
  return(list.out)
}

### Load GIS shapefile from given shiny file input, 'file.in'
# Used for loading shapefiles in Load Model Preds and Overlay sections
# From https://github.com/leonawicz/nwtapp/blob/master/mod_shpPoly.R
read.shp.in <- function(file.in) {
  infiles <- file.in$datapath
  dir <- unique(dirname(infiles))
  outfiles <- file.path(dir, file.in$name)
  purrr::walk2(infiles, outfiles, ~file.rename(.x, .y))
  
  gis.file <- try(readOGR(dir, strsplit(file.in$name[1], "\\.")[[1]][1], 
                          verbose = FALSE), 
                  silent = TRUE)
  
  return(gis.file)
}


### Sort by lat and then long; return crs.ll and orig proj version of file
#   Requires that 'gis.loaded' is an SPolyDF or SPtsDF
gis.model.check <- function(gis.loaded) {
  validate(
    need(class(gis.loaded)[1] %in% c("SpatialPolygonsDataFrame", 
                                     "SpatialPointsDataFrame"), 
         "Object passed to gis.model.check() is not a SPolyDF")
  )
  
  # Sort spdf by lat and then long so polygons are ordered bottom up
  coords <- data.frame(idx = seq_along(gis.loaded), coordinates(gis.loaded))
  idx.sorted <- data.sort(coords, 3, 2)[, 1] # Lat is primary sort
  gis.loaded <- gis.loaded[idx.sorted, ]
  
  # Check crs arguments and project to crs.ll if necessary
  crs.curr <- crs(gis.loaded)

  validate(
    need(!is.na(crs.curr), "Error: GIS file does not have defined projection")
  )
  
  if (identical(crs.curr, crs.ll)) { 
    list(gis.loaded, gis.loaded)
  } else {
    list(spTransform(gis.loaded, crs.ll), gis.loaded)
  }
}
###############################################################################
