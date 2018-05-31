#' Title
#'
#' Overlay models onto base polygons
#'
#' @param base.poly object of class sfc that sdm is being overlaid onto
#' @param sdm object of class sf representing the SDM that is being overlaid onto base.poly
#' @param overlap.perc percentage that each base polygon must be overlapped for density value to be kept
#' @param data.names names or indices of column(s) with data to be overlaid
#'
#' @export

overlay_sdm <- function(base.poly, sdm, overlap.perc, data.names) {
  #--------------------------------------------------------
  # 0) Check that inputs meet requirements
  if (!inherits(base.poly, "sfc")) {
    stop("'base.poly' object must be of class 'sfc'")
  }
  if (!inherits(sdm, "sf")) stop("'sdm' object must be of class 'sf'")
  if (!(is.numeric(overlap.perc) & 0 <= overlap.perc & overlap.perc <= 100)) {
    stop("'overlap.perc' object must be a number greater than 0 and ",
         "less than or equal to 100")
  }
  stopifnot(
    st_crs(base.poly) == st_crs(sdm)
  )
  if (identical(base.poly, st_geometry(sdm))) {
    warning("'base.poly' and 'sdm' have the same geometry and thus ",
            "you shouldn't need to use the full overlay procedure")
  }
  base.area.m2 <- st_area(base.poly)
  if (!all(units(base.area.m2)$numerator == c("m", "m"))) {
    stop("Units of st_area(base.poly) must be m^2")
  }
  sdm.area.m2 <- st_area(sdm)
  if (!all(units(sdm.area.m2)$numerator == c("m", "m"))) {
    stop("Units of st_area(sdm.area.m2) must be m^2")
  }


  #--------------------------------------------------------
  # 1) Get intersection of sdm (sdm being overlaid) and base.poly (base)
  sdm <- sdm %>% dplyr::select(!!quo(data.names))
  # st_agr(sdm) <- "constant"
  sdm <- suppressMessages(st_crop(sdm, st_bbox(base.poly)))
  st_agr(sdm) <- "constant" #st_crop() removes agr info
  int <- try(suppressMessages(st_intersection(sdm, base.poly)))

  if (inherits(int, "try-error")) {
    stop("Unable to run 'st_intersection(sdm, base.poly)'; make sure that ",
         "'base.poly' and 'sdm' are both valid sfc and sf objects, ",
         "respectively")
  }
  if (length(int) == 0) stop("'sdm' and 'base.poly' do not overlap")
  int.area <- as.numeric(st_area(int))
  if (any(int.area == 0)) {
    int <- int[int.area > 1, ]
  }
  # TODO Check if nrow(int) == 0 to see if base.poly and sdm are identical? Or use identical()?


  #########################################################
  # 2) For each int polygon, determine which base polygon it is in
  # Do this by determining which base poly extent each int poly centroid is in
  # bbox idea from https://github.com/r-spatial/sf/issues/377
  # st_relate(int, base.poly, pattern = "2********") takes longer and gives
  # less useful result
  int.cent <- suppressWarnings(st_centroid(int))
  base.bbox.each <- lapply(base.poly, function(i) st_as_sfc(st_bbox(i)))
  base.bbox.each <- do.call("c", base.bbox.each)
  st_crs(base.bbox.each) <- st_crs(base.poly)

  int.baseidx <- suppressMessages(st_intersects(int.cent, base.bbox.each))
  if (!all(sapply(int.baseidx, length) == 1)) {
    stop("There was an error determining polygon overlap")
  }
  rm(int.cent, base.bbox.each)


  #########################################################
  # 3) Add (2) and area data to int
  int$int.area.km <- as.numeric(st_area(int)) / 1e+06
  int$base.which <- unlist(int.baseidx)


  #########################################################
  # 4) Get predicted abundances for base grid cells that had overlap and
  #   set abund as NA for cells that don't meet overlap.perc
  # TODO: should be some way to do this in a tidy fashion, but I'm not sure how
  #   while being able to handle as many data column names as necessary
  int.df <- st_set_geometry(int, NULL)
  new.abund.list <- lapply(data.names, function(i){
    by(int.df[, c(i, "int.area.km")], int.df$base.which,
       function(j) {
         stopifnot(ncol(j) == 2) # adds ~0.1 seconds
         sum(j[, 1] * j[, 2])
       })
  })
  base.which.nona <- as.numeric(names(new.abund.list[[1]]))
  new.abund.df <- as.data.frame(lapply(new.abund.list, as.numeric))
  names(new.abund.df) <- paste0(data.names, ".overlaid")
  # new.abund.df2 <- int.df %>%
  #   group_by(base.which) %>%
  #   summarise(sum(int.area.km * .data[, data.names]))


  #########################################################
  # 5) Set base grid cells that don't meet overlap.perc as NA
  stopifnot(identical(as.numeric(unique(int.df$base.which)), base.which.nona))
  # ^ unique(int.df$base.which) output is of class integer
  int.area.by.base.km <- as.numeric(with(int.df, by(int.area.km, base.which, sum)))
  base.area.km <- as.numeric(base.area.m2) / 1e+06

  if (length(base.which.nona) == 0) {
    stop("There was an error determining overlap percentage")
  } else {
    base.int.perc <- int.area.by.base.km / base.area.km[base.which.nona]
    stopifnot(nrow(new.abund.df) == length(base.int.perc))
    new.abund.df[base.int.perc < (overlap.perc / 100), ] <- NA
  }


  #########################################################
  # 6) Convert abundances to densities
  stopifnot(nrow(new.abund.df) == length(int.area.by.base.km))
  new.dens.df <- new.abund.df / int.area.by.base.km
  # new.dens.df <- new.dens.df %>%
  #   dplyr::mutate(Pixel = 1:nrow(new.dens.df))
  # new.dens.df <- new.dens.df[, c(1, 3, 2, 4)]


  #########################################################
  # 7) Determine which base polys had no overlap with sdm and thus
  # need to be added with NAs for dens value.
  # Add them to abund.df and sort abund.df by base poly order
  base.len <- 1:length(base.poly)
  base.which.na <- base.len[!(base.len %in% base.which.nona)]

  if (length(base.which.na) != 0) {
    new.dens.df.1 <- data.frame(idx = base.which.nona, new.dens.df)
    new.dens.df.2 <- as.data.frame(
      matrix(NA, nrow = length(base.which.na), ncol = ncol(new.dens.df.1))
    )
    new.dens.df.2[, 1] <- base.which.na
    names(new.dens.df.2) <- names(new.dens.df.1)

    new.dens.df <- rbind(new.dens.df.1, new.dens.df.2)
    new.dens.df <- new.dens.df[order(new.dens.df$idx), ]
    row.names(new.dens.df) <- 1:nrow(new.dens.df)
    new.dens.df <- new.dens.df[, -1]
  }
  # else nothing to do


  #########################################################
  # 8) Put base grid together with predicted densities to make overlaid SDM
  stopifnot(nrow(new.dens.df) == nrow(base.poly))
  return(st_sf(new.dens.df, geometry = base.poly, agr = "constant"))
}
