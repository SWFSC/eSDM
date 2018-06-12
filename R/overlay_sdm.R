#' Title
#'
#' Overlay models onto base polygons
#'
#' @importFrom purrr set_names
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
    st_crs(base.poly) == st_crs(sdm),
    all(data.names %in% names(sdm))
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
z <- enquo(data.names)

  #--------------------------------------------------------
  # 1) Get intersection of sdm (sdm being overlaid) and base.poly (base)
  #   after turning base.poly into an sf object with an index variable;
  #   this var gets passed along during st_intersection() and can be used as
  #   int-to-base key
  base.poly.sf <- st_sf(
    base.idx = 1:length(base.poly), base.poly, agr = "constant"
  )

  sdm <- sdm %>% dplyr::select(!!quo(data.names))
  sdm <- st_set_agr(suppressMessages(st_crop(sdm, st_bbox(base.poly))), "constant")
  # ^ not tidied so that suppressMessages() can be used

  int <- try(suppressMessages(st_intersection(sdm, base.poly.sf)))

  if (inherits(int, "try-error")) {
    stop("Unable to run 'st_intersection(sdm, base.poly)'; make sure that ",
         "'base.poly' and 'sdm' are both valid sfc and sf objects, ",
         "respectively")
  }
  if (length(int) == 0) stop("'sdm' and 'base.poly' do not overlap")
  int <- int[as.numeric(st_area(int)) > 1, ]
  # TODO Check if nrow(int) == 0 to see if base.poly and sdm are identical? Or use identical()?


  #########################################################
  # 2) Get predicted abundances for base grid cells that had overlap
  # TODO: should be some way to do this in a tidy fashion, but I'm not sure how
  #   while being able to handle as many data column names as necessary
  int.df <- st_set_geometry(int, NULL) %>%
    dplyr::mutate(int.area.km = as.numeric(st_area(int)) / 1e+06)
  int.df <- int.df[!is.na(int.df[, data.names[1]]), ]

  new.abund.list <- lapply(data.names, function(i){
    by(int.df[, c(i, "int.area.km")], int.df$base.idx,
       function(j) sum(j[, 1] * j[, 2])
    )
  })
  base.idx.nona <- as.numeric(names(new.abund.list[[1]]))
  stopifnot(identical(as.numeric(unique(int.df$base.idx)), base.idx.nona))
  # ^ unique(int.df$base.idx) output is of class integer

  new.abund.df <- as.data.frame(lapply(new.abund.list, as.numeric)) %>%
    set_names(paste0(data.names, ".overlaid"))
  # new.abund.df2 <- int.df %>%
  #   group_by(base.idx) %>%
  #   summarise(sum(int.area.km * .data[, |data.names|]))


  #########################################################
  # 3) Set base grid cells that don't meet overlap.perc as NA
  int.area.by.base.km <- as.numeric(with(int.df, by(int.area.km, base.idx, sum)))
  base.area.km <- as.numeric(base.area.m2) / 1e+06

  if (length(base.idx.nona) == 0) {
    stop("There was an error determining overlap percentage")

  } else {
    base.int.perc <- int.area.by.base.km / base.area.km[base.idx.nona]
    stopifnot(nrow(new.abund.df) == length(base.int.perc))
    new.abund.df[base.int.perc < (overlap.perc / 100), ] <- NA
  }


  #########################################################
  # 4) Convert abundances to densities
  stopifnot(nrow(new.abund.df) == length(int.area.by.base.km))
  new.dens.df <- new.abund.df / int.area.by.base.km
  # new.dens.df <- new.dens.df %>%
  #   dplyr::mutate(Pixel = 1:nrow(new.dens.df))
  # new.dens.df <- new.dens.df[, c(1, 3, 2, 4)]
  rm(new.abund.df, int.area.by.base.km)


  #########################################################
  # 5) Determine which base polys had no overlap with sdm and thus
  # need to be added with NAs for dens value.
  # Add them to abund.df and sort abund.df by base poly order
  base.len <- 1:length(base.poly)
  base.idx.na <- base.len[!(base.len %in% base.idx.nona)]

  if (length(base.idx.na) != 0) {
    new.dens.df.1 <- data.frame(idx = base.idx.nona, new.dens.df)
    new.dens.df.2 <- as.data.frame(
      matrix(NA, nrow = length(base.idx.na), ncol = ncol(new.dens.df.1))
    )
    new.dens.df.2[, 1] <- base.idx.na
    names(new.dens.df.2) <- names(new.dens.df.1)

    new.dens.df <- rbind(new.dens.df.1, new.dens.df.2)
    new.dens.df <- new.dens.df[order(new.dens.df$idx), ]
    row.names(new.dens.df) <- 1:nrow(new.dens.df)
    new.dens.df <- new.dens.df[, -1]
  }
  # else nothing to do


  #########################################################
  # 6) Put base grid together with predicted densities to make overlaid SDM
  stopifnot(nrow(new.dens.df) == nrow(base.poly))

  st_sf(new.dens.df, geometry = base.poly, agr = "constant")
}
