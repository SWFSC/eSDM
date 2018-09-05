#' Overlay SDM predictions onto base geometry
#'
#' Overlay specfied SDM predictions that meet the percent overlap threshold requirement onto base geometry
#'
#' @param base.geom object of class \code{sfc}; base geometry
#' @param sdm object of class \code{sf}; original SDM predictions
#' @param sdm.idx names or indices of column(s) with data to be overlaid
#' @param overlap.perc numeric; percent overlap threshold,
#'   i.e. percentage of each base geometry polygon must overlap with SDM polygons
#'   for overlaid density value to be calculated and not set as NA
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr quo
#' @importFrom dplyr select
#' @importFrom purrr set_names
#' @importFrom sf st_area
#' @importFrom sf st_bbox
#' @importFrom sf st_crop
#' @importFrom sf st_crs
#' @importFrom sf st_intersection
#' @importFrom sf st_geometry
#' @importFrom sf st_sf
#' @importFrom sf st_set_geometry
#' @importFrom sf st_set_agr
#' @importFrom rlang .data
#' @importFrom rlang sym
#'
#' @details See *paper reference* for details about the overlay process
#'
#' @return Object of class \code{sf} with the geometry of \code{base.geom} and
#'   the data in the \code{sdm.idx} columns of \code{sdm} overlaid onto that geometry
#'
#' @examples
#' overlay_sdm(sf::st_geometry(preds.1), preds.2, 1, 50)
#' overlay_sdm(sf::st_geometry(preds.2), preds.1, c("Density", "Density2"), 50)
#'
#' @export
overlay_sdm <- function(base.geom, sdm, sdm.idx, overlap.perc) {
  #----------------------------------------------------------------------------
  # 0) Check that inputs meet requirements
  if (!inherits(base.geom, "sfc")) {
    stop("'base.geom' object must be of class 'sfc'")
  }
  if (!inherits(sdm, "sf")) stop("'sdm' object must be of class 'sf'")
  if (!(is.numeric(overlap.perc) & 0 <= overlap.perc & overlap.perc <= 100)) {
    stop("'overlap.perc' object must be a number greater than 0 and ",
         "less than or equal to 100")
  }
  stopifnot(
    st_crs(base.geom) == st_crs(sdm),
    all(sdm.idx %in% names(sdm)) | inherits(sdm.idx, "numeric")
  )
  if (identical(base.geom, st_geometry(sdm))) {
    warning("'base.geom' and 'sdm' have the same geometry and thus ",
            "you shouldn't need to use the full overlay procedure")
  }

  base.area.m2 <- st_area(base.geom)
  if (!all(units(base.area.m2)$numerator == c("m", "m"))) {
    stop("Units of st_area(base.geom) must be m^2")
  }
  sdm.area.m2 <- st_area(sdm)
  if (!all(units(sdm.area.m2)$numerator == c("m", "m"))) {
    stop("Units of st_area(sdm.area.m2) must be m^2")
  }

  #----------------------------------------------------------------------------
  # 1) Get intersection of sdm (sdm being overlaid) and base.geom (base)
  #   after turning base.geom into an sf object with an index variable;
  #   this var gets passed along during st_intersection() and can be used as
  #   int-to-base key
  base.geom.sf <- st_sf(
    base.idx = 1:length(base.geom), base.geom, agr = "constant"
  )

  if (inherits(sdm.idx, "numeric")) sdm.idx <- names(sdm)[sdm.idx]

  sdm <- sdm %>%
    select(sdm.idx) %>%
    filter(!is.na(!!sym(sdm.idx[1]))) %>%
    st_set_agr("constant")
  sdm <- st_set_agr(suppressMessages(st_crop(sdm, st_bbox(base.geom))), "constant")
  # ^ not tidied so that suppressMessages() can be used
  # ^^ Will throw a waring if st_agr(sdm) != "constant" for all provided data

  int <- try(suppressMessages(st_intersection(sdm, base.geom.sf)), silent = TRUE)

  if (inherits(int, "try-error")) {
    stop("Unable to successfully run 'st_intersection(sdm, base.geom)'; ",
         "make sure that base.geom and sdm are both ",
         "valid objects of class sfc and sf, respectively")
  }

  int <- int[as.numeric(st_area(int)) > 1, ]
  if (nrow(int) == 0) {
    stop("No 'base.geom' and 'sdm' polygons with non-na sdm.idx[1] values ",
         "overlap")
  }
  stopifnot(all(!is.na(st_set_geometry(sdm, NULL)[, 1])))

  #----------------------------------------------------------------------------
  # 2) Get predicted abundances for base grid cells that had overlap
  # TODO: should be some way to do this in a tidy fashion, but I'm not sure how
  #   while being able to handle as many data column names as necessary
  int.df <- st_set_geometry(int, NULL) %>%
    mutate(int.area.km = as.numeric(st_area(int)) / 1e+06)

  new.abund.list <- lapply(sdm.idx, function(i){
    by(int.df[, c(i, "int.area.km")], int.df$base.idx,
       function(j) sum(j[, 1] * j[, 2])
    )
  })
  base.idx.nona <- as.numeric(names(new.abund.list[[1]]))
  stopifnot(identical(as.numeric(unique(int.df$base.idx)), base.idx.nona))
  # ^ unique(int.df$base.idx) output is of class integer

  new.abund.df <- as.data.frame(lapply(new.abund.list, as.numeric)) %>%
    set_names(paste0(sdm.idx, ".overlaid"))


  #----------------------------------------------------------------------------
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


  #----------------------------------------------------------------------------
  # 4) Convert abundances to densities
  stopifnot(nrow(new.abund.df) == length(int.area.by.base.km))
  new.dens.df <- new.abund.df / int.area.by.base.km
  rm(new.abund.df, int.area.by.base.km)


  #----------------------------------------------------------------------------
  # 5) Determine which base polys had no overlap with sdm and thus
  # need to be added with NAs for dens value.
  # Add them to abund.df and sort abund.df by base poly order
  base.len <- 1:length(base.geom)
  base.idx.na <- base.len[!(base.len %in% base.idx.nona)]

  if (length(base.idx.na) > 0) {
    new.dens.df.1 <- data.frame(idx = base.idx.nona, new.dens.df)
    new.dens.df.2 <- as.data.frame(
      matrix(NA, nrow = length(base.idx.na), ncol = ncol(new.dens.df.1))
    )
    new.dens.df.2[, 1] <- base.idx.na
    names(new.dens.df.2) <- names(new.dens.df.1)

    new.dens.df <- rbind(new.dens.df.1, new.dens.df.2) %>%
      arrange(.data$idx) %>%
      select(-.data$idx)
    row.names(new.dens.df) <- 1:nrow(new.dens.df)
  }
  # else nothing to do


  #----------------------------------------------------------------------------
  # 6) Put base grid together with predicted densities to make overlaid SDM
  stopifnot(
    nrow(new.dens.df) == nrow(base.geom),
    inherits(new.dens.df, "data.frame")
  )

  st_sf(new.dens.df, geometry = base.geom, agr = "constant")
}
