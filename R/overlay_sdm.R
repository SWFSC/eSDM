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
#' @importFrom dplyr between
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr set_names
#' @importFrom rlang .data
#' @importFrom rlang sym
#' @importFrom sf st_area
#' @importFrom sf st_bbox
#' @importFrom sf st_crop
#' @importFrom sf st_crs
#' @importFrom sf st_intersection
#' @importFrom sf st_geometry
#' @importFrom sf st_sf
#' @importFrom sf st_set_geometry
#' @importFrom sf st_set_agr
#' @importFrom units set_units
#' @importFrom utils head

#' @details See the eSDM GUI manual for specifics about the overlay process
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
    stop("'base.geom' must be of class 'sfc'")
  }
  if (!inherits(sdm, "sf")) stop("'sdm' must be of class 'sf'")
  if (!(is.numeric(overlap.perc) & between(overlap.perc, 0, 100))) {
    stop("'overlap.perc' must be a number between (inclusive) 0 and 100")
  }

  stopifnot(
    st_crs(base.geom) == st_crs(sdm),
    all(sdm.idx %in% names(sdm)) | inherits(sdm.idx, "numeric") |
      inherits(sdm.idx, "integer")
  )
  if (identical(base.geom, st_geometry(sdm))) {
    warning("'base.geom' and 'sdm' have the same geometry and thus ",
            "you shouldn't need to use the full overlay procedure")
  }

  # If sdm.idx is numeric, get column names and check that they don't start
  #   with a number. Such columns get renamed as 'X...' by base R funcs
  if (inherits(sdm.idx, "numeric") | inherits(sdm.idx, "integer")) {
    sdm.idx <- names(sdm)[sdm.idx]
  }
  if (any(substr(sdm.idx, 1, 1) %in% as.character(0:9))) {
    col.idx <- which(substr(sdm.idx, 1, 1) %in% as.character(0:9))
    stop("The columns specified in 'sdm.idx' cannot begin with a number. ",
         "Please rename the following columns in 'sdm': ",
         paste0("'", paste(names(sdm)[col.idx], collapse = "' ; '"), "'"))
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
    base_idx = 1:length(base.geom), geometry = base.geom, agr = "constant"
  )

  sdm <- sdm %>%
    select(sdm.idx) %>%
    filter(!is.na(!!sym(sdm.idx[1]))) %>%
    st_set_agr("constant")
  sdm <- suppressMessages(st_crop(sdm, st_bbox(base.geom))) %>%
    st_set_agr("constant")
  # ^ separate so that suppressMessages() can be used
  # ^^ Will throw a waring if st_agr(sdm) != "constant" for all provided data

  int <- try(
    suppressMessages(st_intersection(sdm, base.geom.sf)), silent = TRUE
  )

  if (inherits(int, "try-error")) {
    stop("Unable to successfully run 'st_intersection(sdm, base.geom)'; ",
         "make sure that base.geom and sdm are valid objects of ",
         "class sfc and sf, respectively")
  }

  int <- int[as.numeric(st_area(int)) > 1, ]
  if (nrow(int) == 0) {
    stop("No 'base.geom' polygons overlap with any 'sdm' polygons")
  }
  stopifnot(all(!is.na(st_set_geometry(sdm, NULL)[, 1])))

  #----------------------------------------------------------------------------
  # 2) Get predicted densities for base polys that had any overlap
  # Do this by summing the predicted abundance for each base poly (i.e. an
  #   area-weighted average of the abundance) and then converting to density
  int.df <- int %>%
    mutate(int_area_km = as.numeric(set_units(st_area(.data$geometry), "km^2"))) %>%
    st_set_geometry(NULL)

  for(i in sdm.idx) {
    if (exists("new.dens.df")) {
      temp <- int.df %>%
        select(.data$base_idx, .data$int_area_km, curr_dens = !!i) %>%
        mutate(curr_abund = .data$int_area_km * .data$curr_dens) %>%
        group_by(.data$base_idx) %>%
        summarise(curr_dens = sum(.data$curr_abund) / sum(.data$int_area_km))

      new.dens.df <- new.dens.df %>%
        left_join(temp, by = "base_idx")
      rm(temp)

    } else {
      new.dens.df <- int.df %>%
        select(.data$base_idx, .data$int_area_km, curr_dens = !!i) %>%
        mutate(curr_abund = .data$int_area_km * .data$curr_dens) %>%
        group_by(.data$base_idx) %>%
        summarise(area_km_sum = sum(.data$int_area_km),
                  curr_dens = sum(.data$curr_abund) / .data$area_km_sum)
    }

    new.dens.df <- new.dens.df %>%
      set_names(c(head(names(new.dens.df), -1), paste0(i, ".overlaid")))

  }; rm(i)

  base.idx.nona <- new.dens.df$base_idx
  base.idx.na <- base.geom.sf$base_idx[!(base.geom.sf$base_idx %in% base.idx.nona)]
  stopifnot(
    length(base.geom) == length(base.idx.nona) + length(base.idx.na),
    all(base.geom.sf$base_idx %in% c(base.idx.nona, base.idx.na))
  )


  #----------------------------------------------------------------------------
  # 3) Set density values of base polys that don't meet overlap.perc as NA
  if (length(base.idx.nona) == 0) {
    stop("There was an error determining overlap percentage")

  } else {
    base.area.km <- as.numeric(set_units(base.area.m2, "km^2"))
    base.int.perc <- new.dens.df$area_km_sum / base.area.km[base.idx.nona]
    new.dens.df <- new.dens.df %>% select(-.data$area_km_sum)
    new.dens.df[base.int.perc < (overlap.perc / 100), 2:ncol(new.dens.df)] <- NA
  }


  #----------------------------------------------------------------------------
  # 4) Determine which base polys had no overlap with sdm and thus
  # need to be added with density values of NA
  base.len <- 1:length(base.geom)
  base.idx.na <- base.len[!(base.len %in% base.idx.nona)]

  if (length(base.idx.na) > 0) {
    new.dens.df.na <- as.data.frame(
      matrix(NA, nrow = length(base.idx.na), ncol = ncol(new.dens.df))
    ) %>%
      mutate(V1 = base.idx.na) %>%
      set_names(names(new.dens.df))

    new.dens.df <- new.dens.df %>%
      bind_rows(new.dens.df.na) %>%
      arrange(.data$base_idx) %>%
      select(-.data$base_idx)
  }
  # else nothing to do


  #----------------------------------------------------------------------------
  # 5) Create sf obj w/predicted densities and base geom to make overlaid SDM
  stopifnot(
    nrow(new.dens.df) == nrow(base.geom),
    inherits(new.dens.df, "data.frame")
  )

  st_sf(new.dens.df, geometry = base.geom, agr = "constant")
}
