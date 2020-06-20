#' Overlay SDM predictions onto base geometry
#'
#' Overlay specified SDM predictions that meet the percent overlap threshold requirement onto base geometry
#'
#' @param base.geom object of class \code{sfc}; base geometry
#' @param sdm object of class \code{sf}; original SDM predictions
#' @param sdm.idx names or indices of column(s) with data to be overlaid
#' @param overlap.perc numeric; percent overlap threshold,
#'   i.e. percentage of each base geometry polygon must overlap with SDM
#'   prediction polygons for overlaid density value to be calculated and
#'   not set as NA
#'
#' @details See the eSDM GUI manual for specifics about the overlay process.
#'   This process is equivalent to areal interpolation (Goodchild and Lam 1980),
#'   where \code{base.geom} is the target, \code{sdm} is the source, and the data
#'   specified by \code{sdm.idx} are spatially intensive.
#'
#'   Note that \code{overlay_sdm} removes rows in \code{sdm} that have NA values
#'   in the first column specified in \code{sdm.idx} (i.e. \code{sdm.idx[1]}),
#'   before the overlay.
#'   Thus, for valid overlay results, all columns of \code{sdm} specified in
#'   \code{sdm.idx} must either have NA values in the same rows
#'   or contain only NAs.
#'
#' @return Object of class \code{sf} with the geometry of \code{base.geom} and
#'   the data in the \code{sdm.idx} columns of \code{sdm} overlaid onto that
#'   geometry. Note that this means all columns of \code{sdm} not in
#'   \code{sdm.idx} will not be in the returned object.
#'   Because the data are considered spatially intensive, the \code{agr}
#'   attribute will be set as 'constant' for all columns in the returned object
#'
#' @references Goodchild, M.F. & Lam, N.S.-N. (1980) Areal interpolation:
#'   a variant of the traditional spatial problem. Geo-Processing, 1, 297-312.
#'
#' @examples
#' overlay_sdm(sf::st_geometry(preds.1), preds.2, 1, 50)
#' overlay_sdm(sf::st_geometry(preds.2), preds.1, "Density", 50)
#'
#' @export
overlay_sdm <- function(base.geom, sdm, sdm.idx, overlap.perc) {
  #----------------------------------------------------------------------------
  # 0) Check that inputs meet requirements

  #--------------------------------------------------------
  ### Formats
  if (!inherits(base.geom, "sfc")) {
    stop("'base.geom' must be of class 'sfc'")
  }
  if (!inherits(sdm, "sf")) stop("'sdm' must be of class 'sf'")
  if (!(is.numeric(overlap.perc) & between(overlap.perc, 0, 100))) {
    stop("'overlap.perc' must be a number between 0 and 100 (inclusive)")
  }

  stopifnot(
    st_crs(base.geom) == st_crs(sdm),
    all(sdm.idx %in% names(sdm)) | is.numeric(sdm.idx)
  )

  base.area.m2 <- st_area(base.geom)
  if (!all(units(base.area.m2)$numerator == c("m", "m"))) {
    stop("Units of st_area(base.geom) must be m^2")
  }
  sdm.area.m2 <- st_area(sdm)
  if (!all(units(sdm.area.m2)$numerator == c("m", "m"))) {
    stop("Units of st_area(sdm.area.m2) must be m^2")
  }

  #--------------------------------------------------------
  ### Other input checks and some processing
  # Throw warning if base.geom and geometry of sdm are identical
  if (identical(base.geom, st_geometry(sdm))) {
    warning("'base.geom' and 'sdm' have the same geometry and thus ",
            "you shouldn't need to use the full overlay procedure",
            immediate. = TRUE)
  }

  # If sdm.idx is numeric, get column names and check that they don't start
  #   with a number
  #   Columns that start with a number get renamed as 'X...' by base R funcs
  if (inherits(sdm.idx, "numeric") | inherits(sdm.idx, "integer")) {
    sdm.idx <- names(sdm)[sdm.idx]
  }
  if (any(substr(sdm.idx, 1, 1) %in% as.character(0:9))) {
    col.idx <- which(substr(sdm.idx, 1, 1) %in% as.character(0:9))
    stop("The columns specified in 'sdm.idx' cannot begin with a number. ",
         "Please rename the following columns in 'sdm':\n",
         paste0("'", paste(names(sdm)[col.idx], collapse = "' ; '"), "'"))
  }

  # Throw warning if not all columns in sdm.idx either have NAs in the same
  #   rows of sdm.idx[1] or contain only NAs (i.e. unused weight column in GUI)
  if (length(sdm.idx) > 1) {
    sdm.df <- st_set_geometry(sdm, NULL)[, sdm.idx]
    #^ singe bracket ok because of length() check in if()
    sdm.na <- map(sdm.df, function(i) which(is.na(i)))
    sdm.temp <- vapply(
      sdm.na, function(i, j, k) {
        identical(i, j) | (length(i) == k)
      }, as.logical(1), j = sdm.na[[1]], k = nrow(sdm.df)
    )
    if (any(!sdm.temp)) {
      warning("The following columns have 'NA' prediction values for different ",
              "prediction polygons than the first column specified ",
              "in 'sdm.idx', and thus their overlaid values will be invalid ",
              "(see the function documentation for more details):\n",
              paste(names(which(!sdm.temp)), collapse = "; "))
    }
    rm(sdm.df, sdm.na, sdm.temp)
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

  int <- int %>%
    rename(geometry = !!attr(int, "sf_column")) %>%
    mutate(area_m = as.numeric(st_area(.data$geometry))) %>%
    filter(.data$area_m > 1) %>%
    select(-.data$area_m)

  if (nrow(int) == 0) {
    stop("No 'base.geom' polygons overlap with any 'sdm' polygons")
  }
  stopifnot(all(!is.na(st_set_geometry(sdm, NULL)[[1]])))

  #----------------------------------------------------------------------------
  # 2) Get predicted densities for base polys that had any overlap
  # Do this by summing the predicted abundance for each base poly (i.e. an
  #   area-weighted average of the abundance) and then converting to density
  # TODO: make the for loop tidy with complete()/expand()..?
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
      set_names(c(head(names(new.dens.df), -1), i))

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
    base.int.perc <- round(new.dens.df$area_km_sum / base.area.km[base.idx.nona], 2)
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

  } else {
    new.dens.df <- new.dens.df %>% select(-.data$base_idx)
  }


  #----------------------------------------------------------------------------
  # 5) Create sf obj w/predicted densities and base geom to make overlaid SDM
  stopifnot(
    nrow(new.dens.df) == nrow(base.geom),
    inherits(new.dens.df, "data.frame")
  )

  st_sf(new.dens.df, geometry = base.geom, agr = "constant")
}
