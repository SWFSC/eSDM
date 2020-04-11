### Code for importing predicitons for both .shp and .gdb inputs
### Called in create_sf_gis_shp() and create_sf_gis_gdb()

### req() checks
req(pred.idx <= (ncol(gis.file) - 1))
req((weight.idx - 1) <= (ncol(gis.file) - 1))

withProgress(message = prog.message, value = 0.3, {
  ### Check long extent, polygon validity, and generate crs.ll version if nec
  gis.file <- check_dateline(gis.file, progress.detail = TRUE)
  gis.file <- check_valid(gis.file, progress.detail = TRUE)
  sf.list <- check_gis_crs(gis.file)
  incProgress(0.4)

  ### Process spatial data
  sf.load.ll   <- sf.list[[1]]
  sf.load.orig <- sf.list[[2]]
  sf.load.df <- st_set_geometry(sf.load.ll, NULL)


  var.idx <- ifelse(var.idx == 1, NA, var.idx - 1)
  if(!is.na(var.idx)) {
    toadd.v <- sf.load.df[, var.idx]
  } else {
    toadd.v <- as.numeric(NA)
  }

  weight.idx <- ifelse(weight.idx == 1, NA, weight.idx - 1)
  if(!is.na(weight.idx)) {
    toadd.w <- sf.load.df[, weight.idx]
  } else {
    toadd.w <- as.numeric(NA)
  }


  # Names of sf object columns set in other create_local code
  sf.load.ll <- sf.load.ll %>%
    st_set_geometry(NULL) %>%
    dplyr::select(all_of(pred.idx)) %>%
    dplyr::mutate(toadd.v, toadd.w, idx = seq_along(toadd.v)) %>%
    st_sf(geometry = st_geometry(sf.load.ll), agr = "constant")

  sf.load.orig <- sf.load.orig %>%
    st_set_geometry(NULL) %>%
    dplyr::select(all_of(pred.idx)) %>%
    dplyr::mutate(toadd.v, toadd.w, idx = seq_along(toadd.v)) %>%
    st_sf(geometry = st_geometry(sf.load.orig), agr = "constant")
  incProgress(0.1)

  # Calculate resolution of the predictions
  model.res <- gis_res_calc(sf.load.ll, sf.load.orig)
  incProgress(0.2)

  # Need names from sf.list[[1]] since sf.load.ll names will be different
  data.names <- list(names(sf.list[[1]])[c(pred.idx, var.idx, weight.idx)])


  ###### Code common to all importing functions ######
  source("server_1_loadModels_create_local.R",
         local = TRUE, echo = FALSE, chdir = TRUE)
  ####################################################
})
