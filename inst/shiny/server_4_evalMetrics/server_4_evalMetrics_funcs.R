### Non-reactive functions for ensEvalMetrics tab


###############################################################################
# Process evaluation metric validation data

### Process x
eval_proc_df <- function(x, y, p.codes, a.codes) {
  stopifnot(
    is.data.frame(x),
    ncol(x) == 3,
    y %in% c(1, 2)
  )

  if (y == 1) {
    #----------------------------------
    # Count data
    validate(
      need(is.numeric(x[, 3]) | is.integer(x[, 3]),
           paste("Error: Selected validation data column is not numeric.",
                 "Consider loading data as 'Presence or absence' data"))
    )

    x <- x %>%
      dplyr::rename(lon = 1, lat = 2, count = 3) %>%
      dplyr::mutate(sight = as.numeric(count > 0)) %>%
      dplyr::select(1, 2, 4, 3)

    p.data <- x %>% dplyr::filter(sight ==  1)
    a.data <- x %>% dplyr::filter(sight ==  0)

  } else {
    #----------------------------------
    # Presence/absence data
    x <- x %>%
      dplyr::rename(lon = 1, lat = 2, sight = 3) %>%
      dplyr::mutate(count = NA)

    validate(
      need(!(is.null(p.codes) & is.null(a.codes)),
           paste("Error: Please select one or more",
                 "presence codes and absence codes")),
      need(all(!(p.codes %in% a.codes)),
           paste("Error: Please ensure that no presence and",
                 "absence codes are the same")),
      need(length(unique(x$sight)) == length(c(p.codes, a.codes)),
           paste("Error: Please ensure that all codes are classified",
                 "as either presence or absence codes"))
    )

    p.data <- x %>% dplyr::filter(sight %in% p.codes)
    p.data$sight <- 1
    a.data <- x %>% dplyr::filter(sight %in% a.codes)
    a.data$sight <- 0
  }

  # Convert data.frame to sf object
  list(eval_proc_sf(p.data), eval_proc_sf(a.data))
}


#----------------------------------------------------------
### Process csv file of p/a points and convert it to sf object
# For data.csv first two columns must be long and lat, respectively
eval_proc_sf <- function(x) {
  stopifnot(
    ncol(x) == 4,
    names(x) == c("lon", "lat", "sight", "count")
  )

  # Sort by lat (primary) then long for bottom up sort and then create sf obj
  pts.sf <- st_as_sf(data_sort(x, 2, 1), coords = c(1, 2),
                     agr = "constant", crs = 4326)

  # Perform checks
  pts.sf <- check_dateline(pts.sf)
  # pts.sf <- check_valid(pts.sf) #don't need this for pts (hopefully)

  return(pts.sf)
}


###############################################################################
# Helper functions for calculating model evaluation metrics

### Return df with overlap of presence/absence data and model
# Returned df contains data columns for density, abundance, ...
# ...sightings (p/a)) flag, and number of animals observed
# Separate function since AUC, TSS, and RMSE calcs require this overlap info
helper.over <- function(pres.pts, abs.pts, model.data, model.dens.idx) {
  # Check the point and model have same crs
  validate(
    need(identicalCRS(pres.pts, abs.pts),
         paste("Error: Presence and absence points do not",
               "have identical projections")),
    need(identicalCRS(pres.pts, model.data),
         paste("Error: Presence points and model predictions do not",
               "have identical projections"))
  )

  # Overlap of validation data and model
  data.dens <- model.data@data[ ,model.dens.idx]
  model.data@data <- data.frame(dens = data.dens)
  model.data$abund <- model.data$dens * (area(model.data) / 1e+06)

  pres.model.over <- over(pres.pts, model.data)
  abs.model.over <- over(abs.pts, model.data)
  pres.model.over$sight <- rep(1, nrow(pres.model.over))
  abs.model.over$sight <- rep(0, nrow(abs.model.over))
  pres.model.over$num <- pres.pts$pa.num
  abs.model.over$num <- abs.pts$pa.num

  model.pa <- rbind(pres.model.over, abs.model.over)
  model.pa.nona <- model.pa[!is.na(model.pa$dens), ]

  return(model.pa.nona)
}


#################################################
### Returns ROCR::prediction() output of density and sightings columns
# Output used by AUC and TSS calcs
helper.pred <- function(pres.pts, abs.pts, model.data, model.dens.idx,
                        helper.over.result = NA) {
  # Check the point and model have same crs
  validate(
    need(identicalCRS(pres.pts, abs.pts),
         paste("Error: Presence and absence points do not",
               "have identical projections")),
    need(identicalCRS(pres.pts, model.data),
         paste("Error: Presence points and model predictions do not",
               "have identical projections"))
  )

  # Overlap of presence/absence data and model
  model.pa.df <- helper.over.result
  if (all(is.na(model.pa.df))) {
    model.pa.df <- helper.over(pres.pts, abs.pts, model.data, model.dens.idx)
  }

  # Prediction
  pred <- prediction(model.pa.df$dens, model.pa.df$sight)

  return(pred)
}



###############################################################################
# Functions for calculating metric values

### Returns AUC for given p/a points and model predictions
auc.func <- function(pres.pts, abs.pts, model.data, model.dens.idx,
                     plot.name = "", helper.pred.result = NA) {
  # Get overlap of pres/abs data and model and create prediction object
  pred <- helper.pred.result
  if (suppressWarnings(is.na(pred))) {
    pred <- helper.pred(pres.pts, abs.pts, model.data, model.dens.idx)
  }

  # Calculate metric
  perf.auc <- performance(pred, measure = "auc")

  # perf.roc <- performance(pred, measure = "tpr", x.measure = "fpr")
  # plot(perf.roc, main = plot.name)
  # abline(a=0, b= 1)

  return(perf.auc@y.values[[1]])
}


#################################################
### Calculates TSS for given presence and absence points and model predictions
# Also calculates cutoff value for predictions that maximizes sens + spec
# Returns TSS value (can return vector also with cutoff value)
tss.func <- function(pres.pts, abs.pts, model.data, model.dens.idx,
                     helper.pred.result = NA) {
  # Get overlap of pres/abs data and model and create prediction object
  pred <- helper.pred.result
  if (suppressWarnings(is.na(pred))) {
    pred <- helper.pred(pres.pts, abs.pts, model.data, model.dens.idx)
  }

  # Calculate metrics and then TSS
  perf.sens <- performance(pred, measure = "sens")
  sens <- perf.sens@y.values[[1]]

  perf.spec <- performance(pred, measure = "spec")
  spec <- perf.spec@y.values[[1]]

  tss <- sens + spec - 1
  idx <- which.max(tss)
  tss.max <- tss[idx]

  # cutoff.max <- perf.sens@x.values[[1]][idx]
  # cutoff.max <- cutoff.max.adj * data.dens.max

  return (tss.max) #c(tss.max, cutoff.max)
}


#################################################
### Calculates RMSE for given presence numbers and model predictions
rmse.func <- function(pres.pts, abs.pts, model.data, model.dens.idx,
                      helper.over.result = NA) {
  # Get overlap of pres/abs data and model
  model.pa.df <- helper.over.result
  if (all(is.na(model.pa.df))) {
    model.pa.df <- helper.over(pres.pts, abs.pts, model.data, model.dens.idx)
  }

  # Calculate RMSE
  rmse.val <- sqrt(mean((model.pa.df$abund - model.pa.df$num)^2))

  return(rmse.val)
}

###############################################################################
