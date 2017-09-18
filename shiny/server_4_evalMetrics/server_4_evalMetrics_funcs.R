### Non-reactive functions for ensEvalMetrics tab


###############################################################################
### Process csv file of p/a points and convert it to SPtsDF
# For data.csv first two columns must be lat and long
pa.data.csv.process <- function(data.csv, p.a.ind, count.pa.ind) {
  # Sort by lat (primary) then long for bottom up sort
  pa.spdf <- data.sort(data.csv, 2, 1)
  pa.spdf[,1] <- ifelse(pa.spdf[,1] > 180, pa.spdf[,1] - 360, pa.spdf[,1])
  
  # Create SPtsDF
  coordinates(pa.spdf) <- c(1, 2)
  proj4string(pa.spdf) <- crs.ll
  names(pa.spdf) <- "pa.num"
  pa.spdf$pa.sight <- rep(p.a.ind, length(pa.spdf))
  
  if (count.pa.ind == 2) pa.spdf$pa.num <- NA
  
  return(pa.spdf)
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
         "Presence and absence points don't have identical projections"), 
    need(identicalCRS(pres.pts, model.data), 
         paste("Presense and absence points and model predictions",
               "don't have identical projections"))
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
         "Presence and absence points don't have identical projections"), 
    need(identicalCRS(pres.pts, model.data), 
         "Pres/abs points and model predictions don't have identical projections")
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
