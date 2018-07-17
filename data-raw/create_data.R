library(eSDM)
library(sf)

### Create sample predictions to use in examples
# sample_predictions_2
x <- st_read("data-raw/eSDM_shpExport/eSDM_Sample_predictions_2_csv__pred__orig.shp", agr = "constant")
# sample_predictions_1
y <- st_read("data-raw/eSDM_shpExport (1)/eSDM_Sample_predictions_1_csv__PredModel1__orig.shp", agr = "constant")
# sample_predictions_4
z <- st_transform(
  st_read("data-raw/eSDM_shpExport (2)/eSDM_Sample_predictions_4_gdb__Pred1__orig.shp", agr = "constant"),
  4326
)
bite <- eSDM::pts_to_sfc_coords(read.csv("../data_provided/SoCal_bite.csv"), 4326)

preds.1 <- st_set_agr(st_crop(x, bite), "constant")
preds.2 <- st_set_agr(st_crop(y, bite), "constant")
preds.3 <- st_set_agr(st_crop(z, bite), "constant")
rm(x, y, z)

row.names(preds.1) <- 1:nrow(preds.1)
row.names(preds.2) <- 1:nrow(preds.2)
row.names(preds.3) <- 1:nrow(preds.3)

# Spice up preds.1 for the sake of examples
preds.1 <- st_sf(
  data.frame(st_set_geometry(preds.1, NULL), Density2 = runif(325)),
  geometry = st_geometry(preds.1), agr = "constant"
)

# plot(bite, axes = TRUE)
# plot(st_geometry(preds.1), add = TRUE, col = NA, border = "red")
# plot(st_geometry(preds.2), add = TRUE, col = NA, border = "blue")
# plot(st_geometry(preds.3), add = TRUE, col = NA, border = "green")

devtools::use_data(preds.1)
devtools::use_data(preds.2)
devtools::use_data(preds.3)


### Create sample validation data to use in examples
x <- read.csv("../data_provided/Validation_data.csv")
validation.data <- st_as_sf(
  x[, c("mlon", "mlat", "sight", "count")], crs = 4326,
  coords = c("mlon","mlat"), agr = "constant"
)
validation.data <- st_crop(validation.data, bite)

# plot(bite, axes = TRUE)
# plot(st_geometry(validation.data), add = TRUE)
# plot(validation.data[2], pch = 19)

devtools::use_data(validation.data)
