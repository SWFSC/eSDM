library(dplyr)
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
bite <- eSDM::pts2poly_vertices(read.csv("../eSDM-data/Ignore/eSDM_data_sample/SoCal_bight.csv"), crs = 4326)


preds.1 <- st_set_agr(st_crop(x, bite), "constant")
preds.2 <- st_set_agr(st_crop(y, bite), "constant")
preds.3 <- st_set_agr(st_crop(z, bite), "constant")
rm(x, y, z)

row.names(preds.1) <- 1:nrow(preds.1)
row.names(preds.2) <- 1:nrow(preds.2)
row.names(preds.3) <- 1:nrow(preds.3)

# To handle 'old CRS method' warning
st_crs(preds.1) <- st_crs(preds.1)
st_crs(preds.2) <- st_crs(preds.2)
st_crs(preds.3) <- st_crs(preds.3)

# Spice up preds.1 for the sake of examples
preds.1.geom <- st_geometry(preds.1)
preds.1 <-  data.frame(st_set_geometry(preds.1, NULL)) %>%
  mutate(Density2 = runif(325),
         Var1 = runif(325) / 100,
         Var2 = runif(325) / 100) %>%
  st_sf(geometry = preds.1.geom, agr = "constant")

# plot(bite, axes = TRUE)
# plot(st_geometry(preds.1), add = TRUE, col = NA, border = "red")
# plot(st_geometry(preds.2), add = TRUE, col = NA, border = "blue")
# plot(st_geometry(preds.3), add = TRUE, col = NA, border = "green")

usethis::use_data(preds.1) #, overwrite = TRUE)
usethis::use_data(preds.2)
usethis::use_data(preds.3)


### Create sample validation data to use in examples
x <- read.csv("../eSDM-data/Ignore/eSDM_data_sample/Validation_data.csv")
validation.data <- x %>%
  select(lon, lat, sight, count) %>%
  st_as_sf(coords = c("lon","lat"), crs = st_crs(4326), agr = "constant")
validation.data <- st_crop(validation.data, bite)

# plot(bite, axes = TRUE)
# plot(st_geometry(validation.data), add = TRUE)
# plot(validation.data[2], pch = 19)

usethis::use_data(validation.data)
