# Code for creating RDA files stored in inst/extdata and used in the
#   example-analysis vignette


###############################################################################
# Prep
require(sf)

files.path <- "../package_file_old"


###############################################################################
# Load data and save as rds file in inst/extdata

### Predictions - Becker et al. 2016
model.b <- read.csv(paste(files.path, "Predictions_Beckeretal2016.csv", sep = "/"))
saveRDS(model.b, file = "inst/extdata/Predictions_Beckeretal2016.rds")
# save(model.b, file = "inst/extdata/Predictions_Beckeretal2016.rda")

### Predictions - Hazen et al. 2017
model.h <- read.csv(paste(files.path, "Predictions_Hazenetal2017.csv", sep = "/"))
saveRDS(model.h, file = "inst/extdata/Predictions_Hazenetal2017.rds")
# save(model.h, file = "inst/extdata/Predictions_Hazenetal2017.rda")

### Predictions - Redfern et al. 2017
model.r <- st_read(paste(files.path, "Predictions_Redfernetal2017.shp", sep = "/"))
saveRDS(model.r, file = "inst/extdata/Predictions_Redfernetal2017.rds")
# save(model.r, file = "inst/extdata/Predictions_Redfernetal2017.rda")


### Validation data
valid.data <- read.csv(
  paste(files.path, "eSDM_Validation_data_all.csv", sep = "/"),
  stringsAsFactors = FALSE
)
saveRDS(valid.data, file = "inst/extdata/eSDM_Validation_data_all.rds")
# save(valid.data, file = "inst/extdata/eSDM_Validation_data_all.rda")

###############################################################################
