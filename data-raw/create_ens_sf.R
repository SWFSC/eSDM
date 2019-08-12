# Create ens_sf_...rda objects to use in scripts that create ME&E paper figs
# Loaded .RDATA file is saved GUI environment when using data from
#   eSDM_data_manuscript.zip

library(dplyr)
library(sf)

# Load saved GUI environment
load("data-raw/eSDM_ME&E_final.RDATA")

# Check that ensembles are at expected indices
stopifnot(
  vals.save$ensemble.specs[[1]]["ensmethod"] == "Unweighted",
  vals.save$ensemble.specs[[3]]["ensmethod"] == "Weighted - TSS-based"
)

# Unweighted ensemble
ens.sf.unw <- vals.save$ensemble.models[[1]] %>%
  mutate(CV_ens = SE_ens / Pred_ens) %>%
  st_sf(geometry = vals.save$overlay.base.sfc,agr = "constant")
save(ens.sf.unw, file = "data-raw/ens_sf_unw.rda")

# Ensemble weighted by TSS
ens.sf.wtss <- vals.save$ensemble.models[[3]] %>%
  st_sf(geometry = vals.save$overlay.base.sfc,agr = "constant")
save(ens.sf.wtss, file = "data-raw/ens_sf_wtss.rda")

# Validation presence data
valid.sf.pres <- vals.save$eval.data %>%
  filter(sight == 1)
save(valid.sf.pres, file = "data-raw/valid_sf_pres.rda")
