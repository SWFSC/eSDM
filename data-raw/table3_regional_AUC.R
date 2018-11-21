# Code for creating Table 3 for eSDM manuscript (Woodman et al. in prep)

###################################################################################################
library(eSDM) #devtools::install_github("smwoodman/eSDM")
library(ROCR)
library(sf)

# Load data. This .RDATA file is a workspace environment from the GUI saved after using
#   the GUI to overlay the original predictions (from Becker et al. 2016, Hazen et al. 2017,
#   and Redfern et al 2017) following the steps outlined in the 'Example Analysis' section
#   of Woodman et al. (in prep)
load("../eSDM paper/Example analysis workspaces/eSDM_bw2008_ens.RDATA")


###################################################################################################
# Calculate AUC

#--------------------------------------------------------------------------------------------------
### Calculate AUC using all of the overlaid predictions
over1 <- vals.save$overlaid.models[[1]] #Overlaid predictions of Becker et al. (2016)
over2 <- vals.save$overlaid.models[[2]] #Overlaid predictions of Hazen et al. (2017)
over3 <- vals.save$overlaid.models[[3]] #Overlaid predictions of Redfern et al. (2017)

valid.data <- vals.save$eval.data
valid.data.cea <- st_transform(valid.data, st_crs(over3)) #Cylindrical equal area projection

over1.auc <- eSDM::evaluation_metrics(over1, "Pred.overlaid", valid.data.cea, "sight")[1]
over2.auc <- eSDM::evaluation_metrics(over2, "Pred.overlaid", valid.data.cea, "sight")[1]
over3.auc <- eSDM::evaluation_metrics(over3, "Pred.overlaid", valid.data.cea, "sight")[1]


#--------------------------------------------------------------------------------------------------
### Clip overlaid predictions to S of 40 deg N and calculate AUC using clipped predictions
poly.south <- st_sfc(st_polygon(list(matrix(
  c(-110, -110, -140, -140, -110, 20, 40, 40, 20, 20), ncol = 2
))), crs = 4326)
poly.south.over <- st_transform(poly.south, st_crs(over3))

over1.clip <- st_intersection(over1, poly.south.over)
over2.clip <- st_intersection(over2, poly.south.over)
over3.clip <- st_intersection(over3, poly.south.over)

# Large number of points reported because we clipped the predictions
over1.clip.auc <- eSDM::evaluation_metrics(over1.clip, "Pred.overlaid", valid.data.cea, "sight")[1]
over2.clip.auc <- eSDM::evaluation_metrics(over2.clip, "Pred.overlaid", valid.data.cea, "sight")[1]
over3.clip.auc <- eSDM::evaluation_metrics(over3.clip, "Pred.overlaid", valid.data.cea, "sight")[1]


###################################################################################################
# Save table with AUC values
df.over <- data.frame(
  all = c(over1.auc, over2.auc, over3.auc),
  south = c(over1.clip.auc, over2.clip.auc, over3.clip.auc)
)

write.csv(df.over, file = "../eSDM paper/Figures/Table3.csv", row.names = FALSE)

###################################################################################################
