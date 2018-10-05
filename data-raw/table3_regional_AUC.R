library(eSDM)
library(ROCR)
library(sf)


###############################################################################
# Load data
load("../eSDM paper/Example analysis files/eSDM_bw2008_ens.RDATA")

###############################################################################
# Original
# orig1 <- vals.save$models.orig[[1]]
# orig2 <- vals.save$models.orig[[2]]
# orig3 <- vals.save$models.orig[[3]]
#
# valid.data <- vals.save$eval.data
# valid.data.proj <- st_transform(valid.data, st_crs(orig3))
#
#
# #------------------------------------------------------------------------------
# # Calculate AUC using all predictions
# orig1.auc <- eSDM::evaluation_metrics(orig1, "Pred", valid.data, "sight")[1]
# orig2.auc <- eSDM::evaluation_metrics(orig2, "Pred", valid.data, "sight")[1]
# orig3.auc <- eSDM::evaluation_metrics(orig3, "Pred", valid.data.proj, "sight")[1]
#
#
# #------------------------------------------------------------------------------
# # Clip original data to S of 40 deg N and calculate AUC using these
# poly.south <- st_sfc(st_polygon(list(matrix(
#   c(-110, -110, -140, -140, -110, 20, 40, 40, 20, 20), ncol = 2
# ))), crs = 4326)
#
# orig1.clip <- st_intersection(orig1, poly.south)
# orig2.clip <- st_intersection(orig2, poly.south)
# orig3.clip <- st_intersection(orig3, st_transform(poly.south, st_crs(orig3)))
#
# orig1.clip.auc <- eSDM::evaluation_metrics(orig1.clip, "Pred", valid.data, "sight")[1]
# orig2.clip.auc <- eSDM::evaluation_metrics(orig2.clip, "Pred", valid.data, "sight")[1]
# orig3.clip.auc <- eSDM::evaluation_metrics(orig3.clip, "Pred", valid.data.proj, "sight")[1]
#
#
# #------------------------------------------------------------------------------
# # Save data
# df <- data.frame(
#   all = c(orig1.auc, orig2.auc, orig3.auc),
#   south = c(orig1.clip.auc, orig2.clip.auc, orig3.clip.auc)
# )

# write.csv(df, file = "../eSDM paper/Figures/Table3_original.csv", row.names = FALSE)


###############################################################################
# Overlaid
over1 <- vals.save$overlaid.models[[1]]
over2 <- vals.save$overlaid.models[[2]]
over3 <- vals.save$overlaid.models[[3]]

valid.data <- vals.save$eval.data
valid.data.proj <- st_transform(valid.data, st_crs(over3))


#------------------------------------------------------------------------------
# Calculate AUC using all predictions
over1.auc <- eSDM::evaluation_metrics(over1, "Pred.overlaid", valid.data.proj, "sight")[1]
over2.auc <- eSDM::evaluation_metrics(over2, "Pred.overlaid", valid.data.proj, "sight")[1]
over3.auc <- eSDM::evaluation_metrics(over3, "Pred.overlaid", valid.data.proj, "sight")[1]


#------------------------------------------------------------------------------
# Clip overlaid data to S of 40 deg N and calculate AUC using these
poly.south <- st_sfc(st_polygon(list(matrix(
  c(-110, -110, -140, -140, -110, 20, 40, 40, 20, 20), ncol = 2
))), crs = 4326)
poly.south.over <- st_transform(poly.south, st_crs(over3))

over1.clip <- st_intersection(over1, poly.south.over)
over2.clip <- st_intersection(over2, poly.south.over)
over3.clip <- st_intersection(over3, poly.south.over)

over1.clip.auc <- eSDM::evaluation_metrics(over1.clip, "Pred.overlaid", valid.data.proj, "sight")[1]
over2.clip.auc <- eSDM::evaluation_metrics(over2.clip, "Pred.overlaid", valid.data.proj, "sight")[1]
over3.clip.auc <- eSDM::evaluation_metrics(over3.clip, "Pred.overlaid", valid.data.proj, "sight")[1]


#------------------------------------------------------------------------------
# Save data
df.over <- data.frame(
  all = c(over1.auc, over2.auc, over3.auc),
  south = c(over1.clip.auc, over2.clip.auc, over3.clip.auc)
)

write.csv(df.over, file = "../eSDM paper/Figures/Table3.csv", row.names = FALSE)

###############################################################################
