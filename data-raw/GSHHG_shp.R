# Save app's default land files as .RDATA files
# Shapefiles details: http://www.soest.hawaii.edu/pwessel/gshhg/
# Didn't make sense to put GSHHG_shp folder in data-raw because it's so big

library(sf)
library(lwgeom)
library(devtools)

# Load L1 (world except for Antarctica) and L6 (Antarctica ground line)
gshhg.f.L1 <- st_read("../Data files/GSHHG_shp/f/GSHHS_f_L1.shp")
gshhg.h.L1 <- st_read("../Data files/GSHHG_shp/h/GSHHS_h_L1.shp")
gshhg.i.L1 <- st_read("../Data files/GSHHG_shp/i/GSHHS_i_L1.shp")
gshhg.l.L1 <- st_read("../Data files/GSHHG_shp/l/GSHHS_l_L1.shp")
gshhg.c.L1 <- st_read("../Data files/GSHHG_shp/c/GSHHS_c_L1.shp")

gshhg.f.L6 <- st_read("../Data files/GSHHG_shp/f/GSHHS_f_L6.shp")
gshhg.h.L6 <- st_read("../Data files/GSHHG_shp/h/GSHHS_h_L6.shp")
gshhg.i.L6 <- st_read("../Data files/GSHHG_shp/i/GSHHS_i_L6.shp")
gshhg.l.L6 <- st_read("../Data files/GSHHG_shp/l/GSHHS_l_L6.shp")
gshhg.c.L6 <- st_read("../Data files/GSHHG_shp/c/GSHHS_c_L6.shp")

# Combine L1 and L6
gshhg.f.L16 <- st_geometry(rbind(gshhg.f.L1, gshhg.f.L6))
gshhg.h.L16 <- st_geometry(rbind(gshhg.h.L1, gshhg.h.L6))
gshhg.i.L16 <- st_geometry(rbind(gshhg.i.L1, gshhg.i.L6))
gshhg.l.L16 <- st_geometry(rbind(gshhg.l.L1, gshhg.l.L6))
gshhg.c.L16 <- st_geometry(rbind(gshhg.c.L1, gshhg.c.L6))
rm(gshhg.f.L1, gshhg.f.L6, gshhg.h.L1, gshhg.h.L6, gshhg.i.L1, gshhg.i.L6,
   gshhg.l.L1, gshhg.l.L6, gshhg.c.L1, gshhg.c.L6)

# # Check validity of polygons; only gshhg.l.L16 is valid
# all(st_is_valid(gshhg.f.L16))
# all(st_is_valid(gshhg.h.L16))
# all(st_is_valid(gshhg.i.L16))
# all(st_is_valid(gshhg.l.L16))
# all(st_is_valid(gshhg.c.L16))

# Make polygons valid
gshhg.f.L16 <- st_make_valid(gshhg.f.L16)
gshhg.h.L16 <- st_make_valid(gshhg.h.L16)
gshhg.i.L16 <- st_make_valid(gshhg.i.L16)
gshhg.l.L16 <- st_make_valid(gshhg.l.L16)
gshhg.c.L16 <- st_make_valid(gshhg.c.L16)

# Check polygons validity again
all(st_is_valid(gshhg.f.L16))
all(st_is_valid(gshhg.h.L16))
all(st_is_valid(gshhg.i.L16))
all(st_is_valid(gshhg.l.L16))
all(st_is_valid(gshhg.c.L16))

# Save to data folder
devtools::use_data(gshhg.f.L16)
devtools::use_data(gshhg.h.L16)
devtools::use_data(gshhg.i.L16)
devtools::use_data(gshhg.l.L16)
devtools::use_data(gshhg.c.L16)


