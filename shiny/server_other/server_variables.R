### Variables used server-wider


###############################################################################
### For projection of spatial objects
EPSG.all <- make_EPSG()
crs.ll <- CRS(subset(EPSG.all, code == 4326)[,3]) # WGS 84
# Aka CRS("+init=epsg:4326"), but use other one because
#   imported GIS objects don't have "+init..." in their crs
crs.cea <- CRS(paste0("+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 ", 
                      "+datum=WGS84 +units=m +no_defs", 
                      " +ellps=WGS84 +towgs84=0,0,0"))
# From proj4string() of JVR model, 
#   Had Projected Coordinate System 'World_Cylindrical_Equal_Area'


###############################################################################
### For plotting

col.ramp <- c("#313695", "#4575b4", "#74add1", "#abd9e9", "#d1e5f0", 
              "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026")
breaks <- seq(1, 0, -0.1)
labels.at <- seq(0.95, 0.05, -0.1)
labels.lab <- rev(c("Lowest 60%", "35 - 40%", "30 - 35%", "25 - 30%", 
                    "20 - 25%", "15 - 20%", "10 - 15%", 
                    "5 - 10%", "2 - 5%", "Highest 2%"))

###############################################################################
