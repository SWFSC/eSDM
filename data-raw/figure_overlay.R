library(eSDM)
library(sf)
library(RColorBrewer)

# mapview::mapview(preds.3)
# display.brewer.pal(9, "YlGnBu")

x <- st_geometry(preds.2)
y <- st_geometry(preds.3)

# x[1496] is the base geometry polygon
overlap <- st_intersection(x[1496], y)
y.which <- st_intersects(x[1496], y)[[1]]

# graphics::rect(-120, 34.3, -119.8, 34.5, col = "grey81")
# graphics::box()

png("../eSDM paper/Figures/Overlay1.png", width = 4, height = 4, units = "in", res = 300)
par("mar" = rep(0.5, 4))
plot(x, axes = FALSE, col = "navajowhite2", border = "black", lwd = 2,
     xlim = c(-120, -119.8), ylim = c(34.3, 34.5))
plot(y, col = NA, border = "red", lwd = 2, add = TRUE)
# plot(y[y.which], add = TRUE, col = NA, border = brewer.pal(9, "YlGnBu")[4], lwd = 3)
plot(x[1496], add = TRUE, border = "dodgerblue", lwd = 3)
dev.off()

png("../eSDM paper/Figures/Overlay2.png", width = 4, height = 4, units = "in", res = 300)
par("mar" = rep(0.5, 4))
plot(x, axes = FALSE, col = "navajowhite2", border = "black", lwd = 2, xlim = c(-120, -119.8), ylim = c(34.3, 34.5))
plot(y, col = NA, border = "red", lwd = 2, add = TRUE)
plot(overlap, add = TRUE, border = NA, lwd = 2, col = brewer.pal(9, "YlGnBu")[2])
plot(y[y.which], add = TRUE, col = NA, border = brewer.pal(9, "YlGnBu")[4], lwd = 3) #brewer.pal(9, "YlGnBu")[4]
plot(x[1496], add = TRUE, border = "dodgerblue", lwd = 3)
dev.off()
