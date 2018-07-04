library(eSDM)
library(sf)
library(RColorBrewer)

# mapview::mapview(preds.3)

x <- st_geometry(preds.2)
y <- st_geometry(preds.3)

# x[1496] is the base geometry polygon
overlap <- st_intersection(x[1496], y)

# graphics::rect(-120, 34.3, -119.8, 34.5, col = "grey81")
# graphics::box()

png("../eSDM paper/Figures/Overlay1.png", width = 4, height = 4, units = "in", res = 300)
par("mar" = c(5.1, 4.1, 4.1, 2.1) / 4)
plot(x, axes = FALSE, col = "tan", border = "black", lwd = 2,
     xlim = c(-120, -119.8), ylim = c(34.3, 34.5))
plot(y, col = NA, border = "red", lwd = 2, add = TRUE)
plot(x[1496], add = TRUE, border = "dodgerblue", lwd = 3)
dev.off()

png("../eSDM paper/Figures/Overlay2.png", width = 4, height = 4, units = "in", res = 300)
par("mar" = c(5.1, 4.1, 4.1, 2.1) / 4)
plot(x, axes = FALSE, col = "tan", border = "black", lwd = 2, xlim = c(-120, -119.8), ylim = c(34.3, 34.5))
plot(y, col = NA, border = "red", lwd = 2, add = TRUE)
plot(x[1496], add = TRUE, border = "dodgerblue", lwd = 3)
plot(overlap, add = TRUE, border = "dodgerblue", lwd = 2,
     col = brewer.pal(9, "YlGnBu")[2])
dev.off()
