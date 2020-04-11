library(sf)

test_that("pts2poly", {
  # Need to go counter-clockwise form top right point for pts2poly_centroid
  d <- data.frame(
    lon = c(50, 40, 40, 50, 50), lat = c(10, 10, 0, 0, 10)
  )
  da <- data.frame(
    lon = c(45, 40, 40, 45, 45), lat = c(10, 10, 0, 0, 10)
  )
  db <- data.frame(
    lon = c(50, 45, 45, 50, 50), lat = c(10, 10, 0, 0, 10)
  )

  sfc1 <- st_sfc(
    st_polygon(list(matrix(unlist(da), ncol = 2))), st_polygon(list(matrix(unlist(db), ncol = 2))),
    crs = 4326
  )
  sfc2 <- st_sfc(st_polygon(list(matrix(unlist(d), ncol = 2))), crs = 4326)

  sf1 <- st_sf(data.frame(pred = c(2, 2)), geometry = sfc1, agr = "constant")
  sf1b <- st_sf(data.frame(pred = c(1, 2)), geometry = sfc1, agr = "constant")
  sf2 <- st_sf(data.frame(pred = 2), geometry = sfc2, agr = "constant")
  sf2b <- st_sf(data.frame(pred = 1.5), geometry = sfc2, agr = "constant")

  expect_equal(pts2poly_vertices(d, crs = 4326), sfc2)
  expect_equal(pts2poly_vertices(rbind(d, NA), crs = 4326), sfc2)
  expect_equal(pts2poly_vertices(rbind(da, NA, db), crs = 4326), sfc1)

  pt1 <- data.frame(lon = 45, lat = 5, pred = 2)

  expect_equal(pts2poly_centroids(pt1, 5, crs = 4326, agr = "constant"), sf2)
  expect_equal(pts2poly_centroids(pt1[, 1:2], 5, crs = 4326), sfc2)
  expect_equal(pts2poly_centroids(pt1, 5, crs = 4326, agr = "constant", precision = 10),
               st_set_precision(sf2, 10))
  expect_equal(pts2poly_centroids(pt1[, 1:2], 5, crs = 4326, precision = 10),
               st_set_precision(sfc2, 10))

  expect_error(pts2poly_centroids(pt1[, 1:2], 5, crs = 4326, agr = "constant"))
})
