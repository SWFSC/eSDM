library(sf)

test_that("simple overlay", {
  d <- data.frame(
    lon = c(40, 40, 50, 50, 40), lat = c(10, 20, 20, 10, 10)
  )
  da <- data.frame(
    lon = c(40, 40, 45, 45, 40), lat = c(10, 20, 20, 10, 10)
  )
  db <- data.frame(
    lon = c(45, 45, 50, 50, 45), lat = c(10, 20, 20, 10, 10)
  )

  sfc1 <- st_sfc(
    st_polygon(list(as.matrix(da))), st_polygon(list(as.matrix(db))),
    crs = 4326
  )
  sfc2 <- st_sfc(st_polygon(list(as.matrix(d))), crs = 4326)

  sf1 <- st_sf(data.frame(pred = c(2, 2)), geometry = sfc1, agr = "constant")
  sf1b <- st_sf(data.frame(pred = c(1, 2)), geometry = sfc1, agr = "constant")
  sf2 <- st_sf(data.frame(pred = 2), geometry = sfc2, agr = "constant")
  sf2b <- st_sf(data.frame(pred = 1.5), geometry = sfc2, agr = "constant")

  expect_equal(overlay_sdm(sfc2, sf1, 1, 100), sf2)
  expect_equal(overlay_sdm(sfc2, sf1b, 1, 100), sf2b)

  expect_error(overlay_sdm(sf1, sf2, 1, 100))
  expect_error(overlay_sdm(sfc1, sfc2, 1, 100))
})
