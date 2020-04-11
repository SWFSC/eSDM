library(sf)

test_that("abundance", {
  da <- data.frame(
    lon = c(45, 40, 40, 45, 45), lat = c(10, 10, 20, 20, 10)
  )
  db <- data.frame(
    lon = c(50, 45, 45, 50, 50), lat = c(10, 10, 20, 20, 10)
  )

  sfc1 <- st_sfc(
    st_polygon(list(matrix(unlist(da), ncol = 2))), st_polygon(list(matrix(unlist(db), ncol = 2))),
    crs = 4326
  )
  sfc2 <- st_sfc(
    st_polygon(list(matrix(unlist(da), ncol = 2))), st_polygon(list(matrix(unlist(db), ncol = 2)))
  )

  sf1 <- st_sf(data.frame(dens = c(2, 2)), geometry = sfc1, agr = "constant")
  sf2 <- st_sf(data.frame(dens = c(2, 2)), geometry = sfc2, agr = "constant")

  area1 <- as.numeric(units::set_units(st_area(sfc1), "km^2")) * 2

  expect_equal(model_abundance(sf1, 1), sum(area1))
  expect_equal(model_abundance(sf1, 1, sum.abund = FALSE), area1)
  expect_error(model_abundance(sf2, 1))
})
