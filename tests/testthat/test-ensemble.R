library(sf)

test_that("rescale", {
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

  df1 <- data.frame(pred = c(2, 3))
  df1e <- data.frame(pred = c(2, 3) / 5)
  sf1 <- st_sf(df1, geometry = sfc1, agr = "constant")
  sf1.e <- st_sf(df1e, geometry = sfc1, agr = "constant")

  df2 <- data.frame(pred = c(2, 3), var = c(0.2, 0.3))
  df2e <- data.frame(pred = c(0.4, 0.6), var = c(0.2, 0.3) * 1/25)
  sf2 <- st_sf(df2, geometry = sfc1, agr = "constant")
  sf2.e <- st_sf(df2e, geometry = sfc1, agr = "constant")

  expect_equal(ensemble_rescale(sf1, 1, "sumto1"), sf1.e)
  expect_equal(ensemble_rescale(sf2, 1, "sumto1", x.var.idx = 2), sf2.e)

  expect_equal(model_abundance(ensemble_rescale(sf1, 1, "abundance", 42), 1), 42)
})
