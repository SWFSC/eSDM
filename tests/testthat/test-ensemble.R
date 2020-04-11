library(sf)

test_that("rescale", {
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


test_that("create ens", {
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

  # Basic data frame and sf object
  df1 <- data.frame(pred1 = c(2, 3), pred2 = c(4, 6), pred3 = c(3, 5),
                    var1 = c(0.2, 0.3), var2 = c(0.4, 0.6), var3 = c(0.3, 0.5))
  sf1 <- st_sf(df1, geometry = sfc1, agr = "constant")
  sf1c <- st_sf(df1, geometry = sfc1, agr = "identity")

  # Mean of first two pred columns with amv
  df1e <- cbind(
    df1, Pred_ens = c(mean(c(2, 4)), mean(c(3, 6))), Var_ens = c(1, 2.25)
  )
  sf1.e <- st_sf(df1e, geometry = sfc1, agr = "constant")

  # Mean of all 3 pred columns with amv
  df1eb.mean <- c(mean(c(2, 4, 3)), mean(c(3, 6, 5)))
  df1eb.var <- apply(cbind(df1[, 1:3], df1eb.mean), 1, function(i, w){
    esdm_weighted_var_amv(i[1:3], i[4], w)
  }, w = rep(1/3, 3))
  df1eb <- cbind(df1, Pred_ens = df1eb.mean, Var_ens = c(2/3, 14/9))
  sf1.eb <- st_sf(df1eb, geometry = sfc1, agr = "constant")

  # Mean of first two pred columns with wmv
  df1e2.mean <- c(mean(c(2, 4)), mean(c(3, 6)))
  df1e2.var <- apply(df1[, 4:5], 1, esdm_weighted_var_wmv, w = c(0.5, 0.5))
  df1e2 <- cbind(df1, Pred_ens = df1e2.mean, Var_ens = df1e2.var)
  sf1.ec <- st_sf(df1e, geometry = sfc1, agr = c(rep("identity", 6), rep("constant", 2)))
  sf1.e2 <- st_sf(df1e2, geometry = sfc1, agr = "constant")

  # Tests
  expect_equal(ensemble_create(sf1, 1:2, c(0.5, 0.5)), sf1.e)
  expect_equal(ensemble_create(sf1, 1:3, c(1/3, 1/3, 1/3)), sf1.eb)
  expect_equal(ensemble_create(sf1c, 1:2, c(0.5, 0.5)), sf1.ec)

  expect_equal(ensemble_create(sf1, 1:2, c(0.5, 0.5), x.var.idx = 4:5), sf1.e2)

})
