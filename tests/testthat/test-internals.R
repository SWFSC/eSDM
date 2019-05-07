
d.na <- as.numeric(NA)

test_that("internal-rmse", {
  expect_equal(esdm_rmse(c(1, NA), c(2, 3), na.rm = TRUE), 1)
  expect_equal(esdm_rmse(c(1, 2), c(2, NA), na.rm = TRUE), 1)
  expect_equal(esdm_rmse(c(1, NA), c(2, 3), na.rm = FALSE), d.na)
  expect_equal(esdm_rmse(c(1, 2), c(2, NA), na.rm = FALSE), d.na)

  expect_equal(esdm_rmse(c(1:5), c(2:6)), 1)
  expect_equal(esdm_rmse(1, 2), 1)
})


test_that("internal-wtdmean", { #Values calculated using stats::weighted.mean()
  expect_equal(esdm_weighted_mean(c(NA, 2, 3), c(1, 2, 3), na.rm = TRUE), 2.6)
  expect_equal(esdm_weighted_mean(c(NA, 2, 3), c(1, 2, 3), na.rm = FALSE), d.na)
  expect_equal(esdm_weighted_mean(c(5, 2, 3), c(1, 2, 3), na.rm = TRUE), 3)
  expect_equal(esdm_weighted_mean(c(4, 2, 3), c(1, 2, NA), na.rm = TRUE), 8/3)
  expect_equal(esdm_weighted_mean(c(4, 2, 3), c(1, 2, NA), na.rm = FALSE), d.na)
})


test_that("internal-amv", {
  expect_equal(esdm_weighted_var_amv(d.na, 2, 1, na.rm = FALSE), d.na)
  expect_equal(esdm_weighted_var_amv(1:4, 2.5, rep(0.25, 4), na.rm = TRUE), mean((1:4 - 2.5) ^ 2))
  expect_equal(esdm_weighted_var_amv(1:4, 2.5, rep(0.25, 4), na.rm = FALSE), mean((1:4 - 2.5) ^ 2))
  expect_equal(esdm_weighted_var_amv(c(1, 5, NA), 3, c(0.2, 0.8, NA), na.rm = TRUE), 4)
  expect_equal(esdm_weighted_var_amv(c(1, 5, NA), 3, c(0.1, 0.4, 0.5), na.rm = TRUE), 4)
  expect_equal(esdm_weighted_var_amv(c(1, 5, NA), 3, c(0.2, 0.8, NA), na.rm = FALSE), d.na)
})


test_that("internal-wmv", {
  expect_equal(esdm_weighted_var_wmv(c(NA, 0.1), c(0.5, 0.5), na.rm = FALSE), d.na)
  expect_equal(esdm_weighted_var_wmv(c(NA, 0.1), c(0.5, 0.5), na.rm = TRUE), 0.1)
  expect_equal(esdm_weighted_var_wmv(c(0.4, 0.1), c(0.5, 0.5), na.rm = TRUE), 0.4 * 0.25 + 0.1 * 0.25)
})
