library(priorsense)

test_that("powerscale_derivative gives 0 for uniform log_component", {
  expect_equal(
    priorsense::powerscale_derivative(
      x = seq(0, 1, 0.01),
      log_component = log(rep(1/length(seq(0, 1, 0.01)), length(seq(0, 1, 0.01)))),
      quantity = "mean"),
    c(psens_mean = 0)
  )
  expect_equal(
    priorsense::powerscale_derivative(
      x = seq(0, 1, 0.01),
      log_component = log(rep(1/length(seq(0, 1, 0.01)), length(seq(0, 1, 0.01)))),
      quantity = "sd"),
    c(psens_sd = 0)
  )
  expect_equal(
    priorsense::powerscale_derivative(
      x = seq(0, 1, 0.01),
      log_component = log(rep(1/length(seq(0, 1, 0.01)), length(seq(0, 1, 0.01)))),
      quantity = "var"),
    c(psens_var = 0)
  )
})

## test_that("powerscale_derivative gives specific value for derivative", {
##   expect_equal(
##     priorsense::powerscale_derivative(
##       x = seq(0, 1, 0.01),
##       log_component = log(1 + seq(0, 1, 0.01)),
##       quantity = "mean"),
##     c(ps_mean_deriv = 0.05801754),
##     tolerance = 0.001
##   )
##   expect_equal(
##     priorsense::powerscale_derivative(
##       x = seq(0, 1, 0.01),
##       log_component = log(1 + seq(0, 1, 0.01)),
##       quantity = "sd"),
##     c(ps_sd_deriv = -0.002316339),
##     tolerance = 0.001
##   )
##   expect_equal(
##     priorsense::powerscale_derivative(
##       x = seq(0, 1, 0.01),
##       log_component = log(1 + seq(0, 1, 0.01)),
##       quantity = "var"),
##     c(ps_var_deriv = -0.001350646),
##     tolerance = 0.001
##   )
## })

test_that("powerscale_derivative gives warning if not using mean, sd or var", {
  expect_warning(
    priorsense::powerscale_derivative(
      x = seq(0, 1, 0.01),
      log_component = log(1 + seq(0, 1, 0.01)),
      quantity = "median"),
    "Power-scaling derivative for medians or quantiles is zero. Consider using powerscale_gradients instead."
  )
  expect_warning(
    priorsense::powerscale_derivative(
      x = seq(0, 1, 0.01),
      log_component = log(1 + seq(0, 1, 0.01)),
      quantity = "q95"),
    "Power-scaling derivative for medians or quantiles is zero. Consider using powerscale_gradients instead."
  )
  expect_warning(
    priorsense::powerscale_derivative(
      x = seq(0, 1, 0.01),
      log_component = log(1 + seq(0, 1, 0.01)),
      quantity = "mad"),
    "Power-scaling derivative for medians or quantiles is zero. Consider using powerscale_gradients instead."
  )
})
