set.seed(123)
normal_example <- example_powerscale_model("univariate_normal")

sfit <- suppressWarnings(rstan::stan(
  model_code = normal_example$model_code,
  data = normal_example$data,
  refresh = FALSE,
  seed = 123,
  iter = 500,
  warmup = 250,
  chains = 4
))

test_that("moment matching is applied when specified and pareto-k is higher than threshold", {
  expect_true(
    get_powerscaling_details(
      powerscale(
      x = sfit,
      alpha = 0.2,
      component = "likelihood",
      moment_match = TRUE
      )
    )$moment_match
  )
})
