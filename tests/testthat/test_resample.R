set.seed(123)
normal_example <- example_powerscale_model("univariate_normal")
normal_example_sfit <- rstan::stan(model_code = normal_example$model_code, data = normal_example$data, refresh = FALSE, seed = 123)


test_that("powerscale with resample actually resamples", {
  
  expect_equal(
    powerscale(
      x = normal_example_sfit,
      component = "prior",
      alpha = 0.5,
      variables = c("mu"),
      resample = TRUE,
    )$powerscaling$resampled,
    TRUE
  )

  expect_equal(
    weights(powerscale(
      x = normal_example_sfit,
      component = "prior",
      alpha = 0.5,
      variables = c("mu"),
      resample = TRUE
    )),
    NULL
  )
})


test_that("powerscale_sequence with resample actually resamples", {
  
  expect_equal(
    powerscale_sequence(
      x = normal_example_sfit,
      variables = c("mu"),
      resample = TRUE,
    )$resampled,
    TRUE
  )
})
