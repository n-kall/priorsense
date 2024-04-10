set.seed(123)
normal_example <- example_powerscale_model("univariate_normal")
sfit <- rstan::stan(model_code = normal_example$model_code, data = normal_example$data, refresh = FALSE, seed = 123)

test_that("priorsense_data is created", {
  expect_s3_class(
    create_priorsense_data(
      sfit
    ),
    "priorsense_data"
  )
}
)

test_that("powerscale returns powerscaled_draws", {
  expect_s3_class(
    powerscale(
      x = sfit,
      component = "prior",
      alpha = 0.8
    ),
    "powerscaled_draws"
  )
  expect_s3_class(
    powerscale(
      x = sfit,
      component = "likelihood",
      alpha = 0.8
    ),
    "powerscaled_draws"
  )
}
)

test_that("powerscale_seqence returns powerscaled_sequence", {
  expect_s3_class(powerscale_sequence(
      x = sfit
    ),
    "powerscaled_sequence"
  )
}
)

test_that("powerscale_sensitivity returns powerscaled_sensitivity_summary", {
  expect_s3_class(
    powerscale_sensitivity(
      x = sfit
    ),
    "powerscaled_sensitivity_summary"
  )
}
)
