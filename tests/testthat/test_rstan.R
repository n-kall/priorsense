#' @srrstats {G5.0} eight schools is a well-established example for Bayesian models
set.seed(123)
eight_schools_example <- example_powerscale_model("eight_schools")
sfit <- rstan::stan(model_code = eight_schools_example$model_code, data = eight_schools_example$data, refresh = FALSE, seed = 123)

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
