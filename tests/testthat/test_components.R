# create test model fits
set.seed(123)
normal_example <- example_powerscale_model("univariate_normal")
normal_example_sfit <- rstan::stan(model_code = normal_example$model_code, data = normal_example$data, refresh = FALSE, seed = 123)

test_that("powerscale_sensitivity runs without error for different component options", {
  
  expect_error(
    powerscale_sensitivity(
      fit = normal_example_sfit,
      component = "prior",
      variables = "mu",
      log_prior_fn = extract_log_prior
    ),
    NA
  )
  expect_error(
    powerscale_sensitivity(
      fit = normal_example_sfit,
      component = "likelihood",
      variables = "mu",
      log_prior_fn = extract_log_prior
    ),
    NA
  )
  expect_error(
    powerscale_sensitivity(
      fit = normal_example_sfit,
      component = c("likelihood", "prior"),
      variables = "mu",
      log_prior_fn = extract_log_prior
    ),
    NA
  )
}
)

test_that("powerscale_gradients runs without error for different component options", {
  
  expect_error(
    powerscale_gradients(
      fit = normal_example_sfit,
      component = "prior",
      variables = "mu",
      log_prior_fn = extract_log_prior
    ),
    NA
  )
  expect_error(
    powerscale_gradients(
      fit = normal_example_sfit,
      component = "likelihood",
      variables = "mu",
      log_prior_fn = extract_log_prior
    ),
    NA
  )
  expect_error(
    powerscale_sensitivity(
      fit = normal_example_sfit,
      component = c("likelihood", "prior"),
      variables = "mu",
      log_prior_fn = extract_log_prior
    ),
    NA
  )
}
)

test_that("powerscale runs without error for different component options", {
  
  expect_error(
    powerscale_gradients(
      fit = normal_example_sfit,
      component = "prior",
      variables = "mu",
      log_prior_fn = extract_log_prior
    ),
    NA
  )
  expect_error(
    powerscale_gradients(
      fit = normal_example_sfit,
      component = "likelihood",
      variables = "mu",
      log_prior_fn = extract_log_prior
    ),
    NA
  )
}
)
