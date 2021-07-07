set.seed(123)
normal_example <- example_powerscale_model("univariate_normal")
normal_example_sfit <- rstan::stan(model_code = normal_example$model_code, data = normal_example$data, refresh = FALSE, seed = 123)

normal_example_cmdmodel <- cmdstanr::cmdstan_model(cmdstanr::write_stan_file(normal_example$model_code))
normal_example_cmdfit <- normal_example_cmdmodel$sample(data = normal_example$data, seed = 123, refresh = 0)

test_that("powerscale_sensitivity works with rstan and cmdstanr and gives comparable results", {
  
  expect_equal(
    powerscale_sensitivity(
      fit = normal_example_sfit,
      component = "prior",
      variables = "mu",
      log_prior_fn = extract_log_prior
    ),
    powerscale_sensitivity(
      fit = normal_example_cmdfit,
      component = "prior",
      variables = "mu",
      log_prior_fn = extract_log_prior
    ), tolerance = 0.02
  )
}
)
