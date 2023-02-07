cmdstanr_available <- require(cmdstanr)

if(cmdstanr_available) {

set.seed(123)
normal_example <- example_powerscale_model("univariate_normal")

test_that("powerscale functions work for CmdStanFit", {
  skip_on_cran()
  cs <- cmdstanr::cmdstan_model(
    stan_file = cmdstanr::write_stan_file(normal_example$model_code)
  )

  cfit <- cs$sample(
    data = normal_example$data,
    refresh = 0,
    seed = 123,
    iter_sampling = 250,
    iter_warmup = 250,
    chains = 1
  )  
  expect_s3_class(
    create_powerscaling_data(
      cfit
    ),
    "powerscaling_data"
  )
  expect_s3_class(
    powerscale(
      x = cfit,
      component = "prior",
      alpha = 0.8
    ),
    "powerscaled_draws"
  )
  expect_s3_class(
    powerscale(
      x = cfit,
      component = "likelihood",
      alpha = 0.8
    ),
    "powerscaled_draws"
  )
  expect_s3_class(
    suppressWarnings(powerscale_sequence(
      x = cfit
    )),
    "powerscaled_sequence"
  )
  expect_s3_class(
    powerscale_sensitivity(
      x = cfit
    ),
    "powerscaled_sensitivity_summary"
  )
}
)

}