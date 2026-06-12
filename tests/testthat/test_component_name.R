ex <- example_powerscale_model()$draws

ex_renamed <- posterior::rename_variables(
  ex,
  log_prior = lprior,
  log_prior_sigma = lprior_sigma,
  log_prior_mu = lprior_mu
)

psd <- create_priorsense_data(ex)

psd_r <- create_priorsense_data(ex_renamed, log_prior_name = "log_prior")

testthat::expect_error(powerscale_sensitivity(ex, log_lik_name = "ll"))

testthat::expect_error(powerscale_sensitivity(ex_renamed))

testthat::expect_equal(
  powerscale_sensitivity(psd_r),
  powerscale_sensitivity(psd)
)

testthat::expect_equal(
  powerscale_sensitivity(ex),
  powerscale_sensitivity(ex_renamed, log_prior_name = "log_prior")
)

