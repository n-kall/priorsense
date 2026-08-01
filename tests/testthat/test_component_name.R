ex <- example_powerscale_model()$draws

ex_renamed <- posterior::rename_variables(
    ex,
    ll = log_lik,
    log_prior = lprior,
    log_prior_sigma = lprior_sigma,
    log_prior_mu = lprior_mu
)

psd <- create_priorsense_data(ex)

psd_r <- create_priorsense_data(
    ex_renamed,
    log_prior_name = "log_prior",
    log_lik_name = "ll"
)

testthat::expect_error(powerscale_sensitivity(ex, log_lik_name = "ll"))

testthat::expect_error(powerscale_sensitivity(ex_renamed))

testthat::expect_equal(
    powerscale_sensitivity(psd_r),
    powerscale_sensitivity(psd)
)

testthat::expect_equal(
    powerscale_sensitivity(ex),
    powerscale_sensitivity(
        ex_renamed,
        log_prior_name = "log_prior",
        log_lik_name = "ll"
    )
)


ex_new_var <- posterior::mutate_variables(
    ex,
    log_prior = lprior,
    log_lik1 = `log_lik[1]`,
    log_lik2 = `log_lik[1]`,
)

testthat::expect_equal(
    powerscale_sensitivity(
        ex_new_var,
        log_lik_name = "log_lik1",
        variable = "mu"
    ),
    powerscale_sensitivity(
        ex_new_var,
        log_lik_name = "log_lik2",
        variable = "mu"
    )
)

testthat::expect_equal(
    powerscale_sensitivity(
        ex_new_var,
        log_lik_name = "log_lik",
        likelihood_selection = "1",
        separator = "",
        variable = "mu"
    )$likelihood,
    powerscale_sensitivity(
        ex_new_var,
        log_lik_name = "log_lik",
        likelihood_selection = "2",
        separator = "",
        variable = "mu"
    )$likelihood
)
