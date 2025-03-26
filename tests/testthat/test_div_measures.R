div_measures <- c("js_div", "js_dist", "hellinger_dist", "kl_div", "kl_dist", "ks_dist", "ws_dist")

psd <- create_priorsense_data(example_powerscale_model()$draws)

test_that("divergence measures do not error", {

  for (d in div_measures) {
    expect_no_error(
      suppressWarnings(
        powerscale_sensitivity(
          psd,
          div_measure = d
        )
      )
    )
  }
}
)
