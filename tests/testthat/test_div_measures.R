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

test_that("ks_dist handles unequal x/y lengths with y_weights = NULL", {

  set.seed(42)
  x <- rnorm(100)
  y <- rnorm(50)

  expect_no_error(priorsense:::ks_dist(x, y, y_weights = NULL))

  result <- priorsense:::ks_dist(x, y, y_weights = NULL)
  expect_named(result, "ks_dist.D")
  expect_true(is.finite(result))

}
)
