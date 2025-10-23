univariate_normal_draws <- example_powerscale_model()$draws

test_that("powerscale handles variable and variables arguments", {
  psp <- powerscale(
      x = univariate_normal_draws,
      component = "prior",
      alpha = 0.8,
      variable = "mu"
    )
  expect_equal(
    posterior::variables(psp),
    "mu"
  )
  psp2 <- powerscale(
      x = univariate_normal_draws,
      component = "prior",
      alpha = 0.8,
      variables = "mu"
    )
  expect_equal(
    posterior::variables(psp2),
    "mu"
  )

  expect_error(
    powerscale(
      x = univariate_normal_draws,
      component = "prior",
      alpha = 0.8,
      variable = "mu",
      variables = "sigma"
    ),
        "Assertion on '`variable` and `variables`' failed: must be identical if both provided."
    )

  expect_error(
    plot(
      powerscale_sequence(univariate_normal_draws),
      variable = "mu",
      variables = "sigma"
    ),
    "Assertion on '`variable` and `variables`' failed: must be identical if both provided."
  )

}
)
