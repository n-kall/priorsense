set.seed(123)
eight_schools_example <- example_powerscale_model("eight_schools")

sfit <- suppressWarnings(rstan::stan(
  model_code = eight_schools_example$model_code,
  data = eight_schools_example$data,
  refresh = FALSE,
  seed = 123,
  iter = 250,
  warmup = 50,
  chains = 1
))

ps <- powerscale_sequence(sfit, length = 3)

test_that("diagnostic plots give no errors", {
  expect_error(
     powerscale_plot_ecdf(
       ps,
       variables = c("mu", "tau")
     ),
     NA
   )
   expect_error(
     powerscale_plot_dens(
       x = ps,
       variables = c("mu", "tau")
     ),
     NA
   )
  expect_error(
    powerscale_plot_quantities(
      ps,
      variables = c("mu", "tau")
    ),
    NA
  )
})

test_that("plots contain expected data", {
  psq <- powerscale_plot_quantities(
    ps,
    variables = c("mu"),
    quantities = c("quantile", "mean"),
    quantity_args = list(probs = c(0.1, 0.9))
  )
  expect_equal(
    colnames(psq$data),
    c("variable", "alpha", "n_eff", "pareto_k", "component", "quantity", "value", "id", "pareto_k_value")
  )

  expect_equal(
    levels(psq$data$quantity),
    c("q10", "q90", "mean", "cjs_dist")
  )
})
