set.seed(123)
eight_schools_example <- example_powerscale_model("eight_schools")

sfit <- suppressWarnings(rstan::stan(
  model_code = eight_schools_example$model_code,
  data = eight_schools_example$data,
  refresh = FALSE,
  seed = 123,
  iter = 1000,
  warmup = 250,
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



