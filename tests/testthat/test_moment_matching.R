set.seed(123)
normal_example <- example_powerscale_model("univariate_normal")
normal_example_sfit <- rstan::stan(model_code = normal_example$model_code, data = normal_example$data, refresh = FALSE, seed = 123)

## normal_example_cmdmodel <- cmdstanr::cmdstan_model(cmdstanr::write_stan_file(normal_example$model_code))
## normal_example_cmdfit <- normal_example_cmdmodel$sample(data = normal_example$data, seed = 123, refresh = 0)

test_that("moment matching is applied when specified and pareto-k is higher than threshold",
{  
  expect_true(
    powerscale(
      fit = normal_example_sfit,
      alpha = 0.2,
      component = "likelihood",
      moment_match = TRUE
    )$powerscaling$moment_match
  )
})

test_that("moment matching lowers pareto-k",
{  
  expect_lt(
    powerscale(
      fit = normal_example_sfit,
      alpha = 0.2,
      component = "likelihood",
      moment_match = TRUE
    )$powerscaling$importance_sampling$diagnostics$pareto_k,
    powerscale(
      fit = normal_example_sfit,
      alpha = 0.2,
      component = "likelihood",
      moment_match = FALSE
    )$powerscaling$importance_sampling$diagnostics$pareto_k
  )
}
)

test_that("moment matching warnings are displayed",
{

  expect_warning(
    powerscale(
      fit = normal_example_sfit,
      alpha = 0.2,
      component = "likelihood",
      moment_match = TRUE,
      is_method = "sis"
    ),
    "Moment-matching only works with PSIS. Falling back to moment_match = FALSE"
  )
  ## expect_warning(
  ##   powerscale(
  ##     fit = normal_example_cmdfit,
  ##     alpha = 0.2,
  ##     component = "likelihood",
  ##     moment_match = TRUE,
  ##     ),
  ##   "Moment-matching does not yet work with fits created with cmdstanr. Falling back to moment_match = FALSE"
  ## )
}
)
