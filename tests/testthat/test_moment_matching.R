set.seed(123)
normal_example <- example_powerscale_model("univariate_normal")
normal_example_sfit <- rstan::stan(model_code = normal_example$model_code, data = normal_example$data, refresh = FALSE, seed = 123)

test_that("moment matching is applied when specified and pareto-k is higher than threshold",
{  
  expect_true(
    powerscale(
      x = normal_example_sfit,
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
      x = normal_example_sfit,
      alpha = 0.2,
      component = "likelihood",
      moment_match = TRUE
    )$powerscaling$importance_sampling$diagnostics$pareto_k,
    powerscale(
      x = normal_example_sfit,
      alpha = 0.2,
      component = "likelihood",
      moment_match = FALSE
    )$powerscaling$importance_sampling$diagnostics$pareto_k
  )
}
)
