set.seed(123)
normal_example <- example_powerscale_model("univariate_normal")
normal_example_sfit <- rstan::stan(model_code = normal_example$model_code, data = normal_example$data, refresh = FALSE, seed = 123)

test_that("two methods for log_prior give the same output",
{  
  expect_equal(
    log_prior_calc_stanfit(normal_example_sfit),
    log_prior_stanfit(normal_example_sfit)
  )
}
)
