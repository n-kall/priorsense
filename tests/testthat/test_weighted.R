library(priorsense)

x <- c(1, 2, 3, 4, 5)
w <- rep(1, length(x))

set.seed(123)
normal_example <- example_powerscale_model("univariate_normal")
normal_example_sfit <- rstan::stan(model_code = normal_example$model_code, data = normal_example$data, refresh = FALSE, seed = 123, iter = 500, warmup = 100)


test_that("weighted quantities work when weights are 1", {
  expect_equal(
    median_weighted(x = x, weights = w),
    c(median = median(x))
  )
  expect_equal(
    mean_weighted(x = x, weights = w),
    c(mean = mean(x))
  )
  expect_equal(
    sd_weighted(x = x, weights = w),
    c(sd = sd(x))
  )
  expect_equal(
    mad_weighted(x = x, weights = w),
    c(mad = mad(x))
  )
  expect_equal(
    var_weighted(x = x, weights = w),
    c(var = var(x))
  )
})


test_that("weighted quantities are close to resampled", {
  
  expect_equal(
    as.vector(
      summarise_draws(
        powerscale(
          normal_example_sfit, variables = "mu", alpha = 0.5,
          log_prior_fn = extract_log_prior
        )
      )$draws_summary[2:7]),
    as.vector(
      summarise_draws(
        resample_draws(
          powerscale(
            normal_example_sfit, variables = "mu", alpha = 0.5,
            log_prior_fn = extract_log_prior
          )
        ), posterior::default_summary_measures()
      )$draws_summary[2:7]), tolerance = 0.08
  )
})
