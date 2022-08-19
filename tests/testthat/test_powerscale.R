set.seed(123)
normal_example <- example_powerscale_model("univariate_normal")

sfit <- suppressWarnings(rstan::stan(
  model_code = normal_example$model_code,
  data = normal_example$data,
  refresh = FALSE,
  seed = 123,
  iter = 1000,
  warmup = 250,
  chains = 1
))

test_that("powerscaling_data is created", {
  expect_error(
    create_powerscaling_data(
      sfit
    ),
    NA
  )
}
)

test_that("powerscale returns powerscaled_draws", {
  expect_s3_class(
    powerscale(
      x = sfit,
      component = "prior",
      alpha = 0.8
    ),
    "powerscaled_draws"
  )
  expect_s3_class(
    powerscale(
      x = sfit,
      component = "likelihood",
      alpha = 0.8
    ),
    "powerscaled_draws"
  )
}
)

test_that("powerscale_seqence returns powerscaled_sequence", {
  expect_s3_class(
    suppressWarnings(powerscale_sequence(
      x = sfit
    )),
    "powerscaled_sequence"
  )
}
)

test_that("powerscale_sensitivity returns powerscaled_sensitivity_summary", {
  expect_s3_class(
    powerscale_sensitivity(
      x = sfit
    ),
    "powerscaled_sensitivity_summary"
  )
}
)

test_that("powerscale_sequence uses input alphas correctly", {
  lower_alpha <- 0.5
  upper_alpha <- 2.5
  pss <- suppressWarnings(powerscale_sequence(
    x = sfit,
    lower_alpha = lower_alpha,
    upper_alpha = upper_alpha,
    symmetric = FALSE,
    length = 10
  ))

  expect_equal(
    pss$alphas[1],
    0.5
  )
  expect_equal(
    pss$prior_scaled$draws_sequence[[1]]$powerscaling$alpha,
    0.5
  )
  expect_equal(
    pss$alphas[length(pss$alphas)],
    2.5
  )
  expect_equal(
    pss$prior_scaled$draws_sequence[[length(pss$alphas)]]$powerscaling$alpha,
    2.5
  )
  expect_equal(
    length(pss$alphas),
    10
  )
}
)

test_that("powerscale_sequence adapts alphas and keeps pareto-k low", {
  k_threshold <- 0.5
  pss <- suppressWarnings(powerscale_sequence(
    x = sfit,
    auto_alpha_range = TRUE
  ))

  expect_lt(
    pss$likelihood_scaled$draws_sequence[[1]]$powerscaling$importance_sampling$diagnostics$pareto_k,
    k_threshold
  )

  expect_lt(
    pss$likelihood_scaled$draws_sequence[[length(pss$likelihood_scaled$draws_sequence)]]$powerscaling$importance_sampling$diagnostics$pareto_k,
    k_threshold
  )

  expect_lt(
    pss$prior_scaled$draws_sequence[[1]]$powerscaling$importance_sampling$diagnostics$pareto_k,
    k_threshold
  )

  expect_lt(
    pss$prior_scaled$draws_sequence[[length(pss$prior_scaled$draws_sequence)]]$powerscaling$importance_sampling$diagnostics$pareto_k,
    k_threshold
  )

}
)

test_that("powerscale_sequence gives symmetric range", {
  lower_alpha <- 0.3
  length <- 9
  pss <- suppressWarnings(powerscale_sequence(
    x = sfit,
    symmetric = TRUE,
    lower_alpha = lower_alpha,
    length = length
  ))
  expect_equal(
    pss$alphas[1],
    lower_alpha
  )
  expect_equal(
    pss$prior_scaled$draws_sequence[[1]]$powerscaling$alpha,
    lower_alpha
  )
  expect_equal(
    pss$alphas[length(pss$alphas)],
    1 / lower_alpha
  )
  expect_equal(
    pss$prior_scaled$draws_sequence[[length(pss$alphas)]]$powerscaling$alpha,
    1 / lower_alpha
  )
  expect_equal(
    length(pss$alphas),
    8
  )
  expect_equal(
    abs(log(pss$alphas[1])),
    abs(log(pss$alphas[length(pss$alphas)]))
  )
}
)
