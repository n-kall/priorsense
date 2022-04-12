set.seed(123)
normal_example <- example_powerscale_model("univariate_normal")

sfit <- suppressWarnings(rstan::stan(
  model_code = normal_example$model_code,
  data = c(normal_example$data, prior_alpha = 1, likelihood_alpha = 1),
  refresh = FALSE,
  seed = 123,
  iter = 1000,
  warmup = 250,
  chains = 1
))

test_that("powerscale with resample actually resamples", {

  ps <- powerscale(
    x = sfit,
    component = "prior",
    alpha = 0.5,
    resample = TRUE
  )
  
  expect_equal(
    ps$powerscaling$resampled,
    TRUE
  )

  expect_equal(
    stats::weights(ps),
    NULL
  )
})


test_that("powerscale_sequence with resample actually resamples", {

  pss <- suppressWarnings(powerscale_sequence(
    x = sfit,
    variables = c("mu"),
    resample = TRUE,
    ))
    expect_equal(
      pss$resampled,
      TRUE
    )
    expect_equal(
      stats::weights(pss$prior_scaled$draws_sequence[[1]])
     ,
      NULL
    )
  })
