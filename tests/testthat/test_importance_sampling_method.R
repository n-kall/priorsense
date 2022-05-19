set.seed(123)
normal_example <- example_powerscale_model("univariate_normal")

sfit <- suppressWarnings(rstan::stan(
  model_code = normal_example$model_code,
  data = normal_example$data,
  refresh = FALSE,
  seed = 123,
  iter = 500,
  warmup = 250,
  chains = 1
))

test_that("powerscale importance sampling methods are properly recorded", {  
  expect_s3_class(
    suppressWarnings(powerscale(
      x = sfit,
        alpha = 0.5, component = "prior",
      variables = c("mu"),
      is_method = "sis",
      ))$powerscaling$importance_sampling,
    "sis"
  )
  expect_s3_class(
    suppressWarnings(powerscale(
      x = sfit,
      alpha = 0.5, component = "prior",
      variables = c("mu"),
      is_method = "psis",
      ))$powerscaling$importance_sampling,
    "psis"
  )
  expect_s3_class(
    suppressWarnings(powerscale(
      x = sfit,
    alpha = 0.5, component = "prior",
    variables = c("mu"),
    is_method = "tis",
    ))$powerscaling$importance_sampling
    ,
    "tis"
    )
})

test_that("powerscale_sequence importance sampling methods are properly recorded", {
  
  expect_equal(
    powerscale_sequence(
      x = sfit,
      variables = c("mu"),
      is_method = "sis",
      )$is_method,
    "sis"
  )
  expect_equal(
    powerscale_sequence(
      x = sfit,
      variables = c("mu"),
      is_method = "tis",
      )$is_method,
    "tis"
  )
  expect_equal(
    suppressWarnings(powerscale_sequence(
      x = sfit,
      variables = c("mu"),
      is_method = "psis",
      ))$is_method,
    "psis"
  )
})
