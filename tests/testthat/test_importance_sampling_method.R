set.seed(123)
normal_example <- example_powerscale_model("univariate_normal")
normal_example_sfit <- rstan::stan(model_code = normal_example$model_code, data = normal_example$data, refresh = FALSE, seed = 123)

test_that("powerscale importance sampling methods are properly recorded", {
  
  expect_equal(
    class(
      powerscale(
        x = normal_example_sfit,
      alpha = 0.5, component = "prior",
      variables = c("mu"),
      is_method = "sis",
      )$powerscaling$importance_sampling
    ),
    c("sis", "importance_sampling", "list")
  )
    expect_equal(
    class(powerscale(
      x = normal_example_sfit,
      alpha = 0.5, component = "prior",
      variables = c("mu"),
      is_method = "psis",
    )$powerscaling$importance_sampling
    ),
    c("psis", "importance_sampling", "list")
    )
      expect_equal(
    class(powerscale(
      x = normal_example_sfit,
      alpha = 0.5, component = "prior",
      variables = c("mu"),
      is_method = "tis",
    )$powerscaling$importance_sampling
    ),
    c("tis", "importance_sampling", "list")
  )
})

test_that("powerscale_sequence importance sampling methods are properly recorded", {
  
  expect_equal(
    powerscale_sequence(
      x = normal_example_sfit,
      variables = c("mu"),
      is_method = "sis",
    )$is_method,
    "sis"
  )
  expect_equal(
    powerscale_sequence(
      x = normal_example_sfit,
      variables = c("mu"),
      is_method = "tis",
    )$is_method,
    "tis"
  )
    expect_equal(
    powerscale_sequence(
      x = normal_example_sfit,
      variables = c("mu"),
      is_method = "psis",
    )$is_method,
    "psis"
  )
})


test_that("powerscale importance sampling methods are properly recorded", {
  
  expect_equal(
    class(powerscale(
      x = normal_example_sfit,
      alpha = 0.5, component = "prior",
      variables = c("mu"),
      is_method = "sis",
    )$powerscaling$importance_sampling
    ),
    c("sis", "importance_sampling", "list")
  )
    expect_equal(
    class(powerscale(
      x = normal_example_sfit,
      alpha = 0.5, component = "prior",
      variables = c("mu"),
      is_method = "psis",
    )$powerscaling$importance_sampling
    ),
    c("psis", "importance_sampling", "list")
    )
      expect_equal(
    class(powerscale(
      x = normal_example_sfit,
      alpha = 0.5, component = "prior",
      variables = c("mu"),
      is_method = "tis",
    )$powerscaling$importance_sampling
    ),
    c("tis", "importance_sampling", "list")
  )
})
