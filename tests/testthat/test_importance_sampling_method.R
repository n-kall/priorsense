set.seed(123)
normal_example <- example_powerscale_model("univariate_normal")
normal_example_sfit <- rstan::stan(model_code = normal_example$model_code, data = normal_example$data, refresh = FALSE, seed = 123)

test_that("powerscale importance sampling methods are properly recorded", {
  
  expect_equal(
    class(
      powerscale(
        fit = normal_example_sfit,
      alpha = 0.5,
      variables = c("mu"),
      is_method = "sis",
      )$powerscaling$importance_sampling
    ),
    c("sis", "importance_sampling", "list")
  )
    expect_equal(
    class(powerscale(
      fit = normal_example_sfit,
      alpha = 0.5,
      variables = c("mu"),
      is_method = "psis",
    )$powerscaling$importance_sampling
    ),
    c("psis", "importance_sampling", "list")
    )
      expect_equal(
    class(powerscale(
      fit = normal_example_sfit,
      alpha = 0.5,
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
      fit = normal_example_sfit,
      variables = c("mu"),
      is_method = "sis",
    )$is_method,
    "sis"
  )
  expect_equal(
    powerscale_sequence(
      fit = normal_example_sfit,
      variables = c("mu"),
      is_method = "tis",
    )$is_method,
    "tis"
  )
    expect_equal(
    powerscale_sequence(
      fit = normal_example_sfit,
      variables = c("mu"),
      is_method = "psis",
    )$is_method,
    "psis"
  )
})


test_that("powerscale importance sampling methods are properly recorded", {
  
  expect_equal(
    class(powerscale(
      fit = normal_example_sfit,
      alpha = 0.5,
      variables = c("mu"),
      is_method = "sis",
    )$powerscaling$importance_sampling
    ),
    c("sis", "importance_sampling", "list")
  )
    expect_equal(
    class(powerscale(
      fit = normal_example_sfit,
      alpha = 0.5,
      variables = c("mu"),
      is_method = "psis",
    )$powerscaling$importance_sampling
    ),
    c("psis", "importance_sampling", "list")
    )
      expect_equal(
    class(powerscale(
      fit = normal_example_sfit,
      alpha = 0.5,
      variables = c("mu"),
      is_method = "tis",
    )$powerscaling$importance_sampling
    ),
    c("tis", "importance_sampling", "list")
  )
})


test_that("powerscale moment match is properly handled", {
  
  expect_warning(
    powerscale(
      fit = normal_example_sfit,
      alpha = 0.5,
      variables = c("mu"),
      is_method = "sis",
      moment_match = TRUE
    ),
    "Moment-matching only works with PSIS. Falling back to moment_match = FALSE"
  )
    expect_warning(
    powerscale(
      fit = normal_example_sfit,
      alpha = 0.5,
      variables = c("mu"),
      is_method = "tis",
      moment_match = TRUE
    ),
    "Moment-matching only works with PSIS. Falling back to moment_match = FALSE"
  )  
})


test_that("powerscale_sequence moment match is properly handled", {
  
  expect_warning(
    powerscale_sequence(
      fit = normal_example_sfit,
      variables = c("mu"),
      is_method = "sis",
      moment_match = TRUE
    ),
    "Moment-matching only works with PSIS. Falling back to moment_match = FALSE"
  )
    expect_warning(
    powerscale_sequence(
      fit = normal_example_sfit,
      variables = c("mu"),
      is_method = "tis",
      moment_match = TRUE
    ),
    "Moment-matching only works with PSIS. Falling back to moment_match = FALSE"
  )  
})
