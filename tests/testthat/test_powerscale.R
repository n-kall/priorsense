univariate_normal_draws <- example_powerscale_model()$draws

test_that("priorsense_data is created", {
  expect_s3_class(
    create_priorsense_data(
      univariate_normal_draws
    ),
    "priorsense_data"
  )
}
)

#' @srrstats {G5.3} Missing values tested
test_that("powerscale returns powerscaled_draws with no missing values", {
  psp <- powerscale(
      x = univariate_normal_draws,
      component = "prior",
      alpha = 0.8
    )
  expect_s3_class(
    psp,
    "powerscaled_draws"
  )
  expect_false(checkmate::anyMissing(psp))

  psl <- powerscale(
      x = univariate_normal_draws,
      component = "likelihood",
      alpha = 0.8
  )
  
  expect_s3_class(
    psl,
    "powerscaled_draws"
  )
  expect_false(checkmate::anyMissing(psp))

}
)

test_that("powerscale_seqence returns powerscaled_sequence", {
  expect_s3_class(
    suppressWarnings(powerscale_sequence(
      x = univariate_normal_draws
    )),
    "powerscaled_sequence"
  )
}
)

test_that("powerscale_sensitivity returns powerscaled_sensitivity_summary", {
  expect_s3_class(
    powerscale_sensitivity(
      x = univariate_normal_draws
    ),
    "powerscaled_sensitivity_summary"
  )
}
)

test_that("powerscale_sequence uses input alphas correctly", {

  lower_alpha <- 0.5
  upper_alpha <- 2.5
  pss <- suppressWarnings(powerscale_sequence(
    x = univariate_normal_draws,
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
    get_powerscaling_details(pss$prior_scaled$draws_sequence[[1]])$alpha,
    0.5
  )

  expect_equal(
    pss$alphas[length(pss$alphas)],
    2.5
  )

  expect_equal(
    get_powerscaling_details(pss$prior_scaled$draws_sequence[[length(pss$alphas)]])$alpha,
    2.5
  )

  expect_equal(
    length(pss$alphas),
    10
  )

}
)

test_that("powerscale_sequence adapts alphas and keeps pareto-k low", {
  k_threshold <- 0.7
  pss <- suppressWarnings(powerscale_sequence(
    x = univariate_normal_draws,
    auto_alpha_range = TRUE
  ))

  expect_lt(
    get_powerscaling_details(pss$likelihood_scaled$draws_sequence[[1]])$diagnostics$khat,
    k_threshold
  )

  expect_lt(
    get_powerscaling_details(pss$likelihood_scaled$draws_sequence[[length(pss$likelihood_scaled$draws_sequence)]])$diagnostics$khat,
    k_threshold
  )

  expect_lt(
    attr(pss$prior_scaled$draws_sequence[[1]], "powerscaling")$diagnostics$khat,
    k_threshold
  )

  expect_lt(
    get_powerscaling_details(pss$prior_scaled$draws_sequence[[length(pss$prior_scaled$draws_sequence)]])$diagnostics$khat,
    k_threshold
  )

}
)

test_that("powerscale_sequence gives symmetric range", {

  lower_alpha <- 0.3
  length <- 9
  pss <- suppressWarnings(powerscale_sequence(
    x = univariate_normal_draws,
    symmetric = TRUE,
    lower_alpha = lower_alpha,
    length = length
  ))
  
  expect_equal(
    pss$alphas[1],
    lower_alpha
  )
  
  expect_equal(
    get_powerscaling_details(pss$prior_scaled$draws_sequence[[1]])$alpha,
    lower_alpha
  )
  
  expect_equal(
    pss$alphas[length(pss$alphas)],
    1 / lower_alpha
  )
  
  expect_equal(
    get_powerscaling_details(pss$prior_scaled$draws_sequence[[length(pss$alphas)]])$alpha,
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

#' @srrstats {G5.9a} Adding trivial noise to data does not meaningfully change results*
test_that("small variation in draws does not affect result", {

  adjusted_draws <- univariate_normal_draws + .Machine$double.eps

  orig_ps <- powerscale_sensitivity(univariate_normal_draws)
  adjusted_ps <- powerscale_sensitivity(adjusted_draws)

  expect_equal(orig_ps, adjusted_ps)
}
)


#' @srrstats {G5.2} error behaviour tested here
#' @srrstats {G5.8, G5.8d} test edge case out of scope
test_that("powerscaling with alpha < 0 is an error", {
  expect_error(powerscale(univariate_normal_draws,
             component = "prior",
             alpha = -1))
}
)

#' @srrstats {G5.8, G5.8a} test edge case zero-length data
test_that("powerscaling zero draws is an error", {
  zero_draws <- data.frame(mu = numeric(), log_lik = numeric(), lprior = numeric())
  expect_error(powerscale(zero_draws, component = "prior", alpha = 0.1))
}
)

#' @srrstats {G5.2a, G5.2b, G5.8b} constant weights unsupported and give explicit error message
test_that("powerscaling with constant loglik is an error", {
  const_draws <- data.frame(mu = 1:100, log_lik = rep(1, times = 100), lprior = 1:100)
  expect_error(powerscale(const_draws, component = "likelihood", alpha = 0.1),
               "Log likelihood is constant. Power-scaling will not work in this case")
}
)

test_that("powerscaling with constant lprior is an error", {
  const_draws <- data.frame(mu = 1:100, lprior = rep(1, times = 100), log_lik = 1:100)
  expect_error(powerscale(const_draws, component = "prior", alpha = 0.1),
               "Log prior is constant. Power-scaling will not work in this case")
}
)

#' @srrstats {G5.8, G5.8c} test edge case with NAs
test_that("powerscaling with NA weights is an error", {
  na_draws <- data.frame(mu = 1:100, log_lik = rep(NA, times = 100), lprior = 1:100)
  expect_error(powerscale(na_draws, component = "likelihood", alpha = 0.1))
}
)
