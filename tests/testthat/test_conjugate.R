library(posterior)

# test using a conjugate model with one parameter
  post <- function(tau, sigma, mu_0, n, x) {

    post_var <- 1/(n/sigma^2 + 1 /tau^2)

    xbar <- mean(x)
    post_mean <- post_var * (n * xbar / sigma^2 + mu_0 / tau^2)

    return(c(mean = post_mean, sd = sqrt(post_var)))

  }


  ll <- function(mu, x) {
    sum(dnorm(x = x, mean = mu, log = TRUE))
  }

#' @srrstats {G5.4a} Testing with simple model
#' @srrstats {G5.5} Correctness tests run with a fixed random seed
#' @srrstats {G5.6} Diagnostic is tested here in a model with no prior-data conflict and then with models with prior-data conflict
#' @srrstats {G5.6a} Diagnostic value is tested with respect to the threshold of 0.05
#' @srrstats {G5.7} Diagnostic value tested to increase as conflict increases
test_that("powerscaling diagnostic makes sense for simple model", {

  set.seed(123)
  mu <- 0
  sigma <- 1

  n <- 50

  x <- rnorm(n = n, mean = mu, sd = sigma)

  # prior1 is mu ~ normal(0, 1)
  post1 <- post(1, sigma = sigma, mu_0 = 0, n = n, x = x)
  post1_draws <- as_draws_df(data.frame(mu = rnorm(4000, post1[["mean"]], post1[["sd"]])))
  post1_loglik <- as_draws_df(data.frame(log_lik = vapply(post1_draws$mu, FUN = ll, FUN.VALUE = c(1), x = x)))
  post1_lprior <- as_draws_df(data.frame(lprior = dnorm(post1_draws$mu, mean = 0, log = TRUE)))
  psd1 <- create_priorsense_data(x = post1_draws, log_prior = post1_lprior, log_lik = post1_loglik)
  ps1 <- powerscale_sensitivity(psd1)

  # prior2 is mu ~ normal(5, 1)
  post2 <- post(1, sigma = sigma, mu_0 = 5, n = n, x = x)
  post2_draws <- as_draws_df(data.frame(mu = rnorm(4000, post2[["mean"]], post2[["sd"]])))
  post2_loglik <- as_draws_df(data.frame(log_lik = vapply(post2_draws$mu, FUN = ll, FUN.VALUE = c(1), x = x)))
  post2_lprior <- as_draws_df(data.frame(lprior = dnorm(post2_draws$mu, mean = 5, log = TRUE)))
  psd2 <- create_priorsense_data(x = post2_draws, log_prior = post2_lprior, log_lik = post2_loglik)
  ps2 <- powerscale_sensitivity(psd2)

  # prior3 is mu ~ normal(10, 1)
  post3 <- post(1, sigma = sigma, mu_0 = 10, n = n, x = x)
  post3_draws <- as_draws_df(data.frame(mu = rnorm(4000, post3[["mean"]], post3[["sd"]])))
  post3_loglik <- as_draws_df(data.frame(log_lik = vapply(post3_draws$mu, FUN = ll, FUN.VALUE = c(1), x = x)))
  post3_lprior <- as_draws_df(data.frame(lprior = dnorm(post3_draws$mu, mean = 10, log = TRUE)))
  psd3 <- create_priorsense_data(x = post3_draws, log_prior = post3_lprior, log_lik = post3_loglik)
  ps3 <- powerscale_sensitivity(psd3)

  # expect conflict to show up as above threshold
  expect_lt(ps1$prior, 0.05)
  expect_gt(ps2$prior, 0.05)
  expect_gt(ps3$prior, 0.05)

  # expect all models to have likelihood sensitivity
  expect_gt(ps1$likelihood, 0.05)
  expect_gt(ps2$likelihood, 0.05)
  expect_gt(ps3$likelihood, 0.05)

  # expect sensitivity to increase as conflict increases
  expect_lt(ps1$prior, ps2$prior)
  expect_lt(ps2$prior, ps3$prior)
  expect_lt(ps1$likelihood, ps2$likelihood)
  expect_lt(ps2$likelihood, ps3$likelihood)

}
)


#' @srrstats {G5.6b} Multiple seeds tested here
#' @srrstats {G5.9} Several seeds tested here
#' @srrstats {G5.9b} Several seeds tested here and results tested to follow similar pattern
test_that("powerscaling diagnostic makes sense for simple model with different seeds", {

  set.seed(456)
  mu <- 0
  sigma <- 1
  n <- 50
  x <- rnorm(n = n, mean = mu, sd = sigma)

  # prior1 is mu ~ normal(0, 1)
  post1 <- post(1, sigma = sigma, mu_0 = 0, n = n, x = x)
  post1_draws <- as_draws_df(data.frame(mu = rnorm(4000, post1[["mean"]], post1[["sd"]])))
  post1_loglik <- as_draws_df(data.frame(log_lik = vapply(post1_draws$mu, FUN = ll, FUN.VALUE = c(1), x = x)))
  post1_lprior <- as_draws_df(data.frame(lprior = dnorm(post1_draws$mu, mean = 0, log = TRUE)))
  psd1 <- create_priorsense_data(x = post1_draws, log_prior = post1_lprior, log_lik = post1_loglik)
  ps1 <- powerscale_sensitivity(psd1)

  # prior2 is mu ~ normal(5, 1)
  post2 <- post(1, sigma = sigma, mu_0 = 5, n = n, x = x)
  post2_draws <- as_draws_df(data.frame(mu = rnorm(4000, post2[["mean"]], post2[["sd"]])))
  post2_loglik <- as_draws_df(data.frame(log_lik = vapply(post2_draws$mu, FUN = ll, FUN.VALUE = c(1), x = x)))
  post2_lprior <- as_draws_df(data.frame(lprior = dnorm(post2_draws$mu, mean = 5, log = TRUE)))
  psd2 <- create_priorsense_data(x = post2_draws, log_prior = post2_lprior, log_lik = post2_loglik)
  ps2 <- powerscale_sensitivity(psd2)

  # prior3 is mu ~ normal(10, 1)
  post3 <- post(1, sigma = sigma, mu_0 = 10, n = n, x = x)
  post3_draws <- as_draws_df(data.frame(mu = rnorm(4000, post3[["mean"]], post3[["sd"]])))
  post3_loglik <- as_draws_df(data.frame(log_lik = vapply(post3_draws$mu, FUN = ll, FUN.VALUE = c(1), x = x)))
  post3_lprior <- as_draws_df(data.frame(lprior = dnorm(post3_draws$mu, mean = 10, log = TRUE)))
  psd3 <- create_priorsense_data(x = post3_draws, log_prior = post3_lprior, log_lik = post3_loglik)
  ps3 <- powerscale_sensitivity(psd3)

  # expect conflict to show up as above threshold
  expect_lt(ps1$prior, 0.05)
  expect_gt(ps2$prior, 0.05)
  expect_gt(ps3$prior, 0.05)

  # expect all models to have likelihood sensitivity
  expect_gt(ps1$likelihood, 0.05)
  expect_gt(ps2$likelihood, 0.05)
  expect_gt(ps3$likelihood, 0.05)

  # expect sensitivity to increase as conflict increases
  expect_lt(ps1$prior, ps2$prior)
  expect_lt(ps2$prior, ps3$prior)
  expect_lt(ps1$likelihood, ps2$likelihood)
  expect_lt(ps2$likelihood, ps3$likelihood)

  # another seed
  set.seed(789)
  mu <- 0
  sigma <- 1
  n <- 50
  x <- rnorm(n = n, mean = mu, sd = sigma)

  # prior1 is mu ~ normal(0, 1)
  post1 <- post(1, sigma = sigma, mu_0 = 0, n = n, x = x)
  post1_draws <- as_draws_df(data.frame(mu = rnorm(4000, post1[["mean"]], post1[["sd"]])))
  post1_loglik <- as_draws_df(data.frame(log_lik = vapply(post1_draws$mu, FUN = ll, FUN.VALUE = c(1), x = x)))
  post1_lprior <- as_draws_df(data.frame(lprior = dnorm(post1_draws$mu, mean = 0, log = TRUE)))
  psd1 <- create_priorsense_data(x = post1_draws, log_prior = post1_lprior, log_lik = post1_loglik)
  ps1 <- powerscale_sensitivity(psd1)

  # prior2 is mu ~ normal(5, 1)
  post2 <- post(1, sigma = sigma, mu_0 = 5, n = n, x = x)
  post2_draws <- as_draws_df(data.frame(mu = rnorm(4000, post2[["mean"]], post2[["sd"]])))
  post2_loglik <- as_draws_df(data.frame(log_lik = vapply(post2_draws$mu, FUN = ll, FUN.VALUE = c(1), x = x)))
  post2_lprior <- as_draws_df(data.frame(lprior = dnorm(post2_draws$mu, mean = 5, log = TRUE)))
  psd2 <- create_priorsense_data(x = post2_draws, log_prior = post2_lprior, log_lik = post2_loglik)
  ps2 <- powerscale_sensitivity(psd2)

  # prior3 is mu ~ normal(10, 1)
  post3 <- post(1, sigma = sigma, mu_0 = 10, n = n, x = x)
  post3_draws <- as_draws_df(data.frame(mu = rnorm(4000, post3[["mean"]], post3[["sd"]])))
  post3_loglik <- as_draws_df(data.frame(log_lik = vapply(post3_draws$mu, FUN = ll, FUN.VALUE = c(1), x = x)))
  post3_lprior <- as_draws_df(data.frame(lprior = dnorm(post3_draws$mu, mean = 10, log = TRUE)))
  psd3 <- create_priorsense_data(x = post3_draws, log_prior = post3_lprior, log_lik = post3_loglik)
  ps3 <- powerscale_sensitivity(psd3)

  # expect conflict to show up as above threshold
  expect_lt(ps1$prior, 0.05)
  expect_gt(ps2$prior, 0.05)
  expect_gt(ps3$prior, 0.05)

  # expect all models to have likelihood sensitivity
  expect_gt(ps1$likelihood, 0.05)
  expect_gt(ps2$likelihood, 0.05)
  expect_gt(ps3$likelihood, 0.05)

  # expect sensitivity to increase as conflict increases
  expect_lt(ps1$prior, ps2$prior)
  expect_lt(ps2$prior, ps3$prior)
  expect_lt(ps1$likelihood, ps2$likelihood)
  expect_lt(ps2$likelihood, ps3$likelihood)
}
)
