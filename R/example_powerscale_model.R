##' Example models for power-scaling sensitivity
##'
##' Provides example models (with data) that are ready for use with
##' power-scaling.
##' @param model Character specifying which model code to
##'   return. Currently "univariate_normal" and "eight_schools" are
##'   implemented.
##' @return List containing model code and corresponding data.
##' @srrstats {G5.1} This function exposes two example data sets.
##' @examples
##' ex_normal <- example_powerscale_model(model = "univariate_normal", language = "stan")
##'
##' ex_eightschools <- example_powerscale_model(model = "eight_schools", language = "jags")
##' @export
example_powerscale_model <- function(model = "univariate_normal", language = "stan") {


  univariate_normal_model <- list(
    data = list(
      y = c(9.5, 10.2, 9.1, 9.1, 10.3, 10.9, 11.7, 10.3, 9.6, 8.6, 9.1,
            11.1, 9.3, 10.5, 9.7, 10.3, 10.0, 9.8, 9.6, 8.3, 10.2, 9.8,
            10.0, 10.0, 9.1),
      N = 25
    ),
    stan = "data {
  int<lower=1> N;
  array[N] real y;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
transformed parameters {
  real lprior; // joint prior
  real lprior_mu = normal_lpdf(mu | 0, 1); // marginal prior density for mu
  real lprior_sigma = normal_lpdf(sigma | 0, 2.5); // marginal prior density for sigma
  lprior = lprior_mu + lprior_sigma;
}
model {
  // priors
  target += lprior;
  // likelihood
  target += normal_lpdf(y | mu, sigma);
}
generated quantities {
  vector[N] log_lik;
  // likelihood
  for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | mu, sigma);
}

",
jags = "model {
  for(n in 1:N) {
    y[n] ~ dnorm(mu, tau)
    log_lik[n] <- logdensity.norm(y[n], mu, tau)
  }
  mu ~ dnorm(0, 1)
  sigma ~ dnorm(0, 1 / 2.5^2) T(0,)
  tau <- 1 / sigma^2

  lprior_mu <- logdensity.norm(mu, 0, 1)
  lprior_sigma <- logdensity.norm(sigma, 0, 1 / 2.5^2)
  lprior <- lprior_mu + lprior_sigma
}
",

nimble = quote({for (n in 1:N) {
  y[n] ~ dnorm(mu, sd = sigma)
  log_lik[n] <- dnorm(y[n], mu, sd = sigma, log = TRUE)
}

  mu ~ dnorm(0, sd = 1)
  sigma ~ dnorm(0, sd = 2.5)

  lprior_mu <- dnorm(mu, 0, sd = 1, log = TRUE)
  lprior_sigma <- dnorm(sigma, 0, sd = 2.5, log = TRUE)

  lprior <- lprior_mu + lprior_sigma
})
)


  eight_schools_model <- list(
    data = list(
      J = 8,
      y = c(28,  8, -3,  7, -1,  1, 18, 12),
      sigma = c(15, 10, 16, 11,  9, 11, 10, 18)
    ),
    stan = "data {
  int<lower=0> J;          // number of schools
  array[J] real y;         // estimated treatment effects
  array[J] real<lower=0> sigma;  // s.e. of effect estimates
}
parameters {
  vector[J] theta_trans; // transformation of theta
  real mu;               // hyper-parameter of mean
  real<lower=0> tau;     // hyper-parameter of sd
}
transformed parameters{
  vector[J] theta;
  // original theta
  theta = theta_trans * tau + mu;
  real lprior; // joint prior density
  real lprior_mu = normal_lpdf(mu | 0 , 5); // prior density mu
  real lprior_tau = normal_lpdf(tau | 0, 5); // prior density tau
  lprior = lprior_mu + lprior_tau;
}
model {
  // priors
  target += normal_lpdf(theta_trans | 0, 1);
  target += lprior;

  //likelihood
  target += normal_lpdf(y | theta, sigma);
}
generated quantities  {
  vector[J] log_lik;
  for (j in 1:J)
    log_lik[j] = normal_lpdf(y[j] | theta[j], sigma[j]);
}

",
jags = "model {
  for (j in 1:J) {
    y[j] ~ dnorm(theta[j], pow(sigma[j], -2))
    theta[j] <- mu + tau * theta_trans[j]
    theta_trans[j] ~ dnorm(0, 1)
    log_lik[j] <- logdensity.norm(y[j] , theta[j], pow(sigma[j], -2))

}

  mu ~ dnorm(0, pow(5, -2))
  tau ~ dnorm(0, pow(5, -2)) T(0,)

  lprior_mu <- logdensity.norm(mu, 0, pow(5, -2))
  lprior_tau <- logdensity.norm(tau, 0, pow(5, -2))
  lprior <- lprior_mu + lprior_tau
}
",
nimble = quote({
  for (j in 1:J) {
    y[j] ~ dnorm(theta[j], sd = sigma[j])
    theta[j] <- mu + tau * theta_trans[j]
    theta_trans[j] ~ dnorm(0, 1)
    log_lik[j] <- dnorm(y[j] , theta[j], sd = sigma[j], log = TRUE)

  }

  mu ~ dnorm(0, sd = 5)
  tau ~ dnorm(0, sd = 5)

  lprior_mu <- dnorm(mu, 0, sd = 5, log = TRUE)
  lprior_tau <- dnorm(tau, 0, sd = 5, log = TRUE)
  lprior <- lprior_mu + lprior_tau
})
)

  if (model == "univariate_normal") {

    model_code <- univariate_normal_model[[language]]
    data <- univariate_normal_model[["data"]]
    draws <- get("draws_univariate_normal", asNamespace("priorsense"))
  } else if (model == "eight_schools") {
    model_code <- eight_schools_model[[language]]
    data <- eight_schools_model[["data"]]
    draws <- get("draws_eight_schools", asNamespace("priorsense"))
  }
  
  return(
    list(
      model_code = model_code,
      data = data,
      draws = draws
    )
  )
}
