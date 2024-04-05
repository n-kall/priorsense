##' Example Stan model for power-scaling
##'
##' Provides example models (with data) that are ready for use with
##' power-scaling.
##' @param model Character specifying which model code to
##'   return. Currently "univariate_normal" and "eight_schools" are
##'   implemented.
##' @return List containing model code and corresponding data.
##' @export
example_powerscale_model <- function(model = "univariate_normal") {

  examples <- powerscale_examples()

  return(list(model_code = examples[[model]][["model_code"]],
              data = examples[[model]][["data"]]))
}

powerscale_examples <- function() {

  list(
    univariate_normal =
      list(
        model_code = "data {
  int<lower=1> N;
  array[N] real y;

  vector<lower=0>[2] prior_alpha;

  real<lower=0> likelihood_alpha;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
transformed parameters {
  vector[2] lprior;
  // priors
  lprior[1] = normal_lpdf(mu | 0, 1);
  lprior[2] = normal_lpdf(sigma | 0, 2.5);
}
model {
  target += dot_product(prior_alpha, lprior);
  // likelihood
  target += likelihood_alpha * normal_lpdf(y | mu, sigma);
}
generated quantities {
  vector[N] log_lik;
  // likelihood
  for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | mu, sigma);
}

",
data = list(
  y = c(9.5, 10.2, 9.1, 9.1, 10.3, 10.9, 11.7, 10.3, 9.6, 8.6, 9.1,
        11.1, 9.3, 10.5, 9.7, 10.3, 10.0, 9.8, 9.6, 8.3, 10.2, 9.8,
        10.0, 10.0, 9.1),
  N = 25,
  prior_alpha = c(1, 1),
  likelihood_alpha = 1
)
),
eight_schools =
  list(
    model_code = "data {
  int<lower=0> J;          // number of schools
  array[J] real y;         // estimated treatment effects
  real<lower=0> sigma[J];  // s.e. of effect estimates
  real<lower=0> prior_alpha; // power-scaling
  real<lower=0> likelihood_alpha; // power-scaling
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
}
model {
  // priors
  target += normal_lpdf(theta_trans | 0, 1);
  target += prior_alpha * normal_lpdf(mu | 0, 5);
  target += prior_alpha * cauchy_lpdf(tau | 0, 5);

  //likelihood
  target += likelihood_alpha * normal_lpdf(y | theta, sigma);
}
generated quantities  {
  vector[J] log_lik;
  real lprior;
  for (j in 1:J)
    log_lik[j] = normal_lpdf(y[j] | theta[j], sigma[j]);

  // priors to power-scale
  lprior = cauchy_lpdf(tau | 0, 5)
    + normal_lpdf(mu | 0, 5);
}

",
data = list(
  J = 8,
  y = c(28,  8, -3,  7, -1,  1, 18, 12),
  sigma = c(15, 10, 16, 11,  9, 11, 10, 18),
  prior_alpha = 1,
  likelihood_alpha = 1
)
)
)
}
