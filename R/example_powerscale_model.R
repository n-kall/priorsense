##' Example Stan model for power-scaling
##'
##' Provides example models (with data) that are ready for use with power-scaling.
##' @param model Character specifying which model code to return. Currently "univariate_normal" and "eight_schools" are implemented.
##' @return List containing model code and corresponding data.
##' @export
example_powerscale_model <- function(model = "univariate_normal") {

  if (model == "univariate_normal") {
    str <- "data {
int<lower=1> N;
real y[N];
real<lower=0> prior_alpha;
real<lower=0> likelihood_alpha;
}
parameters {
real mu;
real<lower=0> sigma;
}
transformed parameters {
real log_prior = 0;
// priors
log_prior += normal_lpdf(mu | 0, 1);
log_prior += normal_lpdf(sigma | 0, 2.5);
}
model {
target += prior_alpha * log_prior;
// likelihood
target += likelihood_alpha * normal_lpdf(y | mu, sigma);
}
generated quantities {
vector[N] log_lik;
// likelihood
for (n in 1:N) log_lik[n] =  normal_lpdf(y[n] | mu, sigma);
}
"
    data <- list(y = stats::rnorm(100, 10, 1), N = 100)
  }
  
  if (model == "eight_schools") {
    str <- "data {
  int<lower=0> J;          // number of schools
  real y[J];               // estimated treatment effects
  real<lower=0> sigma[J];  // s.e. of effect estimates
}
parameters {
  vector[J] theta_trans; // transformation of theta
  real mu; // hyper-parameter of mean
  real<lower=0> tau; // hyper-parameter of sd
}
transformed parameters{
  vector[J] theta;
  // original theta
  theta = theta_trans * tau + mu;
}
model {
  // priors
  target += normal_lpdf(theta_trans | 0, 1);
  target += normal_lpdf(mu | 0, 5);
  target += cauchy_lpdf(tau | 0, 5);

  //likelihood
  target += normal_lpdf(y | theta, sigma);
}
generated quantities  {
  vector[J] log_lik;
  real log_prior;
  for (j in 1:J)
    log_lik[j] = normal_lpdf(y[j] | theta[j], sigma[j]);

  // priors to power-scale
  log_prior = cauchy_lpdf(tau | 0, 5)
    + normal_lpdf(mu | 0, 5);
}

"
    data <- list(
      J = 8,
      y = c(28,  8, -3,  7, -1,  1, 18, 12),
      sigma = c(15, 10, 16, 11,  9, 11, 10, 18)
    )
  }
  
  return(list(model_code = str, data = data))
}
