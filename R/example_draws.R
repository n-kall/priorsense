## set.seed(12345)

## toy_model_code <- "data {
##   int N;
##   vector[N] y;
##   real mu_center;
##   real<lower=0> mu_scale;
##   real<lower=0> sigma_scale;
## }
## parameters {
##   real mu;
##   real<lower=0> sigma;
## }
## transformed parameters {
##   real log_prior;
##   log_prior = normal_lpdf(sigma | 0, sigma_scale)
##     + normal_lpdf(mu | mu_center, mu_scale); 
## }
## model {
##     target += log_prior;
##     target += normal_lpdf(y | mu, sigma);
## }
## generated quantities {
##   vector[N] log_lik;
##   for (n in 1:N) log_lik[n] =  normal_lpdf(y[n] | mu, sigma);
## }
## "

## toy_model <- rstan::stan_model(model_code = toy_model_code)

## toy_data <- list(
##       y = rnorm(20),
##       prior_alpha = 1,
##       likelihood_alpha = 1,
##       mu_center = 5,
##       mu_scale = 1,
##       sigma_scale = 2.5,
##       N = 20
## )

## toy_fit <- rstan::sampling(
##   object = toy_model,
##   data = toy_data,
##   seed = 12345
## )
