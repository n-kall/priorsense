library(cmdstanr)
library(priorsense)
normal_model <- example_powerscale_model()

stan_model <- cmdstan_model(stan_file = write_stan_file(normal_model$model_code))

fit <- stan_model$sample(
  data = normal_model$data,
  refresh = 0,
  seed = 123
)

draws_univariate_normal <- fit$draws()

usethis::use_data(draws_univariate_normal, overwrite = TRUE, internal = TRUE)
