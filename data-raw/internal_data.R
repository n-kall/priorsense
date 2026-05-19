library(cmdstanr)

schools_model <- example_powerscale_model("eight_schools")

stan_model <- cmdstan_model(stan_file = write_stan_file(schools_model$model_code))

fit_schools <- stan_model$sample(
  data = schools_model$data,
  refresh = 0,
  seed = 123
)

draws_eight_schools <- fit_schools$draws()

normal_model <- example_powerscale_model()

stan_model <- cmdstan_model(stan_file = write_stan_file(normal_model$model_code))

fit_normal <- stan_model$sample(
  data = normal_model$data,
  refresh = 0,
  seed = 123
)

draws_univariate_normal <- fit_normal$draws()


usethis::use_data(
  draws_eight_schools,
  draws_univariate_normal,
  overwrite = TRUE,
  internal = TRUE
)
