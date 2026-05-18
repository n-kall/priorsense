library(cmdstanr)

schools_model <- example_powerscale_model("eight_schools")

stan_model <- cmdstan_model(stan_file = write_stan_file(schools_model$model_code))

fit <- stan_model$sample(
  data = schools_model$data,
  refresh = 0,
  seed = 123
)

draws_eight_schools <- fit$draws()

usethis::use_data(draws_eight_schools, overwrite = TRUE, internal = TRUE)
