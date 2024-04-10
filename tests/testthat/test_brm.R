library(brms)

set.seed(1234)
dat <- data.frame(
  count = rpois(236, lambda = 20),
  visit = factor(rep(1:4, each = 59)),
  patient = factor(rep(1:59, 4)),
  Age = rnorm(236),
  Trt = factor(sample(0:1, 236, TRUE)),
  AgeSD = abs(rnorm(236, 1)),
  Exp = factor(sample(1:5, 236, TRUE), ordered = TRUE),
  volume = rnorm(236),
  gender = factor(c(rep("m", 30), rep("f", 29)))
)

warmup <- 150
iter <- 200
chains <- 1
stan_model_args <- list(save_dso = FALSE)

brmsfit_example1 <- brm(
  bf(count ~ Trt*Age + mo(Exp) + s(Age) + volume +
      offset(Age) + (1+Trt|visit) + arma(visit, patient),
     sigma ~ Trt),
  data = dat, family = student(),
  prior = set_prior("normal(0,2)", class = "b") +
    set_prior("cauchy(0,2)", class = "sd") +
    set_prior("normal(0,3)", dpar = "sigma"),
  sample_prior = TRUE,
  warmup = warmup, iter = iter, chains = chains,
  stan_model_args = stan_model_args, rename = TRUE,
  save_pars = save_pars(all = TRUE),
  backend = "rstan"
)

test_that("priorsense_data is created", {
  expect_s3_class(
    create_priorsense_data(
      brmsfit_example1
    ),
    "priorsense_data"
  )
}
)

test_that("powerscale returns powerscaled_draws", {
  expect_s3_class(
    powerscale(
      x = brmsfit_example1,
      component = "prior",
      alpha = 0.8
    ),
    "powerscaled_draws"
  )
  expect_s3_class(
    powerscale(
      x = brmsfit_example1,
      component = "likelihood",
      alpha = 1.3,
      prediction = \(x) priorsense::predictions_as_draws(x, brms::posterior_epred),
      moment_match = TRUE
    ),
    "powerscaled_draws"
  )  
}
)

test_that("powerscale_sequence returns powerscaled_sequence", {
  expect_s3_class(
    suppressWarnings(powerscale_sequence(
      x = brmsfit_example1
    )),
    "powerscaled_sequence"
  )
}
)

test_that("powerscale_sensitivity returns powerscaled_sensitivity_summary", {
  expect_s3_class(
    powerscale_sensitivity(
      x = brmsfit_example1
    ),
    "powerscaled_sensitivity_summary"
  )
}
)
