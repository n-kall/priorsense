psd <- create_priorsense_data(example_powerscale_model()$draws)


test_that("print methods provide output", {

  ps <- powerscale_sensitivity(psd)
  expect_output(print(ps))

  ps_drw <- powerscale(psd, "prior", 0.5)
  expect_output(print(ps_drw))

  ps_summ <- summarise_draws(ps_drw)
  expect_output(print(ps_summ))

  pss <- powerscale_sequence(psd)
  expect_output(print(pss))

  w <- whiten_draws(posterior::example_draws())
  expect_output(print(w))
})
