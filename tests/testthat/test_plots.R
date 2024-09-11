eight_schools_example <- example_powerscale_model("eight_schools")

ps <- powerscale_sequence(eight_schools_example$draws, length = 3)

test_that("diagnostic plots give no errors", {
  expect_error(
     powerscale_plot_ecdf(
       ps,
       variable = c("mu", "tau")
     ),
     NA
   )
   expect_error(
     powerscale_plot_dens(
       x = ps,
       variable = c("mu", "tau")
     ),
     NA
   )
  expect_error(
    powerscale_plot_quantities(
      ps,
      variable = c("mu", "tau")
    ),
    NA
  )
})

test_that("plots contain expected data", {
  psq <- powerscale_plot_quantities(
    ps,
    variable = c("mu"),
    quantity = c("quantile", "mean"),
    quantity_args = list(probs = c(0.1, 0.9))
  )
  expect_equal(
    colnames(psq$data),
    c("variable", ".powerscale_alpha", "pareto_k_threshold", "pareto_k", "component", "quantity", "value", "id", "pareto_k_value")
  )

  expect_equal(
    unique(psq$data$quantity),
    c("q10", "q90", "mean", "cjs_dist")
  )
})


test_that("help_text behaves as expected in plots", {

 psq_title <- powerscale_plot_quantities(
    ps,
    help_text = TRUE
 )

  psq_notitle <- powerscale_plot_quantities(
    ps,
    help_text = FALSE
  )

  expect_false(is.null(psq_title$labels$title))
  expect_false(is.null(psq_title$labels$subtitle))

  expect_null(psq_notitle$labels$title)
  expect_null(psq_notitle$labels$subtitle)

  psecdf_title <- powerscale_plot_ecdf(ps, variable = "mu")

  psecdf_notitle <- powerscale_plot_ecdf(ps, variable = "mu", help_text = FALSE)


  expect_false(is.null(psecdf_title$labels$title))
  expect_false(is.null(psecdf_title$labels$subtitle))

  expect_null(psecdf_notitle$labels$title)
  expect_null(psecdf_notitle$labels$subtitle)

  psdens_title <- powerscale_plot_dens(ps, variable = "mu")
  psdens_notitle <- powerscale_plot_dens(ps, variable = "mu", help_text = FALSE)

  expect_false(is.null(psdens_title$labels$title))
  expect_false(is.null(psdens_title$labels$subtitle))

  expect_null(psdens_notitle$labels$title)
  expect_null(psdens_notitle$labels$subtitle)
})
