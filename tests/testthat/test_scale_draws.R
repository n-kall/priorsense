test_that("scale draws returns draws object", {

  ex_drw <- posterior::example_draws()

  s_drw <- scale_draws(ex_drw)

  expect_s3_class(s_drw, "draws")

}
)

test_that("scale_draws works on weighted draws without error and preserves weights", {

  ex_drw <- posterior::example_draws()
  weights <- rep(1, posterior::ndraws(ex_drw))
  weighted_drw <- posterior::weight_draws(ex_drw, weights = weights)

  expect_no_error(scale_draws(weighted_drw))

  result <- scale_draws(weighted_drw)
  expect_s3_class(result, "draws")
  expect_false(is.null(stats::weights(result)))

}
)
