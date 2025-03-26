test_that("scale draws returns draws object", {

  ex_drw <- posterior::example_draws()

  s_drw <- scale_draws(ex_drw)

  expect_s3_class(s_drw, "draws")

}
)
