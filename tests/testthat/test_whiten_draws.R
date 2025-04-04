test_that("whiten draws returns expected structure", {

  ex_drw <- posterior::example_draws()

  w_drw <- whiten_draws(ex_drw)

  expect_s3_class(w_drw, "whitened_draws")
  expect_s3_class(w_drw, "draws")

  expect_equal(names(attributes(w_drw)), c("names", "row.names", "class", "loadings"))

  expect_true(is.matrix(attr(w_drw, "loadings")))
}
)
