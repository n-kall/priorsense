x <- c(1, 2, 3, 4)
y <- c(4, 5, 6, 7)
w <- c(0.1, 0.2, 0.3, 0.4)


test_that("cjs works with one weighted", {
  expect_equal(
    cjs_dist(x, x, NULL, w),
    cjs_dist(x, x, w, NULL)
  )
})

test_that("cjs works with different x and y", {
  expect_equal(
    cjs_dist(x, y, NULL, NULL),
    c(cjs = 0.5),
    tolerance = 0.1
  )
})

test_that("cjs returns zero for same x and y", {
  expect_equal(
    cjs_dist(x, x, NULL, NULL),
    c(cjs = 0)
  )
  expect_equal(
    cjs_dist(x, x, w, w),
    c(cjs = 0)
  )
})
