#' @srrstatsTODO {G5.4} **Correctness tests** *to test that statistical algorithms produce expected results to some fixed test data sets (potentially through comparisons using binding frameworks such as [RStata](https://github.com/lbraglia/RStata)).*
#' @srrstatsTODO {G5.4a} *For new methods, it can be difficult to separate out correctness of the method from the correctness of the implementation, as there may not be reference for comparison. In this case, testing may be implemented against simple, trivial cases or against multiple implementations such as an initial R implementation compared with results from a C/C++ implementation.*

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

#' @srrstats {G5.8, G5.8b} complex input should give error
test_that("cjs errors with complex and character input", {
  expect_error(
    cjs_dist(complex(1, 1, 1), complex(1, 1, 1))
  )
  expect_error(
    cjs_dist(c("a", "a", "a"), c("b", "b", "b"))
  )
}
)
