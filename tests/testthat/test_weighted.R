x <- c(1, 2, 3, 4, 5)
w <- rep(1, length(x))
w <- w/sum(w)

test_that("weighted quantities work when weights are 1", {
  expect_equal(
    median_weighted(x = x, weights = w),
    c(median = median(x))
  )
  expect_equal(
    mean_weighted(x = x, weights = w),
    c(mean = mean(x))
  )
  expect_equal(
    sd_weighted(x = x, weights = w),
    c(sd = sd(x))
  )
  expect_equal(
    mad_weighted(x = x, weights = w),
    c(mad = mad(x))
  )
  expect_equal(
    var_weighted(x = x, weights = w),
    c(var = var(x))
  )
  expect_equal(
    quantile_weighted(x = x, weights = w),
    posterior::quantile2(x = x)
  )
})
