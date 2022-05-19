##' Weighted quantities
##'
##' Weighted version of common quantities of interest.
##'
##' @param x Numeric vector to calculate quantity from.
##' @param weights Vector of weights corresponding to values in x.
##' @param probs Vector of probabilities for quantiles.
##' @param type Character vector specifying type of quantiles (either
##'   "7" for Type 7 (default) or "hd" for Harrell-Davis)
##' @param ... Currently unused.
##' @return Named vector of calculated quantity.
##' @name weighted_quantities
NULL

##' @export
##' @rdname weighted_quantities
median_weighted <- function(x, weights, ...) {

  weighted_median <- matrixStats::weightedMedian(
    x = x,
    w = weights
  )

  return(c(median = weighted_median))
}


##' @export
##' @rdname weighted_quantities
mad_weighted <- function(x, weights, ...) {

  weighted_mad <- matrixStats::weightedMad(
    x = x,
    w = weights
  )

  return(c(mad = weighted_mad))
}

##' @export
##' @rdname weighted_quantities
var_weighted <- function(x, weights, ...) {

  if (is.null(weights)) {
    var <- var(x)
  } else {
    var <- as.numeric(stats::cov.wt(cbind(as.numeric(x)), weights)$cov)
  }
  return(c(var = var))
}

##' @export
##' @rdname weighted_quantities
sd_weighted <- function(x, weights, ...) {

  if (is.null(weights)) {
    sd <- sd(x)
  } else {
    sd <- as.numeric(sqrt(var_weighted(x, weights)))
  }
  return(c(sd = sd))
}

##' @export
##' @rdname weighted_quantities
mean_weighted  <- function(x, weights, ...) {

  weighted_mean <- matrixStats::weightedMean(
    x = x,
    w = weights
  )

  return(c(mean = weighted_mean))
}

##' Weighted summary measures
##'
##' Returns weighted versions of
##' `posterior::default_summary_measures()` to be used with
##' `posterior::summarise_draws()`.
##' @param x draws object to extract weights from
##' @return Vector of formulas for use with `posterior::summarise_draws()`
##' @export
weighted_summary_measures <- function(x) {
  funcs <- c(
    stats::as.formula(paste0("~mean_weighted(.x, weights(", x, "))")),
    stats::as.formula(paste0("~median_weighted(.x, weights(", x, "))")),
    stats::as.formula(paste0("~sd_weighted(.x, weights(", x, "))")),
    stats::as.formula(paste0("~mad_weighted(.x, weights(", x, "))")),
    stats::as.formula(paste0("~quantile_weighted(.x, weights(", x, "))"))
  )
  return(funcs)
}

## Following is adapted from https://aakinshin.net/posts/weighted-quantiles/
##' @export
##' @rdname weighted_quantities
quantile_weighted <- function(x, weights, probs = c(0.05, 0.95), type = "7", ...) {
  if (type == "7") {
    # Weighted Type 7 quantile estimator
    cdf_fun <- function(n, p) return(function(cdf_probs) {
      h <- p * (n - 1) + 1
      u <- pmax((h - 1) / n, pmin(h / n, cdf_probs))
      u * n - h + 1
    })
  } else if (type == "hd") {
    # Weighted Harrell-Davis quantile estimator
    cdf_fun <- function(n, p) return(function(cdf_probs) {
      stats::pbeta(cdf_probs, (n + 1) * p, (n + 1) * (1 - p))
    })
  }
  quants <- .quantile_weighted(
    x = x,
    weights = weights,
    probs = probs,
    cdf_fun = cdf_fun
  )
  names(quants) <- paste0("q", probs * 100)
  return(quants)
}

.quantile_weighted <- function(x, probs, cdf_fun, weights) {
  # Weighted generic quantile estimator
  n <- length(x)
  if (is.null(weights))
    weights <- rep(1 / n, n)
  nw <- sum(weights)^2 / sum(weights^2) # Kish's effective sample size

  idx <- order(x)
  x <- x[idx]
  weights <- weights[idx]

  weights <- weights / sum(weights)
  cdf_probs <- cumsum(c(0, weights))

  sapply(probs, function(p) {
    cdf <- cdf_fun(nw, p)
    q <- cdf(cdf_probs)
    w <- utils::tail(q, -1) - utils::head(q, -1)
    sum(w * x)
  })
}

##' @export
##' @rdname weighted_quantities
quantile2_weighted <- quantile_weighted
