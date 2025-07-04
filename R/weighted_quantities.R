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
##' @keywords internal
##' @srrstats {G2.14, G2.14a, G2.15} weighted quantities specified to
##'   error on missing data, as this should not occur
##' @srrstats {G2.6} Inputs to these internal functions are 1-d draws
##'   objects
##' @noRd
NULL

median_weighted <- function(x, weights, ...) {

  checkmate::assert_numeric(x, min.len = 1, any.missing = FALSE)

  checkmate::assert_numeric(weights, len = length(x),
                            null.ok = TRUE, any.missing = FALSE)

  x <- as.numeric(x)
  weights <- as.numeric(weights)

  weighted_median <- matrixStats::weightedMedian(
    x = x,
    w = weights
  )

  return(c(median = weighted_median))
}

mad_weighted <- function(x, weights, ...) {

  checkmate::assert_numeric(x, min.len = 1, any.missing = FALSE)

  checkmate::assert_numeric(weights, len = length(x),
                            null.ok = TRUE, any.missing = FALSE)

  x <- as.numeric(x)
  weights <- as.numeric(weights)

  weighted_mad <- matrixStats::weightedMad(
    x = x,
    w = weights
  )

  return(c(mad = weighted_mad))
}

var_weighted <- function(x, weights, ...) {

  checkmate::assert_numeric(x, min.len = 1, any.missing = FALSE)

  checkmate::assert_numeric(weights, len = length(x), null.ok = TRUE,
                            any.missing = FALSE)

  x <- as.numeric(x)
  weights <- as.numeric(weights)

  if (is.null(weights)) {
    var <- var(x)
  } else {
    var <- as.numeric(stats::cov.wt(cbind(as.numeric(x)), weights)$cov)
  }
  return(c(var = var))
}

sd_weighted <- function(x, weights, ...) {

  checkmate::assert_numeric(x, min.len = 1, any.missing = FALSE)

  checkmate::assert_numeric(weights, len = length(x), null.ok = TRUE,
                            any.missing = FALSE)

  x <- as.numeric(x)
  weights <- as.numeric(weights)

  if (is.null(weights)) {
    sd <- sd(x)
  } else {
    sd <- as.numeric(sqrt(var_weighted(x, weights)))
  }
  return(c(sd = sd))
}

mean_weighted  <- function(x, weights, ...) {

  checkmate::assert_numeric(x, min.len = 1, any.missing = FALSE)

  checkmate::assert_numeric(weights, len = length(x), null.ok = TRUE,
                            any.missing = FALSE)

  x <- as.numeric(x)
  weights <- as.numeric(weights)

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
##' @return Vector of formulas for use with
##'   `posterior::summarise_draws()`
##' @keywords internal
##' @noRd
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

quantile_weighted <- function(x, weights, probs = c(0.05, 0.95),
                              type = "7", ...) {

  checkmate::assert_numeric(x, min.len = 1, any.missing = FALSE)
  checkmate::assert_numeric(weights, len = length(x), null.ok = TRUE,
                            any.missing = FALSE)

  checkmate::assert_numeric(probs, null.ok = FALSE, any.missing = FALSE)

  checkmate::assert_choice(type, c("7", "hd"), null.ok = FALSE)

  ## Following is adapted from Andrey Akinshin (2023) "Weighted
  ## quantile estimators" arXiv:2304.07265 [stat.ME]
  if (type == "7") {
    # Weighted Type 7 quantile estimator
    cdf_fun <- function(n, p) {
      return(function(cdf_probs) {
        h <- p * (n - 1) + 1
        u <- pmax((h - 1) / n, pmin(h / n, cdf_probs))
        u * n - h + 1
      })
    }
  } else if (type == "hd") {
    # Weighted Harrell-Davis quantile estimator
    cdf_fun <- function(n, p) {
      return(function(cdf_probs) {
        stats::pbeta(cdf_probs, (n + 1) * p, (n + 1) * (1 - p))
      })
    }
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
##' calculation of weighted quantile
##' @param x numeric vector of draws
##' @param probs numeric vector specifying probabilities
##' @param cdf_fun cumulative distribution function
##' @param weights numeric vector of weights
##' @param type type of quantile calculation
##' @param ... unused
##' @return vector of quantiles
##' @noRd
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

  vapply(probs,
         function(p) {
           cdf <- cdf_fun(nw, p)
           q <- cdf(cdf_probs)
           w <- utils::tail(q, -1) - utils::head(q, -1)
           sum(w * x)
         },
         FUN.VALUE = c(1))
}

quantile2_weighted <- quantile_weighted

# always use quantile2 internally
quantile <- posterior::quantile2
