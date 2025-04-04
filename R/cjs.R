##' Cumulative Jensen-Shannon divergence
##'
##' Computes the cumulative Jensen-Shannon distance between two
##' samples.
##'
##' The Cumulative Jensen-Shannon distance is a symmetric metric based
##' on the cumulative Jensen-Shannon divergence. The divergence CJS(P || Q)
##' between two cumulative distribution functions P and Q is defined as:
##'
##' \deqn{CJS(P || Q) = \sum P(x) \log \frac{P(x)}{0.5 (P(x) + Q(x))} +
##' \frac{1}{2 \ln 2} \sum (Q(x) - P(x))}
##'
##' The symmetric metric is defined as:
##'
##' \deqn{CJS_{dist}(P || Q) = \sqrt{CJS(P || Q) + CJS(Q || P)}}
##'
##' This has an upper bound of \eqn{\sqrt{ \sum (P(x) + Q(x))}}
##'
##' @param x numeric vector of samples from first distribution
##' @param y numeric vector of samples from second distribution
##' @param x_weights numeric vector of weights of first distribution
##' @param y_weights numeric vector of weights of second distribution
##' @param metric Logical; if TRUE, return square-root of CJS
##' @param unsigned Logical; if TRUE then return max of CJS(P(x) ||
##'   Q(x)) and CJS(P(-x) || Q(-x)). This ensures invariance to
##'   transformations such as PCA.
##' @param ... unused
##' @return distance value based on CJS computation.
##' @references Nguyen H-V., Vreeken J. (2015).  Non-parametric
##'   Jensen-Shannon Divergence.  In: Appice A., Rodrigues P., Santos
##'   Costa V., Gama J., Jorge A., Soares C. (eds) Machine Learning
##'   and Knowledge Discovery in Databases.  ECML PKDD 2015. Lecture
##'   Notes in Computer Science, vol 9285.  Springer, Cham.
##'   \code{doi:10.1007/978-3-319-23525-7_11}
##' @examples
##' x <- rnorm(100)
##' y <- rnorm(100, 2, 2)
##' cjs_dist(x, y, x_weights = NULL, y_weights = NULL)
##' @export
cjs_dist <- function(x,
                     y,
                     x_weights = NULL,
                     y_weights = NULL,
                     metric = TRUE,
                     unsigned = TRUE,
                     ...) {

  checkmate::assert_numeric(x, min.len = 1)
  checkmate::assert_numeric(y, min.len = 1)
  checkmate::assert_numeric(x_weights, len = length(x), null.ok = TRUE)
  checkmate::assert_numeric(y_weights, len = length(y), null.ok = TRUE)
  checkmate::assert_logical(metric, len = 1)
  checkmate::assert_logical(unsigned, len = 1)

  if (
    all(is.na(x)) ||
      all(is.na(y)) ||
      (all(y_weights == 0) && !is.null(y_weights))
  ) {
    cjs <- NA
  } else if (identical(x, y) && identical(x_weights, y_weights)) {
    cjs <- 0
  } else {
    cjs <- .cjs_dist(x, y, x_weights, y_weights, metric, ...)
    if (unsigned) {
      cjsm <- .cjs_dist(-x, -y, x_weights, y_weights, metric, ...)
      cjs <- max(cjs, cjsm)
    }
  }
  return(c(cjs = cjs))
}

.cjs_dist <- function(x, y, x_weights, y_weights, metric = TRUE, ...) {
  # sort draws and weights
  x_idx <- order(x)
  x <- x[x_idx]
  wp <- x_weights[x_idx]

  y_idx <- order(y)
  y <- y[y_idx]
  wq <- y_weights[y_idx]

  if (is.null(wp)) {
    wp <- rep(1 / length(x), length(x))
  }
  if (is.null(wq)) {
    wq <- rep(1 / length(y), length(y))
  }

  if (identical(x, y)) {
    # if all x and y are same, but y is a weighted version of x
    # calculate weighted ecdf via cumsum of weights and use natural
    # bins from stepfun
    bins <- x[-length(x)]
    binwidth <- diff(x)
    px <- cumsum(wp / sum(wp))
    px <- px[-length(px)]
    qx <- cumsum(wq / sum(wq))
    qx <- qx[-length(qx)]
  } else {
    # otherwise the draws are not the same (e.g. resampled) we use
    # approximation with bins and ewcdf. There is a slight bias in
    # this case which overestimates the cjs compared to weighted
    # version
    nbins <- max(length(x), length(y))
    bins <- seq(
      from = min(min(x), min(y)),
      to = max(max(x), max(y)),
      length.out = nbins
    )
    binwidth <- bins[2] - bins[1]

    # calculate required weighted ecdfs
    px <- ewcdf(x, wp)(bins)
    qx <- ewcdf(y, wq)(bins)
  }

  # calculate integral of ecdfs
  px_int <- sum(px * binwidth)
  qx_int <- sum(qx * binwidth)

  # calculate cjs
  cjs_pq <-  sum(binwidth * (
    px * (log(px, base = 2) -
            log(0.5 * px + 0.5 * qx, base = 2)
    )), na.rm = TRUE) + 0.5 / log(2) * (qx_int - px_int)

  cjs_qp <- sum(binwidth * (
    qx * (log(qx, base = 2) -
            log(0.5 * qx + 0.5 * px, base = 2)
    )), na.rm = TRUE) + 0.5 / log(2) * (px_int - qx_int)

  # calculate upper bound
  bound <- px_int + qx_int

  # normalise with respect to upper bound
  out <- (cjs_pq + cjs_qp) / bound

  if (metric) {
    out <- sqrt(out)
  }
  return(out)
}
