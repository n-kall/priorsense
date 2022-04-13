##' Cumulative Jensen-Shannon divergence
##'
##' Computes the cumulative Jensen-Shannon distance between two
##' samples.
##'
##' The Cumulative Jensen-Shannon distance is a symmetric metric based
##' on the cumulative Jensen-Shannon divergence. The divergence CJS(P || Q) between
##' two cumulative distribution functions P and Q is defined as:
##'
##' \deqn{CJS(P || Q) = \sum P(x) \log \frac{P(x)}{0.5 (P(x) + Q(x))} + \frac{1}{2 \ln 2} \sum (Q(x) - P(x))}
##'
##' The symmetric metric is defined as:
##'
##' \deqn{CJS_{dist}(P || Q) = \sqrt{CJS(P || Q) + CJS(Q || P)}}
##'
##' This has an upper bound of \eqn{\sqrt \sum (P(x) + Q(x))}
##'
##' @param x numeric vector of samples from first distribution
##' @param y numeric vector of samples from second distribution
##' @param x_weights numeric vector of weights of first distribution
##' @param y_weights numeric vector of weights of second distribution
##' @param metric Logical; if TRUE, return square-root of CJS
##' @param symmetric Logical; if TRUE then return max of positive and
##'   negaive CJS
##' @param ... unused
##' @return distance value based on CJS computation.
##' @references Nguyen H-V., Vreeken J. (2015).  Non-parametric
##'   Jensen-Shannon Divergence.  In: Appice A., Rodrigues P., Santos
##'   Costa V., Gama J., Jorge A., Soares C. (eds) Machine Learning
##'   and Knowledge Discovery in Databases.  ECML PKDD 2015. Lecture
##'   Notes in Computer Science, vol 9285.  Springer, Cham.
##'   \code{doi:10.1007/978-3-319-23525-7_11}
##' @export
cjs_dist <- function(x, y, x_weights, y_weights, metric = TRUE, symmetric = TRUE, ...) {

  if (all(is.na(x)) | all(is.na(y)) | (all(y_weights == 0) & !is.null(y_weights))) {

    cjs <- NA

  } else {
    cjs <- .cjs_dist(x, y, x_weights, y_weights, metric, ...)
    if (symmetric) {
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

  if (all(x == y)) {
    # don't use equal bin widths when x and y draws are the same but
    # just different weights
    bins <- x[-length(x)]
    binwidth <- diff(x)
  } else {
    # bins
    nbins <- max(length(x), length(y))
    bins <- seq(
      from = min(min(x), min(y)),
      to = max(max(x), max(y)),
      length.out = nbins
    )
    binwidth <- bins[2] - bins[1]
  }

  # calculate required weighted ecdfs
  Px <- spatstat.geom::ewcdf(x, weights = wp)(bins)
  Qx <- spatstat.geom::ewcdf(y, weights = wq)(bins)

  # calculate integral of ecdfs
  Px_int <- sum(Px * binwidth)
  Qx_int <- sum(Qx * binwidth)

  # calculate cjs
  cjs_PQ <-  sum(binwidth * (
    Px * (log(Px, base = 2) -
            log(0.5 * Px + 0.5 * Qx, base = 2)
    )), na.rm = TRUE) + 0.5 / log(2) * (Qx_int - Px_int)

  cjs_QP <- sum(binwidth * (
    Qx * (log(Qx, base = 2) -
            log(0.5 * Qx + 0.5 * Px, base = 2)
    )), na.rm = TRUE) + 0.5 / log(2) * (Px_int - Qx_int)

  # calculate upper bound
  bound <- Px_int + Qx_int

  # normalise with respect to upper bound
  out <- (cjs_PQ + cjs_QP) / bound

  if (metric) {
    out <- sqrt(out)
  }
  out
}
