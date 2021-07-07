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
##' @param ... unused
##' @return distance value based on CJS computation.
##' @references Nguyen H-V., Vreeken J. (2015).  Non-parametric
##'   Jensen-Shannon Divergence.  In: Appice A., Rodrigues P., Santos
##'   Costa V., Gama J., Jorge A., Soares C. (eds) Machine Learning
##'   and Knowledge Discovery in Databases.  ECML PKDD 2015. Lecture
##'   Notes in Computer Science, vol 9285.  Springer, Cham.
##'   \code{doi:10.1007/978-3-319-23525-7_11}
##' @export
cjs_dist <- function(x, y, x_weights, y_weights, ...) {

  # sort draws and weights
  x_idx <- order(x)
  x <- x[x_idx]
  wp <- x_weights[x_idx]

  y_idx <- order(y)
  y <- y[y_idx]
  wq <- y_weights[y_idx]

  # add end point of final step
  x_v <- x[length(x)] + x[length(x)] - x[length(x) - 1]
  y_v <- y[length(y)] + y[length(y)] - y[length(y) - 1]

  # calculate widths of each step
  x_diff <- diff(c(x, x_v))
  y_diff <- diff(c(y, y_v))

  # calculate required weighted ecdfs
  Px <- spatstat.geom::ewcdf(x, weights = wp)(x)
  Qx <- spatstat.geom::ewcdf(y, weights = wq)(x)

  Py <- spatstat.geom::ewcdf(x, weights = wp)(y)
  Qy <- spatstat.geom::ewcdf(y, weights = wq)(y)

  # calculate integral of ecdfs
  Px_int <- drop(Px %*% x_diff)
  Qx_int <- drop(Qx %*% x_diff)

  Py_int <- drop(Py %*% y_diff)
  Qy_int <- drop(Qy %*% y_diff)

  # calculate cjs
  cjs_PQ <-  x_diff %*% (
    Px * (log(Px, base = 2) -
            log(0.5 * Px + 0.5 * Qx, base = 2)
    )
  ) + 0.5 / log(2) * (Qx_int - Px_int)

  cjs_QP <- y_diff %*% (
    Qy * (log(Qy, base = 2) -
            log(0.5 * Qy + 0.5 * Py, base = 2)
    )
  ) + 0.5 / log(2) * (Py_int - Qy_int)
  
  # calculate upper bound
  bound <- Px_int + Qy_int
  
  # normalise with respect to upper bound
  out <- (sqrt(cjs_PQ + cjs_QP)) / sqrt(bound)

  return(c(cjs = out))
}
