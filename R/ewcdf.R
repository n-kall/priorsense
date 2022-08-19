# code adapted from spatstat.geom (c) Adrian Baddeley, Rolf Turner, Ege Rubak
ewcdf <- function(x, weights = NULL) {
  x_idx <- order(x)
  x <- x[x_idx]
  weights <- weights[x_idx]

  nw <- length(weights)
  weighted <- (nw > 0)

  rl <- rle(x)
  vals <- rl$values
  if (!weighted) {
    wmatch <- rl$lengths
  } else {
    wmatch <- tabsumweight(x, weights)
  }
  ## cumulative weight in each interval
  cumwt <- cumsum(wmatch)
  totwt <- sum(wmatch)
  ## rescale
  cumwt <- cumwt / totwt
  totwt <- 1
  ## make function
  rval <- stats::approxfun(
    vals, cumwt,
    method = "constant", yleft = 0, yright = totwt,
    f = 0, ties = "ordered"
  )
  class(rval) <- c("ewcdf",
                   "ecdf",
                   "stepfun", class(rval))
  assign("weights", weights, envir = environment(rval))
  attr(rval, "call") <- sys.call()
  return(rval)
}

tabsumweight <- function(x, weights) {
  v <- unique(sort(x))
  nv <- length(v)
  out <- rep(0, times = nv)
  for (xi in x) {
    vi <- min(which(v >= xi))
    out[vi] <- out[vi] + weights[vi]
  }
  return(out)
}
