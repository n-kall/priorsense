# code adapted from spatstat.geom (c) Adrian Baddeley, Rolf Turner, Ege Rubak
ewcdf <- function(x, weights = NULL, normalise = TRUE, adjust = 1)
{
  nx <- length(x)
  nw <- length(weights)
  weighted <- (nw > 0)

  ## sort in increasing order of x value
  if(!weighted) {
    x <- sort(x)
    w <- rep(1, nx)
  } else {
    ox <- sort.list(x, method = "quick", na.last = NA)
    x <- x[ox]
    w <- weights[ox]
  }
  ## find jump locations and match
  rl <- rle(x)
  vals <- rl$values
  if(!weighted) {
    wmatch <- rl$lengths
  } else {
    nv <- length(vals)    
    wmatch <- tabsumweight(x, w)
  }
  ## cumulative weight in each interval
  cumwt <- cumsum(wmatch)
  totwt <- sum(wmatch)
  ## rescale ?
  if(normalise) {
    cumwt <- cumwt/totwt
    totwt <- 1
  } else if(adjust != 1) {
    cumwt <- adjust * cumwt
    totwt <- adjust * totwt
  }
  ## make function
  rval <- stats::approxfun(
    vals, cumwt,
    method = "constant", yleft = 0, yright = totwt,
    f = 0, ties = "ordered"
  )
  class(rval) <- c("ewcdf",
                   if(normalise) "ecdf" else NULL,
                   "stepfun", class(rval))
  assign("w", w, envir = environment(rval))
  attr(rval, "call") <- sys.call()
  return(rval)
}

tabsumweight <- function(x, w) {
  v <- unique(sort(x))
  nx <- length(x)
  nv <- length(v)
  out <- rep(0, times = nv)
  for (xi in x) {
    vi <- min(which(v >= xi))
    out[vi] <- out[vi] + w[vi]
  }
  out
}
