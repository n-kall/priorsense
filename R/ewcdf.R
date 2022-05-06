# code adapted from spatstat.geom (c) Adrian Baddeley, Rolf Turner, Ege Rubak
ewcdf <- function(x, weights = NULL, normalise = TRUE, adjust = 1)
{
  nx <- length(x)
  nw <- length(weights)
  weighted <- (nw > 0)

  if(weighted) {
    check.nvector(weights, things = "entries of x", oneok = TRUE, vname = "weights")
    stopifnot(all(weights >= 0))
    if(nw == 1)
      weights <- rep(weights, nx)
  }

  ## remove NA's
  nbg <- is.na(x)
  x <- x[!nbg]
  if(weighted) weights <- weights[!nbg]
  n <- length(x)
  if (n < 1)
    stop("'x' must have 1 or more non-missing values")

  ## sort in increasing order of x value
  if(!weighted) {
    x <- sort(x)
    w <- rep(1, n)
  } else {
    ox <- fave.order(x)
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
    wmatch <- .C(
      SG <- tabsumweight,
      nx = as.integer(n),
      x = as.double(x),
      w = as.double(w),
      nv = as.integer(nv),
      v = as.double(vals),
      z = as.double(numeric(nv)),
      PACKAGE = "priorsense"
    )$z
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
  rval <- approxfun(
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
