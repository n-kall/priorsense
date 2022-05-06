##' Weighted quantities
##'
##' Weighted version of common quantities of interest.
##'
##' @param x Numeric vector to calculate quantity from.
##' @param weights Vector of weights corresponding to values in x.
##' @param probs Vector of probabilities for quantiles.
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

  weighted_var <- Hmisc::wtd.var(
    x = x,
    weights = weights,
    normwt = TRUE
  )

  return(c(var = weighted_var))
}

##' @export
##' @rdname weighted_quantities
sd_weighted <- function(x, weights, ...) {

  weighted_sd <- sqrt(Hmisc::wtd.var(
    x = x,
    weights = weights,
    normwt = TRUE
  ))

  return(c(sd = weighted_sd))
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

##' @export
##' @rdname weighted_quantities
quantile_weighted <- function(x, weights, probs = c(0.05, 0.95), ...) {

  quants <- wtd.quantile(
    x = x,
    weights = weights,
    probs = probs,
    normwt = TRUE
  )

  names(quants) <- paste0("q", probs * 100)

  return(quants)
}

##' @export
##' @rdname weighted_quantities
quantile2_weighted <- quantile_weighted

##' Importance sampling effective sample size diagnostic for
##' computing the mean of a parameter
##' @param x vector of values
##' @param weights vector of weights
##' @param ... unused
##' @return numeric
n_eff_mean <- function(x, weights, ...) {
  # TODO: can we pass raw weights here (non-Pareto smoothed)?
  lwf_mean <- c(log(weights) + log(abs(x)))
  lwf_mean <- lwf_mean - matrixStats::logSumExp(lwf_mean)
  n_eff <- 1.0 / sum(exp(2 * lwf_mean))

  return(c(n_eff_mean = n_eff))
}

##' Importance sampling effective sample size diagnostic for
##' computing the variance of a parameter
##' @param x vector of values
##' @param weights vector of weights
##' @param ... unused
##' @return numeric
n_eff_var <- function(x, weights, ...) {
  # TODO: can we pass raw weights here (non-Pareto smoothed)?
  lwf_var <- c(log(weights) + log(abs(x^2)))
  lwf_var <- lwf_var - matrixStats::logSumExp(lwf_var)
  n_eff <- 1.0 / sum(exp(2 * lwf_var))

  return(c(n_eff_var = n_eff))
}

##' Pareto-k diagnostic for computing the mean of a parameter
##' @param x vector of values
##' @param weights vector of weights
##' @param ... unused
##' @return numeric
pareto_k_mean <- function(x, weights, ...) {
  # TODO: can we pass raw weights here (non-Pareto smoothed)?
  psis_f <- SW(loo::psis(
      log_ratios = c(log(weights) + log(abs(x))),
      r_eff = 1)
      )

  return(c(pareto_k_mean = psis_f$diagnostics$pareto_k))
}

##' Pareto-k diagnostic for computing the variance of a parameter
##' @param x vector of values
##' @param weights vector of weights
##' @param ... unused
##' @return numeric
pareto_k_var <- function(x, weights, ...) {
  # TODO: can we pass raw weights here (non-Pareto smoothed)?
  psis_f <- SW(loo::psis(
    log_ratios = c(log(weights) + log(abs(x^2))),
    r_eff = 1)
    )

  return(c(pareto_k_var = psis_f$diagnostics$pareto_k))
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


# code from Hmisc (c) Frank E Harrell Jr
wtd.quantile <- function (x, weights = NULL, probs = c(0, 0.25, 0.5, 0.75, 1),
    type = c("quantile", "(i-1)/(n-1)", "i/(n+1)", "i/n"), normwt = FALSE,
    na.rm = TRUE)
{
    if (!length(weights))
        return(quantile(x, probs = probs, na.rm = na.rm))
    type <- match.arg(type)
    if (any(probs < 0 | probs > 1))
        stop("Probabilities must be between 0 and 1 inclusive")
    nams <- paste(format(round(probs * 100, if (length(probs) >
        1) 2 - log10(diff(range(probs))) else 2)), "%", sep = "")
    i <- is.na(weights) | weights == 0
    if (any(i)) {
        x <- x[!i]
        weights <- weights[!i]
    }
    if (type == "quantile") {
        w <- Hmisc::wtd.table(x, weights, na.rm = na.rm, normwt = normwt,
            type = "list")
        x <- w$x
        wts <- w$sum.of.weights
        n <- sum(wts)
        order <- 1 + (n - 1) * probs
        low <- pmax(floor(order), 1)
        high <- pmin(low + 1, n)
        order <- order%%1
        allq <- approx(cumsum(wts), x, xout = c(low, high), method = "constant",
            f = 1, rule = 2)$y
        k <- length(probs)
        quantiles <- (1 - order) * allq[1:k] + order * allq[-(1:k)]
        names(quantiles) <- nams
        return(quantiles)
    }
    w <- wtd.Ecdf(x, weights, na.rm = na.rm, type = type, normwt = normwt)
    structure(approx(w$ecdf, w$x, xout = probs, rule = 2)$y,
        names = nams)
}

wtd.var <- function (x, weights = NULL, normwt = FALSE, na.rm = TRUE, method = c("unbiased",
    "ML"))
{
    method <- match.arg(method)
    if (!length(weights)) {
        if (na.rm)
            x <- x[!is.na(x)]
        return(var(x))
    }
    if (na.rm) {
        s <- !is.na(x + weights)
        x <- x[s]
        weights <- weights[s]
    }
    if (normwt)
        weights <- weights * length(x)/sum(weights)
    if (normwt || method == "ML")
        return(as.numeric(stats::cov.wt(cbind(x), weights, method = method)$cov))
    sw <- sum(weights)
    if (sw <= 1)
        warning("only one effective observation; variance estimate undefined")
    xbar <- sum(weights * x)/sw
    sum(weights * ((x - xbar)^2))/(sw - 1)
}
