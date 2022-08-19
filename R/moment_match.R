moment_match <- function(x, ...) {
  UseMethod("moment_match")
}

moment_match.powerscaling_data <- function(x, ...) {

  out <- moment_match(
    x = x$fit,
    ...
  )
  out
}

moment_match.stanfit <- function(x, psis, ...) {
  # TODO: ensure compatibility with objects not created in the current R session
  post_draws <- function(x, ...) {
    # ensure additional arguments are not passed further
    as.matrix(x)
  }
  out <- moment_match.default(
    x,
    psis = psis, post_draws = post_draws,
    unconstrain_pars = unconstrain_pars.stanfit,
    log_prob_upars = log_prob_upars.stanfit,
    log_ratio_upars = log_ratio_upars.stanfit,
    nchains = posterior::nchains(posterior::as_draws(x)),
    ...
  )
  out
}

# adapted from original moment matching implementation by Topi Paananen
moment_match.default <- function(
                                 x,
                                 psis,
                                 post_draws,
                                 unconstrain_pars,
                                 log_prob_upars,
                                 log_ratio_upars,
                                 nchains,
                                 max_iters = 30,
                                 k_threshold = 0.5,
                                 cov_transform = TRUE,
                                 ...) {

  # input checks
  checkmate::assertClass(psis, classes = "psis")
  checkmate::assertFunction(post_draws)
  checkmate::assertFunction(unconstrain_pars)
  checkmate::assertFunction(log_prob_upars)
  checkmate::assertFunction(log_ratio_upars)
  checkmate::assertNumber(max_iters)
  checkmate::assertNumber(k_threshold)
  checkmate::assertLogical(cov_transform)

  k <- psis$diagnostics$pareto_k
  stopifnot(length(k) == 1)
  if (k <= k_threshold) {
    return(list(x = x, importance_sampling = psis))
  }

  S <- dim(psis)[1]
  pars <- post_draws(x, ...)
  # transform the model parameters to unconstrained space
  upars <- unconstrain_pars(x, pars = pars, ...)
  # number of parameters in the **parameters** block only
  npars <- ncol(upars)
  # if more parameters than samples, do not do Cholesky transformation
  cov_transform <- cov_transform && S >= 10 * npars
  # compute log-probabilities of the original parameter values
  orig_log_prob <- log_prob_upars(x, upars = upars, ...)
  # obtain original weights
  lw <- as.vector(stats::weights(psis))

  # include information about number of MCMC chains
  r_eff <- loo::relative_eff(
    1 / exp(lw),
    chain_id = rep(1:nchains, each = S / nchains)
  )

  # try several transformations one by one
  # if one does not work, do not apply it and try another one
  # to accept the transformation, Pareto k needs to improve
  # when transformation succeeds, start again from the first one
  iterind <- 1
  while (iterind <= max_iters && k > k_threshold) {
    if (iterind == max_iters) {
      warning(
        "The maximum number of moment matching iterations ('max_iters' ",
        "argument) was reached. Increasing the value may improve accuracy."
      )
    }

    # 1. match means
    trans <- shift(x, upars, lw)
    quantities <- update_quantities(
      x,
      upars = trans$upars,
      orig_log_prob = orig_log_prob,
      log_prob_upars = log_prob_upars,
      log_ratio_upars = log_ratio_upars,
      r_eff = r_eff, ...
    )
    if (quantities$k < k) {
      upars <- trans$upars

      lw <- quantities$lw
      k <- quantities$k
      iterind <- iterind + 1
      next
    }

    # 2. match means and marginal variances
    trans <- shift_and_scale(x, upars, lw)
    quantities <- update_quantities(
      x,
      upars = trans$upars,
      orig_log_prob = orig_log_prob,
      log_prob_upars = log_prob_upars,
      log_ratio_upars = log_ratio_upars,
      r_eff = r_eff, ...
    )
    if (quantities$k < k) {
      upars <- trans$upars

      lw <- quantities$lw
      k <- quantities$k
      iterind <- iterind + 1
      next
    }

    # 3. match means and covariances
    if (cov_transform) {
      trans <- shift_and_cov(x, upars, lw)
      quantities <- update_quantities(
        x,
        upars = trans$upars,
        orig_log_prob = orig_log_prob,
        log_prob_upars = log_prob_upars,
        log_ratio_upars = log_ratio_upars,
        r_eff = r_eff, ...
      )
      if (quantities$k < k) {
        upars <- trans$upars

        lw <- quantities$lw
        k <- quantities$k
        iterind <- iterind + 1
        next
      }
    }
    # none of the transformations improved khat
    # so there is no need to try further
    break
  }

  if (k > k_threshold) {
    warning("The Pareto k diagnostic value is still too high after moment matching.")
  }

  # update results
  n_eff <- 1.0 / sum(exp(2 * lw)) * r_eff
  psis$log_weights <- as.matrix(lw)
  psis$diagnostics$pareto_k <- k
  psis$diagnostics$n_eff <- n_eff

  class(psis) <- c("psis", "importance_sampling")
  x <- update_pars(x, upars = upars, ...)
  list(x = x, importance_sampling = psis)
}

# update pareto-k values and psis weights
update_quantities <- function(x, upars, orig_log_prob,
                              log_prob_upars, log_ratio_upars,
                              r_eff, ...) {
  log_prob_new <- log_prob_upars(x, upars = upars, ...)
  log_ratio_new <- log_ratio_upars(x, upars = upars, ...)

  # compute new log importance weights
  psis_new <- SW(
    loo::psis(
      log_ratio_new + log_prob_new - orig_log_prob,
      r_eff = r_eff,
      )
  )
  lw_new <- as.vector(stats::weights(psis_new))
  k_new <- psis_new$diagnostics$pareto_k

  # gather results
  list(
    lw = lw_new,
    k = k_new
  )
}

#' Shift a matrix of parameters to their weighted mean.
#' Also calls update_quantities which updates the importance weights based on
#' the supplied model object.
#'
#' @noRd
#' @param x A fitted model object.
#' @param upars A matrix representing a sample of vector-valued parameters in
#' the unconstrained space
#' @param lw A vector representing the log-weight of each parameter
#' @return List with the shift that was performed, and the new parameter matrix.
#'
shift <- function(x, upars, lw, ...) {
  w <- exp(lw) * length(lw)
  # compute moments using log weights
  vars_original <- matrixStats::colVars(upars)
  vars_weighted <- matrixStats::colWeightedVars(upars, w = w)
  if (all(vars_original > 1e-12) && all(vars_weighted > 1e-12)) {
    scaling <- sqrt(vars_weighted / vars_original)

    mean_original <- colMeans(upars)
    mean_weighted <- matrixStats::colWeightedMeans(upars, w = w)

    upars_new <- sweep(upars, 2, mean_original, "-")
    upars_new <- sweep(upars_new, 2, scaling, "*")
    upars_new <-
      sweep(upars_new, 2, mean_weighted, "+")
  } else {
    upars_new <- upars
  }
  list(
    upars = upars_new
  )
}

#' Shift a matrix of parameters to their weighted mean and scale the marginal
#' variances to match the weighted marginal variances. Also calls
#' update_quantities which updates the importance weights based on
#' the supplied model object.
#'
#' @noRd
#' @param x A fitted model object.
#' @param upars A matrix representing a sample of vector-valued parameters in
#' the unconstrained space
#' @param lw A vector representing the log-weight of each parameter
#' @return List with the shift and scaling that were performed, and the new
#' parameter matrix.
#'
#'
shift_and_scale <- function(x, upars, lw, ...) {
  w <- exp(lw) * length(lw)
  # compute moments using log weights
  vars_original <- matrixStats::colVars(upars)
  vars_weighted <- matrixStats::colWeightedVars(upars, w = w)
  if (all(vars_original > 1e-12) && all(vars_weighted > 1e-12)) {
    scaling <- sqrt(vars_weighted / vars_original)

    mean_original <- colMeans(upars)
    mean_weighted <- matrixStats::colWeightedMeans(upars, w = w)

    upars_new <- sweep(upars, 2, mean_original, "-")
    upars_new <- sweep(upars_new, 2, scaling, "*")
    upars_new <- sweep(upars_new, 2, mean_weighted, "+")
  } else {
    upars_new <- upars
  }
  list(
    upars = upars_new
  )
}

#' Shift a matrix of parameters to their weighted mean and scale the covariance
#' to match the weighted covariance.
#' Also calls update_quantities which updates the importance weights based on
#' the supplied model object.
#'
#' @noRd
#' @param x A fitted model object.
#' @param upars A matrix representing a sample of vector-valued parameters in
#' the unconstrained space
#' @param lw A vector representing the log-weight of each parameter
#' @return List with the shift and mapping that were performed, and the new
#' parameter matrix.
#'
shift_and_cov <- function(x, upars, lw, ...) {
  w <- exp(lw) * length(lw)
  # compute moments using log weights
  covar_original <- stats::cov(upars)
  covar_weighted <- stats::cov.wt(upars, wt = w)$cov
  chol1 <- tryCatch(
  {
    chol(covar_weighted)
  },
  error = function(cond) {
    return(NULL)
  }
  )
  chol2 <- tryCatch(
  {
    chol(covar_original)
  },
  error = function(cond) {
    return(NULL)
  }
  )
  if (is.null(chol1) || is.null(chol2)) {
    upars_new <- upars
    mapping <- diag(ncol(upars))
  } else {
    mapping <- t(chol1) %*% solve(t(chol2))

    mean_original <- colMeans(upars)
    mean_weighted <- matrixStats::colWeightedMeans(upars, w = w)

    # transform posterior upars
    upars_new <- sweep(upars, 2, mean_original, "-")
    upars_new <- tcrossprod(upars_new, mapping)
    upars_new <- sweep(upars_new, 2, mean_weighted, "+")
    colnames(upars_new) <- colnames(upars)
  }

  list(
    upars = upars_new
  )
}

# transform parameters to the unconstrained space
unconstrain_pars <- function(x, ...) {
  UseMethod("unconstrain_pars")
}

unconstrain_pars.stanfit <- function(x, pars, ...) {
  skeleton <- .create_skeleton(x@sim$pars_oi, x@par_dims[x@sim$pars_oi])
  upars <- apply(pars, 1, FUN = function(theta) {
    rstan::unconstrain_pars(x, pars = .rstan_relist(theta, skeleton))
  })
  # for one parameter models
  if (is.null(dim(upars))) {
    dim(upars) <- c(1, length(upars))
  }
  t(upars)
}


# compute log_prob for each posterior draws on the unconstrained space
log_prob_upars <- function(x, ...) {
  UseMethod("log_prob_upars")
}

log_prob_upars.stanfit <- function(x, upars, ...) {
  apply(upars, 1, rstan::log_prob,
        object = x,
        adjust_transform = TRUE, gradient = FALSE
        )
}

update_pars <- function(x, ...) {
  UseMethod("update_pars")
}

# update fit object according to new parameter values
update_pars.stanfit <- function(x, upars, save_old_pars = TRUE, ...) {

  # save originals
  if (save_old_pars == TRUE) {
    x@sim$pars_oi_old <- x@sim$pars_oi
    x@sim$dims_oi_old <- x@sim$dims_oi
    x@sim$fnames_oi_old <- x@sim$fnames_oi
  }
  # list with one element per posterior draw
  pars <- apply(upars, 1, rstan::constrain_pars, object = x)
  parnames <- rep(names(pars[[1]]), lengths(pars[[1]]))
  # transform samples
  nsamples <- length(pars)
  pars <- unlist(pars)
  npars <- length(pars) / nsamples
  dim(pars) <- c(npars, nsamples)
  rownames(pars) <- parnames
  # lp__ is not computed automatically
  lp__ <- log_prob_upars(x, upars = upars)
  pars <- rbind(pars, lp__ = lp__)
  # bring samples into the right structure
  new_samples <- named_list(x@sim$fnames_oi_old, list(numeric(nsamples)))
  new_parnames <- sub("\\[.+", "", names(new_samples))
  new_parnames_unique <- unique(new_parnames)
  for (p in new_parnames_unique) {
    sub_pars <- pars[rownames(pars) == p, , drop = FALSE]
    sel <- which(new_parnames == p)
    for (i in seq_along(sel)) {
      new_samples[[sel[i]]] <- sub_pars[i, ]
    }
  }
  # create new sim object to overwrite x@sim
  x@sim <- list(
    samples = list(new_samples),
    iter = nsamples,
    thin = 1,
    warmup = 0,
    chains = 1,
    n_save = nsamples,
    warmup2 = 0,
    permutation = list(seq_len(nsamples)),
    pars_oi = x@sim$pars_oi_old,
    dims_oi = x@sim$dims_oi_old,
    fnames_oi = x@sim$fnames_oi_old,
    n_flatnames = length(x@sim$fnames_oi_old)
  )
  x@stan_args <- list(
    list(chain_id = 1, iter = nsamples, thin = 1, warmup = 0)
  )
  x
}

# compute log_ratio values based on the unconstrained parameters
log_ratio_upars <- function(x, ...) {
  UseMethod("log_ratio_upars")
}

log_ratio_upars.stanfit <- function(x, upars, component_fn, ...) {
  x <- update_pars(x, upars = upars, ...)
  component_draws <- component_fn(x)
  scaled_log_ratio(component_draws, ...)
}

# -------- will be imported from rstan at some point -------
# create a named list of draws for use with rstan methods
.rstan_relist <- function(x, skeleton) {
  out <- utils::relist(x, skeleton)
  for (i in seq_along(skeleton)) {
    dim(out[[i]]) <- dim(skeleton[[i]])
  }
  out
}

# rstan helper function to get dims of parameters right
.create_skeleton <- function(pars, dims) {
  out <- lapply(seq_along(pars), function(i) {
    len_dims <- length(dims[[i]])
    if (len_dims < 1) {
      return(0)
    }
    return(array(0, dim = dims[[i]]))
  })
  names(out) <- pars
  out
}

# initialize a named list
# @param names names of the elements
# @param values optional values of the elements
named_list <- function(names, values = NULL) {
  if (!is.null(values)) {
    if (length(values) <= 1L) {
      values <- replicate(length(names), values)
    }
    values <- as.list(values)
    stopifnot(length(values) == length(names))
  } else {
    values <- vector("list", length(names))
  }
  stats::setNames(values, names)
}

SW <- function(...) {
  suppressWarnings(...)
}
