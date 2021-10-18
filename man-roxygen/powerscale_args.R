##' @param is_method The importance sampling method to use. The
##'   following methods are available:
##'
##' * `"psis"`: Pareto-Smoothed Importance Sampling (PSIS). Default
##'   method.
##'
##' * `"tis"`: Truncated Importance Sampling (TIS) with truncation at
##'   `sqrt(S)`, where `S` is the number of posterior draws.
##'
##' * `"sis"`: Standard Importance Sampling (SIS).
##'
##' For further details, see the `loo` package.
##' @param moment_match Logical; Indicate whether or not moment
##'   matching should be performed. Can only be TRUE if `is_method` is
##'   "psis".
##' @param transform Indicate transformation of posterior
##'   draws. Either "scale" or "spherize".
##' @param k_threshold Threshold value for Pareto k values above which
##'   the moment matching algorithm is used. Default is 0.5.
##' @param resample Logical; Indicate whether or not draws should be
##'   resampled based on calculated importance weights.
##' @param log_prior_fn A function that takes as input the model fit
##'   and returns the log prior values. Provided functions are
##'   `calculate_log_prior` (requires model to be fit with rstan and
##'   in the same R session) and `extract_log_prior` (requires a
##'   variable in the Stan model code to correspond to the joint log
##'   prior, by default named "log_prior").
##' @param joint_log_lik_fn A function that takes as input the model
##'   fit and returns the joint log likelihood values.
