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
##' @param transform Indicate a transformation of posterior
##'   draws to perform before sensitivity analysis. Either "scale" or "whiten".
##' @param k_threshold Threshold value for Pareto k values above which
##'   the moment matching algorithm is used. Default is 0.5.
##' @param resample Logical; Indicate whether or not draws should be
##'   resampled based on calculated importance weights.
