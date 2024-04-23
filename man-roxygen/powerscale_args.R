##' @param moment_match Logical; Indicate whether or not moment
##'   matching should be performed. Can only be TRUE if `is_method` is
##'   "psis".
##' @param transform Indicate a transformation of posterior draws to
##'   perform before sensitivity analysis. Either "scale" or "whiten".
##' @param k_threshold Threshold value for Pareto k values above which
##'   the moment matching algorithm is used. Default is 0.5.
##' @param resample Logical; Indicate whether or not draws should be
##'   resampled based on calculated importance weights.
