##' @param alpha Value by which to power-scale specified
##'   component. (likelihood/prior).
##' @param lower_alpha Lower power-scaling alpha value in sequence.
##' @param upper_alpha Upper power-scaling alpha value in sequence.
##' @param length Total length of alpha sequence, including 1.0.
##' @param auto_alpha_range Boolean. Restrict range to ensure Pareto-k values
##'   below threshold?
##' @param symmetric Boolean. Should the alpha range be symmetrical around alpha
##'   = 1 on log-scale? If `TRUE`, the length of the range will be always be
##'   odd, and will be equal to `length - 1` if specified `length` is even.
