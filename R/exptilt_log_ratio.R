##' Calculate importance ratios based on scaling of component
##'
##' @param component_draws draws from component to powerscale
##' @param alpha scaling factor
##' @param ... unused
##' @return log ratio
##' @keywords internal
##' @noRd
exptilt_log_ratio <- function(draws,
                              eta,
                              tilt_fun,
                              tilt_fun_args) {

  # calculate log ratios for tilting
  tilted <- eta * tilt_fun(draws)
  
  return(tilted)
}

# Density ratio function for moment matching
##' @param draws draws object
##' @param fit fit object that will be passed to `iwmm::constrain_draws`
##' @param alpha power-scaling alpha value
##' @param component_fn function to extract log component from constrained draws
##' @param ... unused
##' @return vector of density ratio
##' @keywords internal
##' @noRd
powerscale_log_ratio_fun <- function(draws, fit, alpha, component_fn, ...) {

  constr_draws <- iwmm::constrain_draws(fit, draws)

  component_draws <- rowsums_draws(component_fn(constr_draws))
   
  component_draws * (alpha - 1)
  
}
