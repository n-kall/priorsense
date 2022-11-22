moment_match.powerscaling_data <- function(x, ...) {

  out <- iwmm::moment_match(
    x = x$fit,
    ...
  )
  out
}

powerscale_log_ratio_fun <- function(draws, component_draws, alpha, ...) {
  ratio <- (alpha - 1) * component_draws
  
  return(ratio)
}
