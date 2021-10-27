#' @export
print.powerscaling_details <- function(x, ...) {

  pareto_k <- x$importance_sampling$diagnostics$pareto_k
  pareto_kf <- x$importance_sampling$diagnostics$pareto_kf
  n_eff <- x$importance_sampling$diagnostics$n_eff
  is_method <- class(x$importance_sampling)[[1]]

  pareto_k_print <- c()

  if (is_method == "psis") {
    pareto_k_print <- paste("pareto-k:", round(pareto_k, digits = 3), "\n")
  }
  if (!is.null(pareto_kf)) {
    pareto_k_print <- c(
      "moment-matched\n",
      pareto_k_print,
      paste("pareto-kf",
            round(pareto_kf, digits = 3),
            "\n")
    )
  }

  cat(
    "\npower-scaling\n",
    paste("alpha:", x$alpha, "\n"),
    paste("scaled component:", x$component, "\n"),
    paste("importance sampling method:", is_method, "\n"),
    pareto_k_print,
    paste("n_eff:", round(n_eff, digits = 3), "\n"),
    paste("resampled:", x$resampled, "\n"),
    paste("transform:", x$transform_details$transform, "\n")
  )

  invisible(x)
}

##' @export
print.powerscaled_draws <- function(x, ...) {
  print(x$draws)
  print(x$powerscaling)

  invisible(x)
}

##' @export
print.powerscaled_draws_summary <- function(x, ...) {
  print(x$draws_summary)
  print(x$powerscaling)

  invisible(x)
}

#' @export
print.powerscaled_sequence <- function(x, ...) {

  if (!is.null(x$prior_scaled)) {
    alpha_range <- c(
      x$prior_scaled$draws_sequence[[1]]$powerscaling$alpha,
      x$prior_scaled$draws_sequence[[length(x$prior_scaled$draws_sequence)]]$powerscaling$alpha
    )
  } else {
    alpha_range <- c(
      x$likelihood_scaled$draws_sequence[[1]]$powerscaling$alpha,
      x$likelihood_scaled$draws_sequence[[length(x$likelihood_scaled$draws_sequence)]]$powerscaling$alpha
    )
  }

  component <- c()
  if (!is.null(x$prior_scaled)) {
    component <- c("prior", component)
  }

  if (!is.null(x$likelihood_scaled)) {
    component <- c("likelihood", component)
  }
  
  cat("base draws:\n")
  print(x$base_draws)

  cat(
    "\npower-scaling\n",
    paste0("alpha range: [", alpha_range[1], ", ",  alpha_range[2], "]\n"),
    paste("length of sequence:", min(length(x$prior_scaled$draws_sequence), length(x$likelihood_scaled$draws_sequence), na.rm = TRUE), "\n"),
    paste("scaled component:", component, "\n"),
    paste("importance sampling method:", x$is_method, "\n")
  )

  invisible(x)
}


#' @export
print.powerscaled_sensitivity_summary <- function(x, ...) {

  cat(paste0("Sensitivity based on ", x$div_measure, ":\n"))
  
  print(x$sensitivity)
    
  invisible(x)

}
