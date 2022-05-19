##' @export
print.powerscaling_details <- function(x, ...) {

  pareto_k <- x$importance_sampling$diagnostics$pareto_k
  pareto_kf <- x$importance_sampling$diagnostics$pareto_kf
  ess <- x$importance_sampling$diagnostics$n_eff
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
    paste("ESS:", round(ess, digits = 3), "\n"),
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

##' @export
print.powerscaled_sequence <- function(x, ...) {

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
    paste0("alpha range: [", min(x$alphas), ", " , max(x$alphas), "]\n"),
    paste("length of sequence:", length(x$alphas), "\n"),
    paste("scaled component:", component, "\n"),
    paste("importance sampling method:", x$is_method, "\n"),
    paste("transform:", x$transform$transform, "\n")
  )

  invisible(x)
}


##' @export
print.powerscaled_sensitivity_summary <- function(x, ...) {

  cat(paste0("Sensitivity based on ", x$div_measure, ":\n"))
  
  print(x$sensitivity)
  if (!is.null(x$loadings)) {
    cat("Factor loadings:\n")
    print(round(x$loadings, 2))
  }
  invisible(x)

}
