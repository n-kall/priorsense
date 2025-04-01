##' @export
print.powerscaling_details <- function(x, ...) {

  pareto_k <- x$diagnostics$khat
  pareto_k_threshold <- x$diagnostics$khat_threshold
  pareto_kf <- x$diagnostics$khatf

  pareto_k_print <- c()

  pareto_k_print <- paste("pareto-k:", round(pareto_k, digits = 2), "\n")

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
    "selection:", x$selection, "\n",
    pareto_k_print,
    paste("pareto-k threshold:", round(pareto_k_threshold, 2), "\n"),
    paste("resampled:", x$resampled, "\n"),
    paste("transform:", x$transform_details$transform, "\n")
  )

  invisible(x)
}

##' @export
print.powerscaled_draws <- function(x, ...) {
  NextMethod(...)
  print(attr(x, "powerscaling"), ...)

  invisible(x)
}

##' @export
print.powerscaled_draws_summary <- function(x, ...) {
  NextMethod()
  print(get_powerscaling_details(x))

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
  print(x$base_draws, ...)

  cat(
    "\npower-scaling\n",
    paste0("alpha range: [", min(x$alphas), ", ", max(x$alphas), "]\n"),
    paste("length of sequence:", length(x$alphas), "\n"),
    paste("scaled component:", component, "\n"),
    paste("transform:", x$transform$transform, "\n")
  )

  invisible(x)
}


##' @export
print.powerscaled_sensitivity_summary <- function(x, ..., num_args = NULL) {

  num_args <- num_args %||% attr(x, "num_args")

  for (i in seq_cols(x)) {
    if (is.numeric(x[[i]])) {
      x[[i]] <- do.call(tibble::set_num_opts, c(list(x[[i]]), num_args))
    }
  }
  cat(paste0("Sensitivity based on ", attr(x, "div_measure"), "\n"))
  cat(paste0("Prior selection: ", attr(x, "prior_selection"), "\n"))
  cat(paste0("Likelihood selection: ", attr(x, "likelihood_selection"), "\n"))

  NextMethod()
  if (!is.null(attr(x, "loadings"))) {
    cat("Factor loadings:\n")
    print(round(attr(x, "loadings"), 2))
  }
  invisible(x)
}


##' @export
print.whitened_draws <- function(x, ...) {
  NextMethod()
  cat("Factor loadings:\n")
  print(attr(x, "loadings"), ...)
  invisible(x)

}

##' @export
print.whitened_draws_summary <- function(x, ...) {
  NextMethod()
  cat("Factor loadings:\n")
  print(attr(x, "loadings"), ...)
  invisible(x)

}
