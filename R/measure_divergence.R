##' Calculate specified divergence measures for each posterior
##'
##' @param draws1 draws of first distribution
##' @param draws2 draws of second distribution
##' @param measure divergence measure
##' @param measure_args arguments for divergence measure
##' @param ... unused
##' @return a tibble
measure_divergence <- function(draws1, draws2,
                                measure,
                                measure_args = list(),
                                ...) {

  draws1 <- posterior::as_draws_df(draws1)
  draws2 <- posterior::as_draws_df(draws2)

  weights1 <- stats::weights(draws1, log = FALSE)
  weights2 <- stats::weights(draws2, log = FALSE)

  variables <- posterior::variables(draws1)

  out <- tibble::as_tibble_col(variables, "variable")

  for (m in measure) {
    divs <- numeric(length(variables))
    names(divs) <- variables
    for (v in variables) {

      args <- c(
        list(
          x = draws1[[v]],
          y = draws2[[v]],
          x_weights = weights1,
          y_weights = weights2
        ),
        measure_args
      )

      divs[v] <- do.call(
        what = m,
        args = args
      )
    }
    divs <- tibble::as_tibble_col(divs, column_name = m)
    out <- cbind(out, divs)
  }

  return(out)
}
