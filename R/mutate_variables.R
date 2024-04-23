##' @export
mutate_variables.powerscaled_draws <- function(.x, ...) {

  .x$draws <- posterior::mutate_variables(.x$draws, ...)

  return(.x)
}
