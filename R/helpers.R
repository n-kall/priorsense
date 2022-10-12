rowsums_draws <- function(x) {
  posterior::draws_array(
        sum = rowSums(
          x,
          dims = 2
        ),
        .nchains = posterior::nchains(x)
      )
}
