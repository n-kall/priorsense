`%||%` <- function(x, y) {
  if (is.null(x)) x <- y
  x
}

seq_cols <- function(x) {
  seq_len(NCOL(x))
}
