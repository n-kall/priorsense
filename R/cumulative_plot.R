##' stat ewcdf
##'
##' @param mapping aesthetic mapping
##' @param data data for plotting
##' @param ... unused
##' @return LayerInstance object
##' @keywords internal
##' @noRd
stat_ewcdf <- function(mapping = NULL, data = NULL,
                       ...) {
  # Code adapted from
# https://github.com/finnlindgren/StatCompLab/blob/main/R/ggplot.R and
# https://rdrr.io/github/tidyverse/ggplot2/src/R/stat-ecdf.r

# TODO: when ggplot2 version 3.5.2 is release, switch to ggplot2 version
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatEwcdf,
    geom = "step",
    position = "identity",
    show.legend = NA,
    inherit.aes = TRUE,
    params = list(
      n = NULL,
      pad = TRUE,
      na.rm = FALSE
    )
  )
}


StatEwcdf <- ggplot2::ggproto(
  "StatEwcdf", ggplot2::Stat,
  required_aes = c("x|y", "weight"),

  default_aes = ggplot2::aes(y = ggplot2::after_stat(y)),

  setup_params = function(data, params) {
    params$flipped_aes <-
      ggplot2::has_flipped_aes(data,
                               params,
                               main_is_orthogonal = FALSE,
                               main_is_continuous = TRUE)

    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))
    has_weights <- !(is.null(data$weight) && is.null(params$weight))

    params
  },

  compute_group = function(data, scales, n = NULL,
                           pad = TRUE, flipped_aes = FALSE) {
    data <- ggplot2::flip_data(data, flipped_aes)
    # If n is NULL, use raw values; otherwise interpolate
    if (is.null(n)) {
      x <- unique(data$x)
    } else {
      x <- seq(min(data$x), max(data$x), length.out = n)
    }

    if (pad) {
      x <- c(-Inf, x, Inf)
    }
    if (is.null(data$weight)) {
      data_ecdf <- stats::ecdf(data$x)(x)
    } else {
      data_ecdf <-
        ewcdf(
          data$x,
          weights = data$weight / sum(data$weight)
        )(x)
    }

    df_ecdf <- data.frame(x = x, y = data_ecdf)
    df_ecdf$flipped_aes <- flipped_aes
    ggplot2::flip_data(df_ecdf, flipped_aes)
    
  },
  dropped_aes = "weight"
)
