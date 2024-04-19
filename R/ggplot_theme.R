#' priorsense theme
#'
#' Custom ggplot theme
#'
#' @return A ggplot2 theme
#' @export

theme_priorsense <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      strip.placement = "outside",
      axis.line.x = ggplot2::element_line(
        linewidth = 0.5,
        linetype = "solid",
        colour = "black"
      ),
      axis.line.y = ggplot2::element_line(
        linewidth = 0.5,
        linetype = "solid",
        colour = "black"
      ),
      axis.text = ggplot2::element_text(colour = "black")
    )
}
