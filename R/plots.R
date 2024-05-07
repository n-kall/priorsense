##' Diagnostic plots for power-scaling sensitivity
##'
##' Various diagnostic plots for power-scaling sensitivity. See **Plot Descriptions** below for details.
##'
##' @name powerscale_plots
##'
##' @template plot_args
##' @template div_measure_arg
##' @template resample_arg
##' @template ggplot_return
##' @section Plot Descriptions: \describe{
##'   \item{`powerscale_plot_dens()`}{ Kernel density plot of
##'   power-scaled posterior draws with respect to power-scaling.  }
##'   \item{`powerscale_plot_ecdf()`}{ Empirical cumulative
##'   distribution function plot of power-scaled posterior draws with
##'   respect to power-scaling.  }
##'   \item{`powerscale_plot_quantities()`}{ Plot of posterior
##'   quantities with respect to power-scaling.} }
##'
##' @importFrom rlang .data
##' @importFrom lifecycle deprecated
NULL

prepare_plot_data <- function(x, variable, resample, ...) {

  base_draws <- posterior::merge_chains(x$base_draws)

  if (!resample && !(x$resampled)) {
    base_draws <- posterior::weight_draws(
      x = base_draws,
      weights = rep(
        1 / posterior::ndraws(base_draws),
        posterior::ndraws(base_draws)
      )
    )
  }

  likelihood_draws <- list()
  prior_draws <- list()
  base_draws_prior <- data.frame()
  base_draws_lik <- data.frame()

  if (!(is.null(x$prior_scaled))) {
    prior_scaled <- x$prior_scaled$draws_sequence

    for (i in seq_along(prior_scaled)) {
      prior_draws[[i]] <- posterior::merge_chains(prior_scaled[[i]])

      if (resample && !x$resampled) {
        prior_draws[[i]]  <- posterior::resample_draws(prior_draws[[i]])
      }

      prior_ps_details <- get_powerscaling_details(prior_scaled[[i]])

      prior_draws[[i]]$alpha <- prior_ps_details$alpha
      prior_draws[[i]]$component <- "prior"
      prior_draws[[i]]$pareto_k <- prior_ps_details$diagnostics$khat
      prior_draws[[i]]$pareto_k_threshold <- prior_ps_details$diagnostics$khat_threshold

    }

    base_draws_prior <- base_draws
    base_draws_prior$alpha <- 1
    base_draws_prior$component <- "prior"
    base_draws_prior$pareto_k <- -Inf
    base_draws_prior$pareto_k_threshold <- Inf
  }

  if (!(is.null(x$likelihood_scaled))) {

    likelihood_scaled <- x$likelihood_scaled$draws_sequence

    for (i in seq_along(likelihood_scaled)) {
      likelihood_draws[[i]] <- posterior::merge_chains(
        likelihood_scaled[[i]]
      )

      if (resample && !x$resampled) {
        likelihood_draws[[i]]  <- posterior::resample_draws(
          likelihood_draws[[i]]
        )
      }

      likelihood_ps_details <- get_powerscaling_details(likelihood_scaled[[i]])

      likelihood_draws[[i]]$alpha <- likelihood_ps_details$alpha
      likelihood_draws[[i]]$component <- "likelihood"
      likelihood_draws[[i]]$pareto_k <- likelihood_ps_details$diagnostics$khat
      likelihood_draws[[i]]$pareto_k_threshold <- likelihood_ps_details$diagnostics$khat_threshold


    }

    base_draws_lik <- base_draws
    base_draws_lik$alpha <- 1
    base_draws_lik$component <- "likelihood"
    base_draws_lik$pareto_k <- -Inf
    base_draws_lik$pareto_k_threshold <- Inf
  }

  d <- rbind(
    do.call("rbind", prior_draws),
    do.call("rbind", likelihood_draws),
    base_draws_lik,
    base_draws_prior
  )

  d$pareto_k_value <- ifelse(d$pareto_k > d$pareto_k_threshold, "High",
                             "OK")

  d$pareto_k_value <- factor(
    d$pareto_k_value,
    levels = c("OK", "High")
  )


  d$component <- factor(d$component, levels = c("prior", "likelihood"))

  # prepare for plotting
  d <- stats::reshape(
    data = as.data.frame(d),
    varying = variable,
    direction = "long",
    times = variable,
    v.names = "value",
    timevar = "variable"
  )

  return(d)
}

prepare_plot <- function(d, resample, variable, colors, ...) {
  if (resample) {
    p <- ggplot2::ggplot(
      data = d,
      ggplot2::aes(
        x = .data$value,
        group = .data$alpha,
        color = .data$alpha,
        linetype = .data$pareto_k_value
      )
    )
  } else {
    p <- ggplot2::ggplot(
      data = d,
      ggplot2::aes(
        x = .data$value,
        weight = exp(.data$.log_weight),
        group = .data$alpha,
        color = .data$alpha,
        linetype = .data$pareto_k_value
      )
    )
  }

  p <- p +
    ggplot2::scale_linetype_manual(
      values = c("solid", "dashed"),
      drop = TRUE,
      name = "Pareto k"
    ) +
    ggplot2::scale_color_gradientn(
      name = "Power-scaling alpha",
      colours = colors[1:3],
      trans = "log",
      limits = c(min(d$alpha) - 0.01, max(d$alpha) + 0.01),
      breaks = c(min(d$alpha), 1, max(d$alpha)),
      labels = c(
        round(min(d$alpha), digits = 3),
        "1",
        round(max(d$alpha), digits = 3)
      )
    ) +
    ggplot2::scale_fill_gradientn(
      colours = c(colors[1:3]),
      trans = "log",
      limits = c(min(d$alpha) - 0.01, max(d$alpha) + 0.01),
      breaks = c(min(d$alpha), 1, max(d$alpha)),
      labels = c(
        round(min(d$alpha), digits = 3),
        "1",
        round(max(d$alpha), digits = 3)
      )
    )

  if (length(unique(d$alpha)) == 3) {
    p <- p +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = ggplot2::aes(linetype = "solid")
        )
      )
  }

  if (!(any(d$pareto_k_value == "High"))) {
    p <- p +
      ggplot2::guides(
        linetype = "none"
      )
  }

  return(p)

}

##' @rdname powerscale_plots
##' @export
powerscale_plot_dens <- function(x, ...) {
  UseMethod("powerscale_plot_dens")
}

##' @export
powerscale_plot_dens.default <- function(x,
                                         variable = NULL,
                                         length = 3,
                                         resample = FALSE,
                                         intervals = c(0.5, 0.8, 0.95),
                                         trim = NULL,
                                         facet_rows = "component",
                                         help_text = getOption("priorsense.plot_help_text", TRUE),
                                         colors = NULL,
                                         switch_facets = deprecated(),
                                         variables = deprecated(), ...) {
  ps <- powerscale_sequence(x, length = length, ...)
  powerscale_plot_dens(
    ps,
    variable = variable,
    length = length,
    resample = resample,
    intervals = intervals,
    trim = trim,
    facet_rows = facet_rows,
    help_text = help_text,
    colors = colors,
    switch_facets = switch_facets,
    variables = variables
  )
}


##' @export
powerscale_plot_dens.powerscaled_sequence <- function(x,
                                                      variable = NULL,
                                                      resample = FALSE,
                                                      intervals = c(0.5, 0.8, 0.95),
                                                      trim = NULL,
                                                      facet_rows = "component",
                                                      help_text = getOption("priorsense.plot_help_text", TRUE),
                                                      colors = NULL,
                                                      switch_facets = deprecated(),
                                                      variables = deprecated(),
                                                      ...) {

    if (lifecycle::is_present(variables)) {
    lifecycle::deprecate_warn("0.9", "powerscale_plot_dens(variables)", "powerscale_plot_dens(variable)")
    variable <- variables
    }

  if (lifecycle::is_present(switch_facets)) {
    lifecycle::deprecate_warn("0.9.1", "powerscale_plot_dens(switch_facets)", "powerscale_plot_dens(facet_rows)")
    if (switch_facets) {
      facet_rows <- "variable"
    } else {
      facet_rows <- "component"
    }
  }

  # input checks
  checkmate::assert_character(variable, null.ok = TRUE)
  checkmate::assert_logical(resample, len = 1)
  checkmate::assert_logical(help_text, len = 1)
  checkmate::assertCharacter(colors, len = 3, null.ok = TRUE)
  checkmate::assert_numeric(intervals, null.ok = TRUE)
  checkmate::assert_choice(facet_rows, c("component", "variable"))

  if (is.null(colors)) {
    colors <- default_priorsense_colors()[1:3]
  }

  if (is.null(variable)) {
    variable <- posterior::variables(x$base_draws)
  } else {
    variable <- posterior::variables(
      posterior::subset_draws(x$base_draws, variable = variable)
    )
  }

  d <- prepare_plot_data(x, variable = variable, resample = resample, ...)

  interval_positions <- data.frame(
    alpha = unique(d$alpha),
    interval_y = as.numeric(as.factor(unique(d$alpha)))
  )

  interval_positions$interval_y <- (0.5 - (interval_positions$interval_y)) / (6 * nrow(interval_positions))

  d <- merge(d, interval_positions)

  if (resample || x$resample) {
    resample <- TRUE
  }

  n_components <- length(unique(d$component))

  out <- prepare_plot(d, resample = resample, colors = colors, ...) +
    ggplot2::ylab("Density")

    # here we have to draw 2 stat slabs (one with alpha 0 and black fill,
    # one with fill as NA) to get the legend correct see
  # https://github.com/mjskay/ggdist/issues/134
  out <- out +
    ggdist::stat_slab(
      fill = "black",
       alpha = 0,
       linewidth = 0.5,
       trim = FALSE,
       normalize = "xy",
       key_glyph = "smooth"
     ) +
    ggdist::stat_slab(
       fill = NA,
       alpha = 1,
       linewidth = 0.5,
       trim = FALSE,
       normalize = "xy",
       key_glyph = "smooth"
     )

  if (!is.null(intervals)) {

  out <- out +
    ggdist::stat_pointinterval(
      ggplot2::aes(y = .data$interval_y),
      .width = intervals,
      fill = NA,
      alpha = 1,
      trim = FALSE,
      normalize = "xy",
      key_glyph = "smooth"
    )
  }


  if (facet_rows == "component") {
    out <- out +
      ggh4x::facet_grid2(
        rows = ggplot2::vars(.data$component),
        cols = ggplot2::vars(.data$variable),
        labeller = ggplot2::labeller(
          component = c(
            likelihood = "Likelihood power-scaling",
            prior = "Prior power-scaling"
          )
        ),
        independent = "all",
        scales = "free",
        switch = "y"
      )
  } else {
  out <- out +
    ggh4x::facet_grid2(
      rows = ggplot2::vars(.data$variable),
      cols = ggplot2::vars(.data$component),
      labeller = ggplot2::labeller(
        component = c(
          likelihood = "Likelihood power-scaling",
          prior = "Prior power-scaling"
        )
      ),
      independent = "all",
      scales = "free",
      switch = "y"
    )

  }

  if (help_text) {
    out <- out +
      ggplot2::ggtitle(
        label = "Power-scaling sensitivity",
        subtitle = "Posterior density estimates depending on amount of power-scaling (alpha).\nOverlapping lines indicate low sensitivity.\nWider gaps between lines indicate greater sensitivity.\nEstimates with high Pareto k (dashed lines) may be inaccurate."
      )
  }

  if (getOption("priorsense.use_plot_theme", TRUE)) {
    out <- out +
      theme_priorsense() +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(NULL) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank()
      )
  }

  if (facet_rows == "component") {
    out <- out +
      ggplot2::theme(legend.position = "bottom")
  }


  if (!is.null(trim)) {
  position_scales <- lapply(
    variable, FUN =
    function(.x, prob) {
      limits <- quantile2(x$base_draws[[.x]], probs = c((1 - prob)/2, prob + (1 - prob)/2))
      return(scale_x_continuous(limits = limits))
    }, prob = trim
  )

  if (facet_rows == "component") {
    out <- out + ggh4x::facetted_pos_scales(x = rep(position_scales, times = 2))
  } else {
    out <- out + ggh4x::facetted_pos_scales(x = rep(position_scales, each = 2))
  }
  }

  return(out)
}

##' @rdname powerscale_plots
##' @export
powerscale_plot_ecdf <- function(x, ...) {
  UseMethod("powerscale_plot_ecdf")
}


##' @export
powerscale_plot_ecdf.default <- function(x,
                                         variable = NULL,
                                         length = 3,
                                         resample = FALSE,
                                         facet_rows = "component",
                                         help_text = getOption("priorsense.plot_help_text", TRUE),
                                         colors = NULL,
                                         variables = lifecycle::deprecated(), ...) {
  ps <- powerscale_sequence(x, length = length, ...)
  powerscale_plot_ecdf(
    ps,
    variable = variable,
    resample = resample,
    facet_rows = facet_rows,
    help_text = help_text,
    colors = colors,
    variables = variables
  )
}

##' @rdname powerscale_plots
##' @export
powerscale_plot_ecdf.powerscaled_sequence <- function(x,
                                                      variable = NULL,
                                                      resample = FALSE,
                                                      length = 3,
                                                      facet_rows = "component",
                                                      help_text = getOption("priorsense.plot_help_text", TRUE),
                                                      colors = NULL,
                                                      switch_facets = deprecated(),
                                                      variables = deprecated(), ...) {

  if (lifecycle::is_present(variables)) {
    lifecycle::deprecate_warn("0.9", "powerscale_plot_ecdf(variables)", "powerscale_plot_ecdf(variable)")
    variable <- variables
  }

  if (lifecycle::is_present(switch_facets)) {
    lifecycle::deprecate_warn("0.9.1", "powerscale_plot_dens(switch_facets)", "powerscale_plot_dens(facet_rows)")
    if (switch_facets) {
      facet_rows <- "variable"
    } else {
      facet_rows <- "component"
    }
  }


  # input checks
  checkmate::assert_character(variable, null.ok = TRUE)
  checkmate::assert_logical(resample, len = 1)
  checkmate::assert_logical(help_text, len = 1)
  checkmate::assertCharacter(colors, len = 3, null.ok = TRUE)
  checkmate::assert_choice(facet_rows, c("component", "variable"))

  if (is.null(colors)) {
    colors <- default_priorsense_colors()[1:3]
  }

  if (is.null(variable)) {
    variable <- posterior::variables(x$base_draws)
  } else {
    variable <- posterior::variables(
      posterior::subset_draws(x$base_draws, variable = variable)
    )
  }

  d <- prepare_plot_data(x, variable = variable, resample = resample, ...)

  n_components <- length(unique(d$component))

  if (resample || x$resample) {
    resample <- TRUE
  }
  p <- prepare_plot(d, resample = resample, colors = colors, ...) +
    ggplot2::guides(
      linetype = ggplot2::guide_legend(
        title = "Pareto k"
      )
    ) +
    ggplot2::ylab("ECDF")

  if (resample || x$resampled) {
    p <- p +
      ggplot2::stat_ecdf(ggplot2::aes(color = .data$alpha))
  } else {
    p <- p +
      stat_ewcdf(ggplot2::aes(color = .data$alpha))
  }


  if (facet_rows == "component") {
  p <- p +
    ggh4x::facet_grid2(
      rows = ggplot2::vars(.data$component),
      cols = ggplot2::vars(.data$variable),
    labeller = ggplot2::labeller(
      component = c(
        likelihood = "Likelihood power-scaling",
        prior = "Prior power-scaling"
      )
    ),
    scales = "free",
    independent = "all",
    switch = "y"
    )
  } else {
   p <- p + ggh4x::facet_grid2(
      rows = ggplot2::vars(.data$variable),
      cols = ggplot2::vars(.data$component),
    labeller = ggplot2::labeller(
      component = c(
        likelihood = "Likelihood power-scaling",
        prior = "Prior power-scaling"
      )
    ),
    scales = "free",
    independent = "all",
    switch = "y"
    )

  }

  if (!(any(d$pareto_k_value == "High"))) {

    p <- p +
      ggplot2::guides(linetype = "none")

  }

  if (help_text) {
    p <- p +
  ggplot2::ggtitle(
    label = "Power-scaling sensitivity",
    subtitle = "Posterior ECDF depending on amount of power-scaling (alpha).\nOverlapping lines indicate low sensitivity.\nWider gaps between lines indicate greater sensitivity.\nEstimates with high Pareto k (dashed lines) may be inaccurate."
  )
  }

  if (getOption("priorsense.use_plot_theme", TRUE)) {
    p <- p +
      ggplot2::xlab(NULL) +
      theme_priorsense()
  }

  if (facet_rows == "component") {
    p <- p +
      ggplot2::theme(legend.position = "bottom")
  }

  return(p)

}


##' @rdname powerscale_plots
##' @export
powerscale_plot_quantities <- function(x, ...) {
  UseMethod("powerscale_plot_quantities")
}

##' @export
powerscale_plot_quantities.default <- function(x, variable = NULL,
                                       quantity = c("mean", "sd"),
                                       div_measure = "cjs_dist",
                                       length = 11,
                                       resample = FALSE,
                                       measure_args = NULL,
                                       mcse = TRUE,
                                       quantity_args = NULL,
                                       help_text = getOption("priorsense.plot_help_text", TRUE),
                                       colors = NULL,
                                       switch_facets = deprecated(),
                                       variables = deprecated(),
                                       quantities = deprecated(),
                                       ...) {
  ps <- powerscale_sequence(x, length = length, ...)

  powerscale_plot_quantities(
    ps,
    variable = variable,
    quantity = quantity,
    div_measure = div_measure,
    resample = resample,
    measure_args = measure_args,
    mcse = mcse,
    quantity_args = quantity_args,
    help_text = help_text,
    colors = colors,
    switch_facets = switch_facets,
    quantities = quantities,
    variables = variables
  )
}

##' @rdname powerscale_plots
##' @export
powerscale_plot_quantities.powerscaled_sequence <- function(x, variable = NULL,
                                       quantity = c("mean", "sd"),
                                       div_measure = "cjs_dist",
                                       resample = FALSE,
                                       measure_args = NULL,
                                       mcse = TRUE,
                                       quantity_args = NULL,
                                       help_text = getOption("priorsense.plot_help_text", TRUE),
                                       colors = NULL,
                                       switch_facets = deprecated(),
                                       quantities = deprecated(),
                                       variables = deprecated(),
                                       ...) {

  if (lifecycle::is_present(variables)) {
    lifecycle::deprecate_warn("0.9", "powerscale_plot_quantities(variables)", "powerscale_plot_quantities(variable)")
    variable <- variables
  }

  if (lifecycle::is_present(quantities)) {
    lifecycle::deprecate_warn("0.9", "powerscale_plot_quantities(quantities)", "powerscale_plot_quantities(quantity)")
    quantity <- quantities
  }

    if (lifecycle::is_present(switch_facets)) {
    lifecycle::deprecate_warn("0.9.1", "powerscale_plot_dens(switch_facets)", "powerscale_plot_dens(facet_rows)")
    if (switch_facets) {
      facet_rows <- "variable"
    } else {
      facet_rows <- "component"
    }
  }


  checkmate::assertCharacter(variable, null.ok = TRUE)
  checkmate::assertCharacter(quantity)
  checkmate::assertCharacter(div_measure, null.ok = TRUE)
  checkmate::assertLogical(resample, len = 1)
  checkmate::assertList(measure_args, null.ok = TRUE)
  checkmate::assertLogical(mcse, len = 1)
  checkmate::assertList(quantity_args, null.ok = TRUE)
  checkmate::assertLogical(help_text, len = 1)
  checkmate::assertCharacter(colors, len = 2, null.ok = TRUE)

  if (is.null(colors)) {
    colors <- default_priorsense_colors()[4:5]
  }

  names(quantity) <- quantity

  summ <- summarise_draws(
    x,
    quantity,
    .args = quantity_args,
    resample = resample,
    div_measures = div_measure,
    measure_args = measure_args
  )

  if (is.null(variable)) {
    variable <- posterior::variables(x$base_draws)
  } else {
    # TODO: better way to handle e.g. "a" -> "a[1]", "a[2]"
    variable <- posterior::variables(
      posterior::subset_draws(x$base_draws, variable = variable)
    )
  }

  if (mcse) {
    quants <- setdiff(
      colnames(summ[[1]]),
      c("variable", "alpha", "component",
        "pareto_k", "pareto_kf", "pareto_k_threshold", "n_eff", div_measure)
    )

    mcse_functions <- paste0("mcse_", quantity)

    base_quantities <- summ[[1]][which(summ[[1]]$alpha == 1), ]
    base_quantities <- unique(base_quantities[c("variable", quants)])

    base_q <- as.data.frame(base_quantities)

    base_q <- as.data.frame(base_quantities)

    base_q <- stats::reshape(
      data = base_q,
      varying = quants,
      direction = "long",
      times = quants,
      v.names = "value",
      timevar = "quantity",
      idvar = "variable"
    )

    base_mcse <- posterior::summarise_draws(
      x$base_draws,
      mcse_functions,
      .args = quantity_args
    )
    base_mcse <- base_mcse[which(base_mcse$variable %in% variable), ]
    base_mcse <- as.data.frame(base_mcse)

    mcse_names <- colnames(base_mcse)[-1]

    base_mcse <- stats::reshape(
      data = base_mcse,
      varying = mcse_names,
      direction = "long",
      times = quants,
      v.names = "mcse",
      timevar = "quantity",
      idvar = "variable"
    )



    base_mcse <- merge(base_q, base_mcse)
    base_mcse$mcse_min <- base_mcse$value - 2 * base_mcse$mcse
    base_mcse$mcse_max <- base_mcse$value + 2 * base_mcse$mcse

  } else {
    base_mcse <- NULL
  }

  return(
    powerscale_summary_plot(
      summ,
      variable = variable,
      base_mcse = base_mcse,
      help_text = help_text,
      colors = colors,
      ...
    )
  )

}

powerscale_summary_plot <- function(x,
                                    variable,
                                    base_mcse = NULL,
                                    help_text,
                                    colors,
                                    ...) {


  pareto_k_colours <- colors

  # get default quantities
  quantities <- setdiff(
    colnames(x[[1]]),
    c("variable", "alpha", "component",
      "pareto_k", "pareto_kf", "n_eff", "pareto_k_threshold")
  )

  # select only specified variables
    x[[1]] <- x[[1]][x[[1]][["variable"]] %in% variable, ]

  # reshape quantities for plotting
  summaries <- stats::reshape(
    data = x[[1]],
    varying = quantities,
    direction = "long",
    times = quantities,
    v.names = "value",
    timevar = "quantity"
  )

  summaries$pareto_k_value <- ifelse(
    summaries$pareto_k > summaries$pareto_k_threshold,
    "High",
    "OK"
  )

  summaries$pareto_k_value <- factor(
    summaries$pareto_k_value,
    levels = c("OK", "High")
  )

  # subset for plotting points at ends of lines
  points <- summaries[summaries$alpha == min(summaries$alpha) |
                        summaries$alpha == max(summaries$alpha), ]

  p <- ggplot2::ggplot(
    data = summaries,
    mapping = ggplot2::aes(x = .data$alpha, y = .data$value)
  ) +
    ggplot2::geom_line(ggplot2::aes(
      color = .data$pareto_k_value, group = .data$component)) +
    ggh4x::facet_grid2(
      rows = ggplot2::vars(factor(.data$variable, levels = unique(.data$variable))),
      cols = ggplot2::vars(factor(.data$quantity, levels = unique(.data$quantity))),
      scales = "free",
      switch = "y",
    independent = "all"
  ) +
  ggplot2::geom_point(
    ggplot2::aes(
      x = .data$alpha,
      y = .data$value,
      shape = .data$component
    ),
    fill = "white",
    size = 3,
    data = points
  ) +
    ggplot2::scale_shape_manual(
      values = c("likelihood" = 22, "prior" = 15)) +
  ggplot2::scale_color_manual(values = pareto_k_colours) +
  ggplot2::guides(
    color = ggplot2::guide_legend(
      title = "Pareto k",
      override.aes = list(shape = 15)
    )
  ) +
  ggplot2::ylab(NULL) +
  ggplot2::scale_x_continuous(
    trans = "log2",
    limits = c(0.95 * min(summaries$alpha), 1.05 * max(summaries$alpha)),
    breaks = c(min(summaries$alpha), 1, max(summaries$alpha)),
    labels = round(c(min(summaries$alpha), 1, max(summaries$alpha)), digits = 3)
  )

  if (!(any(summaries$pareto_k_value == "High"))) {
    p <- p +
      ggplot2::guides(
        colour = "none"
      )
  }

  if (help_text) {
    p <- p +
  ggplot2::ggtitle(
    label = "Power-scaling sensitivity",
    subtitle = "Posterior quantities depending on amount of power-scaling (alpha).\nHorizontal lines indicate low sensitivity.\nSteeper lines indicate greater sensitivity.\nEstimates with high Pareto k (highlighted) may be inaccurate."
  )
  }

  if (!is.null(base_mcse)) {

    p <- p +
      ggplot2::scale_linetype_manual(values = "dashed", name = NULL) +
      ggplot2::geom_hline(
        ggplot2::aes(
          yintercept = .data$mcse_min,
          linetype = "+-2MCSE"
        ),
        data = base_mcse,
        color = "black"
      ) +
      ggplot2::geom_hline(
        ggplot2::aes(
          yintercept = .data$mcse_max,
          linetype = "+-2MCSE"
        ),
        data = base_mcse,
        color = "black"
      )
  }

  if (getOption("priorsense.use_plot_theme", TRUE)) {
    p <- p +
      theme_priorsense()
  }

  return(p)
}
