##' Diagnostic plots for power-scaling sensitivity
##'
##' Various diagnostic plots for power-scaling sensitivity. See **Plot
##' Descriptions** below for details.
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
##' @srrstats {EA5.0} 
##' @srrstats {EA5.0a} default typefaces in ggplot2 are accessible
##' @srrstats {EA5.0b} default color scheme chosen to be accessible
##' @srrstats {EA5.4} values are rounded before plotting


##' @importFrom rlang .data
##' @examples
##' ex <- example_powerscale_model()
##'
##' powerscale_plot_dens(ex$draws)
NULL

##' prepare plot data
##' @param x priorsense_data object
##' @param variable character vector specifying variables to plot
##' @param resample boolean flag specifying whether to resample
##' @param ... unused
##' @return data frame with data for plotting
##' @keywords internal
##' @noRd
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

      prior_draws[[i]][[".powerscale_alpha"]] <- prior_ps_details$alpha
      prior_draws[[i]]$component <- "prior"
      prior_draws[[i]]$pareto_k <- prior_ps_details$diagnostics$khat
      prior_draws[[i]]$pareto_k_threshold <-
        prior_ps_details$diagnostics$khat_threshold

    }

    base_draws_prior <- base_draws
    base_draws_prior[[".powerscale_alpha"]] <- 1
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

      likelihood_ps_details <- get_powerscaling_details(
        likelihood_scaled[[i]]
      )

      likelihood_draws[[i]][[".powerscale_alpha"]] <-
        likelihood_ps_details$alpha
      
      likelihood_draws[[i]]$component <- "likelihood"
      
      likelihood_draws[[i]]$pareto_k <- likelihood_ps_details$diagnostics$khat
      
      likelihood_draws[[i]]$pareto_k_threshold <-
        likelihood_ps_details$diagnostics$khat_threshold


    }

    base_draws_lik <- base_draws
    base_draws_lik[[".powerscale_alpha"]] <- 1
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
##' prepare plot
##' @param d data frame of data from plotting
##' @param resample boolean flag specifying whether to resample
##' @param variable character vector specifying variables to plot
##' @param colors character vector specifying colors
##' @param ... unused
##' @return ggplot object
##' @keywords internal
##' @noRd
prepare_plot <- function(d, resample, variable, colors, ...) {
  if (resample) {
    p <- ggplot2::ggplot(
      data = d,
      ggplot2::aes(
        x = .data$value,
        group = .data[[".powerscale_alpha"]],
        color = .data[[".powerscale_alpha"]],
        linetype = .data$pareto_k_value
      )
    )
  } else {
    p <- ggplot2::ggplot(
      data = d,
      ggplot2::aes(
        x = .data$value,
        weight = exp(.data$.log_weight),
        group = .data[[".powerscale_alpha"]],
        color = .data[[".powerscale_alpha"]],
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
      limits = c(min(d[[".powerscale_alpha"]]) - 0.01,
                 max(d[[".powerscale_alpha"]]) + 0.01),
      breaks = c(min(d[[".powerscale_alpha"]]), 1,
                 max(d[[".powerscale_alpha"]])),
      labels = c(
        round(min(d[[".powerscale_alpha"]]), digits = 3),
        "1",
        round(max(d[[".powerscale_alpha"]]), digits = 3)
      )
    ) +
    ggplot2::scale_fill_gradientn(
      colours = c(colors[1:3]),
      trans = "log",
      limits = c(min(d[[".powerscale_alpha"]]) - 0.01,
                 max(d[[".powerscale_alpha"]]) + 0.01),
      breaks = c(min(d[[".powerscale_alpha"]]), 1,
                 max(d[[".powerscale_alpha"]])),
      labels = c(
        round(min(d[[".powerscale_alpha"]]), digits = 3),
        "1",
        round(max(d[[".powerscale_alpha"]]), digits = 3)
      )
    )

  if (length(unique(d[[".powerscale_alpha"]])) == 3) {
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
powerscale_plot_dens.default <-
  function(x,
           variable = NULL,
           length = 3,
           resample = FALSE,
           intervals = c(0.5, 0.8, 0.95),
           trim = NULL,
           facet_rows = "component",
           help_text = getOption("priorsense.plot_help_text", TRUE),
           colors = NULL,
           variables_per_page = 6,
           ...
           ) {
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
      variables_per_page = variables_per_page
    )
  }


##' @export
powerscale_plot_dens.powerscaled_sequence <-
  function(x,
           variable = NULL,
           resample = FALSE,
           intervals = c(0.5, 0.8, 0.95),
           trim = NULL,
           facet_rows = "component",
           help_text = getOption("priorsense.plot_help_text", TRUE),
           colors = NULL,
           variables_per_page = getOption(
             "priorsense.plot_variables_per_page", 6
           ),
           ...
           ) {

    # input checks
    checkmate::assert_character(variable, null.ok = TRUE)
    checkmate::assert_logical(resample, len = 1)
    checkmate::assert_logical(help_text, len = 1)
    checkmate::assert_character(colors, len = 3, null.ok = TRUE)
    checkmate::assert_numeric(intervals, null.ok = TRUE)
    checkmate::assert_choice(facet_rows, c("component", "variable"))
    checkmate::assert_number(variables_per_page, lower = 1, null.ok = TRUE)

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

    nvars <- length(variable)

    if (is.null(variables_per_page) || is.infinite(variables_per_page)) {
      variables_per_page <- nvars
    }

    variables_per_page <- floor(variables_per_page)

    n_plots <- ceiling(nvars / variables_per_page)
    plots <- vector(mode = "list", length = n_plots)

    d <- prepare_plot_data(x, variable = variable, resample = resample, ...)

    interval_positions <- data.frame(
      ".powerscale_alpha" = unique(d[[".powerscale_alpha"]]),
      interval_y = as.numeric(as.factor(unique(d[[".powerscale_alpha"]])))
    )

    interval_positions$interval_y <- (0.5 - (interval_positions$interval_y)) /
      (6 * nrow(interval_positions))

    d <- merge(d, interval_positions)

    n_components <- length(unique(d$component))

    for (i in seq_len(n_plots)) {

      sub <- ((i - 1) *
                variables_per_page + 1):min(i * variables_per_page, nvars)
      sub_variable <- variable[sub]

      if (resample || x$resample) {
        resample <- TRUE
      }

      dsub <- d[d$variable %in% sub_variable, ]

      plot <- prepare_plot(dsub, resample = resample, colors = colors, ...) +
        ggplot2::ylab("Density")

      # here we have to draw 2 stat slabs (one with alpha 0 and black fill,
      # one with fill as NA) to get the legend correct see
      # https://github.com/mjskay/ggdist/issues/134
      plot <- plot +
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
        ) +
        ggplot2::xlab(NULL) +
        ggplot2::ylab(NULL)

      if (!is.null(intervals)) {

        plot <- plot +
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
        plot <- plot +
          ggh4x::facet_grid2(
            rows = ggplot2::vars(.data$component),
            cols = ggplot2::vars(.data$variable),
            labeller = ggplot2::labeller(
              component = c(
                likelihood = "Likelihood\npower-scaling",
                prior = "Prior\npower-scaling"
              )
            ),
            independent = "all",
            scales = "free",
            switch = "y"
          )
      } else {
        plot <- plot +
          ggh4x::facet_grid2(
            rows = ggplot2::vars(.data$variable),
            cols = ggplot2::vars(.data$component),
            labeller = ggplot2::labeller(
              component = c(
                likelihood = "Likelihood\npower-scaling",
                prior = "Prior\npower-scaling"
              )
            ),
            independent = "all",
            scales = "free",
            switch = "y"
          )

      }

      if (help_text) {
        plot <- plot +
          ggplot2::ggtitle(
            label = "Power-scaling sensitivity",
            subtitle = paste0(
              "Posterior density estimates depending on ",
              "amount of power-scaling (alpha).\n",
              "Overlapping lines indicate low sensitivity.\n",
              "Wider gaps between lines indicate greater sensitivity.\n",
              "Estimates with high Pareto k (dashed lines) may be inaccurate."
            )
          )
      }

      # additional theming
      plot <- plot +
        ggplot2::theme(
          axis.line.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )

      if (facet_rows == "component") {
        plot <- plot +
          ggplot2::theme(legend.position = "bottom")
      }


      if (!is.null(trim)) {
        position_scales <- lapply(
          variable,
          FUN =
            function(.x, prob) {
              limits <- posterior::quantile2(x$base_draws[[.x]],
                                             probs = c((1 - prob) / 2,
                                                       prob + (1 - prob)/2))
              return(ggplot2::scale_x_continuous(limits = limits))
            }, prob = trim
        )

        if (facet_rows == "component") {
          plot <- plot + ggh4x::facetted_pos_scales(
            x = rep(
              position_scales,
              times = 2)
          )
        } else {
          out <- out + ggh4x::facetted_pos_scales(
            x = rep(
              position_scales,
              each = 2
            )
          )
        }
      }

      plots[[i]] <- plot

    }

    class(plots) <- c("priorsense_plot", class(plots))

    if (length(plots) == 1) {
      plots <- plots[[1]]
    }

    return(plots)
  }

##' @rdname powerscale_plots
##' @export
powerscale_plot_ecdf <- function(x, ...) {
  UseMethod("powerscale_plot_ecdf")
}


##' @export
powerscale_plot_ecdf.default <-
  function(x,
           variable = NULL,
           length = 3,
           resample = FALSE,
           facet_rows = "component",
           help_text = getOption("priorsense.plot_help_text", TRUE),
           colors = NULL,
           variables_per_page = getOption(
             "priorsense.plot_variables_per_page",
             6
           ),
           ...) {
    ps <- powerscale_sequence(x, length = length, ...)
    powerscale_plot_ecdf(
      ps,
      variable = variable,
      resample = resample,
      facet_rows = facet_rows,
      help_text = help_text,
      colors = colors,
      variables_per_page = variables_per_page
    )
  }

##' @rdname powerscale_plots
##' @export
powerscale_plot_ecdf.powerscaled_sequence <-
  function(x,
           variable = NULL,
           resample = FALSE,
           length = 3,
           facet_rows = "component",
           help_text = getOption("priorsense.plot_help_text", TRUE),
           colors = NULL,
           variables_per_page = getOption(
             "priorsense.plot_variables_per_page",
             6
           ),
           ...) {

    # input checks
    checkmate::assert_character(variable, null.ok = TRUE)
    checkmate::assert_logical(resample, len = 1)
    checkmate::assert_logical(help_text, len = 1)
    checkmate::assertCharacter(colors, len = 3, null.ok = TRUE)
    checkmate::assert_choice(facet_rows, c("component", "variable"))
    checkmate::assert_number(variables_per_page, lower = 1, null.ok = TRUE)

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

    nvars <- length(variable)

    if (is.null(variables_per_page) || is.infinite(variables_per_page)) {
      variables_per_page <- nvars
    }

    variables_per_page <- floor(variables_per_page)

    n_plots <- ceiling(nvars / variables_per_page)
    plots <- vector(mode = "list", length = n_plots)

    for (i in seq_len(n_plots)) {

      sub <- ((i - 1) * variables_per_page + 1):min(i * variables_per_page, nvars)
      sub_variable <- variable[sub]

      dsub <- d[d$variable %in% sub_variable, ]

      p <- prepare_plot(dsub, resample = resample, colors = colors, ...) +
        ggplot2::guides(
          linetype = ggplot2::guide_legend(
            title = "Pareto k"
          )
        ) +
        ggplot2::ylab("ECDF") +
        ggplot2::xlab(NULL)

      p <- p +
        ggplot2::stat_ecdf(ggplot2::aes(color = .data[[".powerscale_alpha"]]))

      if (facet_rows == "component") {
        p <- p +
          ggh4x::facet_grid2(
            rows = ggplot2::vars(.data$component),
            cols = ggplot2::vars(.data$variable),
            labeller = ggplot2::labeller(
              component = c(
                likelihood = "Likelihood\npower-scaling",
                prior = "Prior\npower-scaling"
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
              likelihood = "Likelihood\npower-scaling",
              prior = "Prior\npower-scaling"
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
            subtitle = paste0(
              "Posterior ECDF depending on amount of power-scaling (alpha).\n",
              "Overlapping lines indicate low sensitivity.\n",
              "Wider gaps between lines indicate greater sensitivity.\n",
              "Estimates with high Pareto k (dashed lines) may be inaccurate."
            )
          )
      }
      if (facet_rows == "component") {
        p <- p +
          ggplot2::theme(legend.position = "bottom")
      }

      plots[[i]] <- p

    }

    class(plots) <- c("priorsense_plot", class(plots))

    if (length(plots) == 1) {
      plots <- plots[[1]]
    }

    return(plots)
  }


##' @rdname powerscale_plots
##' @export
powerscale_plot_quantities <- function(x, ...) {
  UseMethod("powerscale_plot_quantities")
}

##' @export
powerscale_plot_quantities.default <-
  function(x, variable = NULL,
           quantity = c("mean", "sd"),
           div_measure = "cjs_dist",
           length = 11,
           resample = FALSE,
           measure_args = NULL,
           mcse = TRUE,
           quantity_args = NULL,
           help_text = getOption("priorsense.plot_help_text", TRUE),
           colors = NULL,
           variables_per_page = getOption(
             "priorsense.plot_variables_per_page",
             6
           )
          ,
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
      variables_per_page = variables_per_page
    )
  }

##' @rdname powerscale_plots
##' @export
powerscale_plot_quantities.powerscaled_sequence <-
  function(x, variable = NULL,
           quantity = c("mean", "sd"),
           div_measure = "cjs_dist",
           resample = FALSE,
           measure_args = NULL,
           mcse = TRUE,
           quantity_args = NULL,
           help_text = getOption("priorsense.plot_help_text", TRUE),
           colors = NULL,
           variables_per_page = getOption(
             "priorsense.plot_variables_per_page",
             6
           ),
           ...) {

    checkmate::assertCharacter(variable, null.ok = TRUE)
    checkmate::assertCharacter(quantity)
    checkmate::assertCharacter(div_measure, null.ok = TRUE)
    checkmate::assertLogical(resample, len = 1)
    checkmate::assertList(measure_args, null.ok = TRUE)
    checkmate::assertLogical(mcse, len = 1)
    checkmate::assertList(quantity_args, null.ok = TRUE)
    checkmate::assertLogical(help_text, len = 1)
    checkmate::assertCharacter(colors, len = 2, null.ok = TRUE)
    checkmate::assert_number(variables_per_page, lower = 1, null.ok = TRUE)

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
        c("variable", ".powerscale_alpha", "component",
          "pareto_k", "pareto_kf", "pareto_k_threshold", "n_eff", div_measure)
      )

      mcse_functions <- paste0("mcse_", quantity)

      base_quantities <- summ[[1]][which(summ[[1]][[".powerscale_alpha"]] == 1), ]

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

    powerscale_summary_plot(
      summ,
      variable = variable,
      base_mcse = base_mcse,
      help_text = help_text,
      colors = colors,
      variables_per_page = variables_per_page,
      ...
    )

  }
##' power-scale summary plot
##'
##' internal function for powerscale_plot_quantities
##' @param x data frame of plotting data
##' @param variable character vector specifying variables to plot
##' @param base_mcse base Monte Carlo standard error estimates
##' @param help_text boolean flag indicating whether or not to show
##'   helper text
##' @param colors character vector specifying colors
##' @param variables_per_page number specifying how many variables to
##'   plot on each page
##' @param ... unused
##' @return ggplot object
##' @keywords internal
##' @noRd
powerscale_summary_plot <- function(x,
                                    variable,
                                    base_mcse = NULL,
                                    help_text,
                                    colors,
                                    variables_per_page,
                                    ...) {


  nvars <- length(variable)

  if (is.null(variables_per_page) || is.infinite(variables_per_page)) {
    variables_per_page <- nvars
  }

  variables_per_page <- floor(variables_per_page)

  n_plots <- ceiling(nvars / variables_per_page)
  plots <- vector(mode = "list", length = n_plots)

  pareto_k_colours <- colors

  # get default quantities
  quantities <- setdiff(
    colnames(x[[1]]),
    c("variable", ".powerscale_alpha", "component",
      "pareto_k", "pareto_kf", "n_eff", "pareto_k_threshold")
  )

  for (i in seq_len(n_plots)) {

    sub <- ((i - 1) * variables_per_page + 1):min(i * variables_per_page, nvars)
    sub_variable <- variable[sub]
    # select only specified variables
    xsub <- x[[1]][x[[1]][["variable"]] %in% sub_variable, ]

    sub_mcse <- base_mcse[base_mcse$variable %in% sub_variable, ]

    # reshape quantities for plotting
    summaries <- stats::reshape(
      data = xsub,
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
    points <- summaries[
      summaries[[".powerscale_alpha"]] == min(summaries[[".powerscale_alpha"]]) |
        summaries[[".powerscale_alpha"]] == max(summaries[[".powerscale_alpha"]]),
      ]

    p <- ggplot2::ggplot(
      data = summaries,
      mapping = ggplot2::aes(x = .data[[".powerscale_alpha"]], y = .data$value)
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
          x = .data[[".powerscale_alpha"]],
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
        limits = c(
          0.95 * min(summaries[[".powerscale_alpha"]]),
          1.05 * max(summaries[[".powerscale_alpha"]])
        ),
        breaks = c(
          min(summaries[[".powerscale_alpha"]]),
          1,
          max(summaries[[".powerscale_alpha"]])
        ),
        labels = round(c(
          min(summaries[[".powerscale_alpha"]]),
          1,
          max(summaries[[".powerscale_alpha"]])
        ), digits = 3),
        name = "Power-scaling alpha"
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
          subtitle = paste0(
            "Posterior quantities depending on amount of power-scaling (alpha).\n",
            "Horizontal lines indicate low sensitivity.\n",
            "Steeper lines indicate greater sensitivity.\n",
            "Estimates with high Pareto k (highlighted) may be inaccurate.")
          
        )
    }

    if (!is.null(sub_mcse)) {

      p <- p +
        ggplot2::scale_linetype_manual(values = "dashed", name = NULL) +
        ggplot2::geom_hline(
          ggplot2::aes(
            yintercept = .data$mcse_min,
            linetype = "+/-2MCSE"
          ),
          data = sub_mcse,
          color = "black"
        ) +
        ggplot2::geom_hline(
          ggplot2::aes(
            yintercept = .data$mcse_max,
            linetype = "+/-2MCSE"
          ),
          data = sub_mcse,
          color = "black"
        )
    }

    plots[[i]] <- p
  }

  class(plots) <- c("priorsense_plot", class(plots))

  if (length(plots) == 1) {
    plots <- plots[[1]]
  }

  return(plots)
}

##' @exportS3Method
plot.priorsense_plot <- function(x,
                                 ask = getOption("priorsense.plot_ask", TRUE),
                                 ...) {

  grDevices::devAskNewPage(ask = FALSE)
  on.exit(grDevices::devAskNewPage(ask = FALSE))

  for (i in seq_along(x)) {
    plot(x[[i]], newpage = TRUE)
    if (i == 1) {
      grDevices::devAskNewPage(ask = ask)
    }
  }
  invisible(x)
}


##' @exportS3Method
print.priorsense_plot <- plot.priorsense_plot
