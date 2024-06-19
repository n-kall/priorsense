library(hexSticker)
library(priorsense)
library(bayesplot)
library(ggplot2)
library(cmdstanr)

theme_set(bayesplot_theme_get())

x <- example_powerscale_model()

m <- cmdstan_model(write_stan_file(x$model_code), force_recompile = TRUE)

f <- m$sample(data = x$data, iter_sampling = 20000, seed = 123)

d <- f$draws()

ps <- powerscale_sequence(f, lower_alpha = 0.77, length = 5, component = "prior", variable = "mu", moment_match = FALSE)

p <- powerscale_plot_dens(ps, help_text = FALSE, intervals = NULL) +
  theme(legend.key = element_blank(), legend.text = element_blank(), legend.title = element_blank(),
        axis.title = element_blank(), axis.line = element_blank(), strip.text = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank()) +
  guides(colour = "none", linetype = "none")

sticker(p, package = "priorsense", filename = "man/figures/logo.png", p_size = 16, p_color = "#221F21", h_fill = "white", h_color = "black", s_width = 7.5, s_height = 1.3, s_x = -1.15, s_y = 1.05, p_x = 0.7, p_y = 1.33)
