library(cmdstanr)
library(priorsense)
library(hexSticker)

e <- example_powerscale_model()

m <- cmdstan_model(cmdstanr::write_stan_file(e$model_code))

fit <- m$sample(data = e$data, refresh = 0, iter_warmup = 1000, iter_sampling = 30000)

ps <- powerscale_sequence(fit, alpha_step = 0.2, log_prior_fn = extract_log_prior, component = "prior")

p <- powerscale_plot_dens(ps, variables = "mu") + theme_void() + theme_transparent() + theme(legend.position = "none", strip.text = element_blank())

sticker(p, package="priorsense", p_size=20, s_x=1, s_y=.9, s_width=1.3, s_height=1, h_fill = "white", h_color = "black", p_color = "black", filename = "hex.png")

