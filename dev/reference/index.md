# Package index

## Package description

- [`priorsense-package`](https://mc-stan.org/priorsense/dev/reference/priorsense-package.md)
  [`priorsense`](https://mc-stan.org/priorsense/dev/reference/priorsense-package.md)
  : priorsense: Prior (and likelihood) diagnostics and sensitivity
  analysis

## Numerical power-scaling sensitivity checks

Numerical sensitivity checks for prior and likelihood

- [`powerscale_sensitivity()`](https://mc-stan.org/priorsense/dev/reference/powerscale-sensitivity.md)
  : Power-scaling sensitivity analysis
- [`powerscale_derivative()`](https://mc-stan.org/priorsense/dev/reference/powerscale_derivative.md)
  : Derivative with respect to power-scaling

## Graphical checks

Plots for power-scaling sensitivity checks

- [`powerscale_plot_dens()`](https://mc-stan.org/priorsense/dev/reference/powerscale-plots.md)
  [`powerscale_plot_ecdf()`](https://mc-stan.org/priorsense/dev/reference/powerscale-plots.md)
  [`powerscale_plot_quantities()`](https://mc-stan.org/priorsense/dev/reference/powerscale-plots.md)
  [`plot(`*`<powerscaled_sequence>`*`)`](https://mc-stan.org/priorsense/dev/reference/powerscale-plots.md)
  : Diagnostic plots for power-scaling sensitivity

## Example models

Provides example model code and data for different PPLs compatible with
priorsense

- [`example_powerscale_model()`](https://mc-stan.org/priorsense/dev/reference/example_powerscale_model.md)
  : Example models for power-scaling sensitivity

## Other functions for exploring sensitivity

- [`powerscale()`](https://mc-stan.org/priorsense/dev/reference/powerscale-overview.md)
  [`powerscale_sequence()`](https://mc-stan.org/priorsense/dev/reference/powerscale-overview.md)
  : Prior/likelihood power-scaling perturbation
- [`powerscale_gradients()`](https://mc-stan.org/priorsense/dev/reference/powerscale-gradients.md)
  : Power-scale gradients
- [`create_priorsense_data()`](https://mc-stan.org/priorsense/dev/reference/create-priorsense-data.md)
  : Create data structure for priorsense

## Helper functions

- [`create_priorsense_data()`](https://mc-stan.org/priorsense/dev/reference/create-priorsense-data.md)
  : Create data structure for priorsense
- [`log_lik_draws()`](https://mc-stan.org/priorsense/dev/reference/log_lik_draws.md)
  : Extract log likelihood draws
- [`log_prior_draws()`](https://mc-stan.org/priorsense/dev/reference/log_prior_draws.md)
  : Extract log prior draws
- [`predictions_as_draws()`](https://mc-stan.org/priorsense/dev/reference/predictions_as_draws.md)
  : brms predictions as draws
- [`cjs_dist()`](https://mc-stan.org/priorsense/dev/reference/cjs_dist.md)
  : Cumulative Jensen-Shannon divergence
