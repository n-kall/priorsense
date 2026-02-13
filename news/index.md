# Changelog

## priorsense 1.2.0

CRAN release: 2025-10-28

- Allow either `variable` or `variables` to be accepted to subset
- Fix issue with plotting of high Pareto-k values in quantities plot
- Use new weighted ecdf from ggplot2, which fixes issue with previous
  implementation

## priorsense 1.1.1

CRAN release: 2025-08-22

- Maintain compatibility with ggplot2

## priorsense 1.1.0

CRAN release: 2025-04-10

- Add pagination of plots when there are many variables
- Selection of priors updated to work with brms prior tags
- Fix issue with component argument in powerscale_sensitivity
- Fix MCSE not displaying in powerscale_plot_quantities
- Switch to quarto for vignettes
- Add support for rjags objects from R2jags

## priorsense 1.0.4

CRAN release: 2024-11-01

- Fix an issue where `lower_alpha` was not taken into account when
  calculating the gradient of the divergence.

## priorsense 1.0.3

CRAN release: 2024-10-01

- Fix issue with model parameter named “alpha”

## priorsense 1.0.2

CRAN release: 2024-07-16

- Fix to Pareto smoothing of weights
- Improvements to vignette

## priorsense 1.0.1

CRAN release: 2024-06-24

- Fixes to documentation and description

## priorsense 1.0.0

CRAN release: 2024-06-20

- First stable release
