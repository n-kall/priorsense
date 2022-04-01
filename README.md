
<!-- README.md is generated from README.Rmd. Please edit that file -->
<img src='man/figures/logo.png' align="right" height="139" />

# priorsense

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) [![CRAN status](https://www.r-pkg.org/badges/version/priorsense)](https://CRAN.R-project.org/package=priorsense) [![R-CMD-check](https://github.com/n-kall/priorsense/workflows/R-CMD-check/badge.svg)](https://github.com/n-kall/priorsense/actions) <!-- badges: end -->

## Overview

priorsense provides tools for prior diagnostics and sensitivity analysis.

It currently includes functions for performing power-scaling sensitivity analysis on Stan models. This is a way to check how sensitive a posterior is to perturbations of the prior and likelihood and diagnose the cause of sensitivity. For efficient computation, power-scaling sensitivity analysis relies on Pareto smoothed importance sampling (Vehtari et al., 2021) and importance weighted moment matching (Paananen et al., 2021).

Power-scaling sensitivity analysis and priorsense are described in Kallioinen et al. (2021).

## Installation

Download the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("n-kall/priorsense")
```

## Usage

priorsense currently works with models created with rstan, cmdstanr or brms. However, moment matching currently does not work with cmdstan models.

### Example

Consider a simple univariate model with unknown mu and sigma fit to some data y (available via`example_powerscale_model("univariate_normal")`:

``` stan
data {
  int<lower=1> N;
  real y[N];
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  // priors
  target += normal_lpdf(mu | 0, 1);
  target += normal_lpdf(sigma | 0, 2.5);
  // likelihood
  target += normal_lpdf(y | mu, sigma);
}
generated quantities {
  vector[N] log_lik;
  // likelihood
  real log_prior;
  for (n in 1:N) log_lik[n] =  normal_lpdf(y[n] | mu, sigma);
  // joint prior specification
  log_prior = normal_lpdf(mu | 0, 1) +
    normal_lpdf(sigma | 0, 2.5);
}
```

We first fit the model using Stan:

``` r
library(priorsense)

normal_model <- example_powerscale_model("univariate_normal")

fit <- rstan::stan(
  model_code = normal_model$model_code,
  data = normal_model$data,
  refresh = FALSE,
  seed = 123
)
```

Once fit, sensitivity can be checked as follows:

``` r
powerscale_sensitivity(fit, variables = c("mu", "sigma"))
#> Sensitivity based on cjs_dist:
#> # A tibble: 2 × 4
#>   variable  prior likelihood diagnosis          
#>   <chr>     <dbl>      <dbl> <chr>              
#> 1 mu       0.167      0.228  prior-data conflict
#> 2 sigma    0.0233     0.0627 -
```

To visually inspect changes to the posterior, first create a power-scaling sequence and then use a plotting function. Here we use moment matching in order to arrive at more stable estimates.

``` r
pss <- powerscale_sequence(fit)
powerscale_plot_ecdf(pss, variables = c("mu", "sigma"))
```

<img src="man/figures/README-sequence-nomm-1.png" width="50%" height="50%" />

In case there are Pareto k values above 0.5, indicating that those estimates should not be trusted, moment matching may help at the expense of slightly more computation:

``` r
pss_mm <- powerscale_sequence(fit, moment_match = TRUE)
powerscale_plot_ecdf(pss_mm, variables = c("mu", "sigma"))
```

<img src="man/figures/README-sequence-mm-1.png" width="50%" height="50%" />

## References

Kallioinen, N., Paananen, T., Bürkner P-C., and Vehtari, A. (2021). Detecting and diagnosing prior and likelihood sensitivity with power-scaling. preprint [arXiv:2107.14054](https://arxiv.org/abs/2107.14054)

Paananen, T., Piironen, J., Bürkner P-C., and Vehtari, A. (2021). Implicitly adaptive importance sampling. Statistics and Computing 31, 16. <https://doi.org/10.1007/s11222-020-09982-2>

Vehtari, A., Simpson, D., Gelman, A., Yao, Y., and Gabry, J. (2021). Pareto smoothed importance sampling. preprint [arXiv:1507.02646](https://arxiv.org/abs/1507.02646)
