
<!-- README.md is generated from README.Rmd. Please edit that file -->
<img src='man/figures/logo.png' align="right" height="139" />

# priorsense

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) [![CRAN status](https://www.r-pkg.org/badges/version/priorsense)](https://CRAN.R-project.org/package=priorsense) [![R-CMD-check](https://github.com/n-kall/priorsense/workflows/R-CMD-check/badge.svg)](https://github.com/n-kall/priorsense/actions) <!-- badges: end -->

## Overview

priorsense provides tools for prior diagnostics and sensitivity analysis.

It currently includes functions for performing power-scaling sensitivity analysis on Stan models. This way to check how sensitive a posterior is to perturbations of the prior and likelihood and diagnose the cause of sensitivity. For efficient computation, power-scaling sensitivity analysis relies on Pareto smoothed importance sampling (Vehtari et al., 2019) and importance weighted moment matching (Paananen et al., 2021).

Power-scaling sensitivity analysis and priorsense are described in Kallioinen et al. (2021).

## Installation

Download the development version from [GitHub](https://github.com/) with:

`r, eval = F # install.packages("remotes") remotes::install_github("n-kall/priorsense")`

## Usage

priorsense currently works best with Stan models created with rstan. However there is partial support for models fit with cmdstanr.

### Example

Consider the following model (available via`example_powerscale_model("univariate_normal")`:

*y* ∼ normal(*μ*, *σ*)
*μ* ∼ normal(0, 1)
*σ* ∼ normal<sup>+</sup>(0, 2.5)

We have 100 data points for *y* We first fit the model using Stan:

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
  // joint prior specification required for cmdstanr
  log_prior = normal_lpdf(mu | 0, 1) +
    normal_lpdf(sigma | 0, 2.5);
}
```

``` r
library(priorsense)
normal_model <- example_powerscale_model("univariate_normal")
```

### Fitting with rstan and cmdstanr

``` r
rs_fit <- rstan::stan(
  model_code = normal_model$model_code,
  data = normal_model$data,
  refresh = FALSE,
  seed = 123
)

cs_model <- cmdstanr::cmdstan_model(
  stan_file = cmdstanr::write_stan_file(normal_model$model_code)
)

cs_fit <- cs_model$sample(data = normal_model$data, refresh = 0, seed = 123) 
```

Once fit, a sensitivity analysis can be performed as follows. For cmdstanr models it is currently necessary to specify `log_prior_fn = extract_log_prior`and have the joint log prior specified in the Stan code (as above)

``` r
powerscale_sensitivity(rs_fit, variables = c("mu", "sigma"))


powerscale_sensitivity(cs_fit, variables = c("mu", "sigma"), log_prior_fn = extract_log_prior)
```

## References

Kallioinen, N., Paananen, T., Bürkner P-C., and Vehtari, A. (2021). Detecting and diagnosing prior and likelihood sensitivity with power-scaling. preprint [arXiv:2107.14054](https://arxiv.org/abs/2107.14054)

Paananen, T., Piironen, J., Bürkner P-C., and Vehtari, A. (2021). Implicitly adaptive importance sampling. Statistics and Computing 31, 16. <https://doi.org/10.1007/s11222-020-09982-2>

Vehtari, A., Simpson, D., Gelman, A., Yao, Y., and Gabry, J. (2019). Pareto smoothed importance sampling. preprint [arXiv:1507.02646](https://arxiv.org/abs/1507.02646)
