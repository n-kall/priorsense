# priorsense

### Efficient prior and likelihood sensitivity checks

**priorsense** is an R package that provides tools for prior diagnostics
and sensitivity analysis of Bayesian models [(Kallioinen et al.,
2026)](https://doi.org/10.21105/joss.11036).

It currently includes functions for performing power-scaling sensitivity
analysis on fitted Bayesian models. This is a way to check how sensitive
a posterior is to perturbations of the prior and likelihood and diagnose
the cause of sensitivity. For efficient computation, power-scaling
sensitivity analysis relies on Pareto smoothed importance sampling
(Vehtari et al., 2024) and importance weighted moment matching (Paananen
et al., 2021).

Priorsense supports fitted model objects from `brms`, Stan (`cmdstanr`
and `rstan`), JAGS (`jagsUI` and `R2jags`) and NIMBLE, along with
posterior draws from other software. Power-scaling sensitivity analysis
checks are described in [Kallioinen et
al. (2023)](https://doi.org/10.1007/s11222-023-10366-5).

### Resources

- Check the [getting started
  vignette](https://n-kall.github.io/priorsense/articles/getting_started.html)
  for a simple example

- For a more detailed modelling example see
  [here](https://n-kall.github.io/priorsense/articles/airquality.html)

### Installation

Download the stable version from CRAN with:

``` r

install.packages("priorsense")
```

Download the development version from [GitHub](https://github.com/)
with:

``` r

# install.packages("pak")
pak::pkg_install("n-kall/priorsense@development")
```

### Contributing

Contributions are welcome! If you find a bug or have an idea for a
feature, open an issue. If you are able to fix an issue, fork the
repository and make a pull request to the `development` branch.

### References

Noa Kallioinen, Topi Paananen, Paul-Christian Bürkner, Aki Vehtari
(2026). priorsense: Efficient prior and likelihood sensitivity checks
for Bayesian models in R. Journal of Open Source Software. 11, 123.
<https://doi.org/10.21105/joss.11036>

Noa Kallioinen, Topi Paananen, Paul-Christian Bürkner, Aki Vehtari
(2023). Detecting and diagnosing prior and likelihood sensitivity with
power-scaling. Statistics and Computing. 34, 57.
<https://doi.org/10.1007/s11222-023-10366-5>

Topi Paananen, Juho Piironen, Paul-Christian Bürkner, Aki Vehtari
(2021). Implicitly adaptive importance sampling. Statistics and
Computing 31, 16. <https://doi.org/10.1007/s11222-020-09982-2>

Aki Vehtari, Daniel Simpson, Andrew Gelman, Yuling Yao, Jonah Gabry
(2024). Pareto smoothed importance sampling. Journal of Machine Learning
Research. 25, 72. <https://jmlr.org/papers/v25/19-556.html>
