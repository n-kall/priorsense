# priorsense: Prior (and likelihood) diagnostics and sensitivity analysis

The priorsense package provides functions for prior and likelihood
sensitivity analysis of Bayesian models. Currently it implements methods
to determine the sensitivity of the posterior to power-scaling
perturbations of the prior and likelihood and is the first
implementation of the method described in Kallioinen et al. (2023).

## Details

The main diagnostic function provided by priorsense is
[`powerscale_sensitivity`](https://n-kall.github.io/priorsense/reference/powerscale-sensitivity.md).
Given a fitted model or draws object, it computes the powerscaling
sensitivity diagnostic described in Kallioinen et al. (2023). It does so
by perturbing the prior and likelihood and computing the effect on the
posterior, without needing to refit the model (using Pareto smoothed
importance sampling and importance weighted moment matching; Vehtari et
al. 2022, Paananen et al. 2021).

In addition, visual diagnostics are available by first using
[`powerscale_sequence`](https://n-kall.github.io/priorsense/reference/powerscale-overview.md)
to create a sequence of perturbed posteriors, and then a plot function
such as
[`powerscale_plot_ecdf`](https://n-kall.github.io/priorsense/reference/powerscale_plots.md)
to visualise the change.

The following global options are available:

- `priorsense.plot_help_text`: If `TRUE` (the default), priorsense plots
  will include a title and explanatory text. If `FALSE` they will not.

- `priorsense.plot_variables_per_page`: Number specifying the maximum
  number of variables to be plotted on one page of a plot.

- `priorsense.plot_ask`: If `TRUE` (the default), when multiple pages
  are plotted input is required before each subsequent page is rendered.
  If `FALSE` no input is required.

## References

Kallioinen, N., Paananen, T., Bürkner, P-C., Vehtari, A. (2023).
Detecting and diagnosing prior and likelihood sensitivity with
power-scaling perturbations. *Statistics and Computing*. 34(57).
`doi:10.1007/s11222-023-10366-5`

Vehtari, A., Simpson, D., Gelman, A., Yao, Y., and Gabry, J. (2024).
Pareto smoothed importance sampling. *Journal of Machine Learning
Research*. 25(72). `https://jmlr.org/papers/v25/19-556.html`

Paananen, T., Piironen, J., Bürkner, P-C., Vehtari, A. (2021).
Implicitly adaptive importance sampling. *Statistics and Computing*.
31(16). `doi:10.1007/s11222-020-09982-2`

## See also

[`powerscale_sensitivity`](https://n-kall.github.io/priorsense/reference/powerscale-sensitivity.md)
[`powerscale_sequence`](https://n-kall.github.io/priorsense/reference/powerscale-overview.md)
[`powerscale`](https://n-kall.github.io/priorsense/reference/powerscale-overview.md)
[`powerscale_plot_ecdf`](https://n-kall.github.io/priorsense/reference/powerscale_plots.md)
[`powerscale_plot_dens`](https://n-kall.github.io/priorsense/reference/powerscale_plots.md)
[`powerscale_plot_quantities`](https://n-kall.github.io/priorsense/reference/powerscale_plots.md)

## Author

**Maintainer**: Noa Kallioinen <noa.kallioinen@aalto.fi> \[copyright
holder\]

Authors:

- Topi Paananen

- Paul-Christian Bürkner

- Aki Vehtari

Other contributors:

- Frank Weber \[contributor\]
