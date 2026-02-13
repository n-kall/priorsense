# Power-scaling sensitivity analysis

Calculates the prior/likelihood sensitivity based on power-scaling
perturbations. This is done using importance sampling (and optionally
moment matching).

## Usage

``` r
powerscale_sensitivity(x, ...)

# Default S3 method
powerscale_sensitivity(
  x,
  variable = NULL,
  lower_alpha = 0.99,
  upper_alpha = 1.01,
  div_measure = "cjs_dist",
  measure_args = list(),
  component = c("prior", "likelihood"),
  sensitivity_threshold = 0.05,
  moment_match = FALSE,
  k_threshold = 0.5,
  resample = FALSE,
  transform = NULL,
  prediction = NULL,
  prior_selection = NULL,
  likelihood_selection = NULL,
  log_prior_name = "lprior",
  log_lik_name = "log_lik",
  num_args = NULL,
  ...
)

# S3 method for class 'priorsense_data'
powerscale_sensitivity(
  x,
  variable = NULL,
  lower_alpha = 0.99,
  upper_alpha = 1.01,
  div_measure = "cjs_dist",
  measure_args = list(),
  component = c("prior", "likelihood"),
  sensitivity_threshold = 0.05,
  moment_match = FALSE,
  k_threshold = 0.5,
  resample = FALSE,
  transform = NULL,
  prediction = NULL,
  prior_selection = NULL,
  likelihood_selection = NULL,
  num_args = NULL,
  ...
)

# S3 method for class 'CmdStanFit'
powerscale_sensitivity(x, ...)

# S3 method for class 'stanfit'
powerscale_sensitivity(x, ...)
```

## Arguments

- x:

  Model fit object or priorsense_data object.

- ...:

  Further arguments passed to functions.

- variable:

  Character vector of variables to check.

- lower_alpha:

  Lower alpha value for gradient calculation.

- upper_alpha:

  Upper alpha value for gradient calculation.

- div_measure:

  Character (case sensitive) specifying the divergence measure to use.
  The following methods are implemented:

  - `"cjs_dist"`: Cumulative Jensen-Shannon distance. Default method.
    See function `cjs_dist` for more details.

  - `"js_dist"`: Jensen-Shannon distance.

  - `"js_div"`: Jensen-Shannon divergence.

  - `"hellinger_dist"`: Hellinger distance.

  - `"kl_dist"`: Kullback-Leibler distance.

  - `"kl_div"`: Kullback-Leibler divergence.

  - `"ks_dist"`: Kolmogorov-Smirnov distance.

  - `"hellinger_dist"`: Hellinger distance.

  - `"ws_dist"`: Wassterstein distance (pass
    `measure_args = list(p = N)`) for a different order, where N is the
    order.

- measure_args:

  Named list of further arguments passed to divergence measure
  functions.

- component:

  Character vector specifying component(s) to scale (default is both
  "prior" and "likelihood").

- sensitivity_threshold:

  Threshold for flagging variable as sensitive to power-scaling.

- moment_match:

  Logical; Indicate whether or not moment matching should be performed.
  Can only be TRUE if `is_method` is "psis".

- k_threshold:

  Threshold value for Pareto k values above which the moment matching
  algorithm is used. Default is 0.5.

- resample:

  Logical; Indicate whether or not draws should be resampled based on
  calculated importance weights.

- transform:

  Indicate a transformation of posterior draws to perform before
  sensitivity analysis. Either "scale" or "whiten".

- prediction:

  Function taking the model fit and returning a draws_df of predictions
  to be appended to the posterior draws

- prior_selection:

  Vector specifying partitions of component to be included in
  power-scaling. Default is NULL, which takes all partitions. If this is
  a character, then it is appended to the variable name (specified by
  `log_prior_name`) with an `_` between them. If numeric, then it is
  appended inside `[]`.

- likelihood_selection:

  Vector specifying partitions of component to be included in
  power-scaling. Default is NULL, which takes all partitions. If this is
  a character, then it is appended to the variable name (specified by
  `log_lik_name`) with an `_` between them. If numeric, then it is
  appended inside `[]`.

- log_prior_name:

  Character (case sensitive) specifying name of the variable storing the
  log prior evaluations

- log_lik_name:

  Character (case sensitive) specifying name of the variable storing the
  log likelihood evaluations

- num_args:

  (named list) Optional arguments passed to
  [num()](https://tibble.tidyverse.org/reference/num.html) for pretty
  printing of summaries. Can be controlled globally via the
  `posterior.num_args` [option](https://rdrr.io/r/base/options.html).

## Value

Table of sensitivity values for each specified variable.

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

## Examples

``` r
ex <- example_powerscale_model()
powerscale_sensitivity(ex$draws)
#> Sensitivity based on cjs_dist
#> Prior selection: all priors
#> Likelihood selection: all data
#> 
#>  variable prior likelihood                     diagnosis
#>        mu 0.433      0.641 potential prior-data conflict
#>     sigma 0.360      0.674 potential prior-data conflict
```
