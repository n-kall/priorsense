# Prior/likelihood power-scaling perturbation

Estimate posterior draws based on power-scaling perturbations of prior
or likelihood using importance sampling (and optionally moment
matching).

## Usage

``` r
powerscale(x, ...)

# Default S3 method
powerscale(
  x,
  component,
  alpha,
  moment_match = FALSE,
  k_threshold = NULL,
  resample = FALSE,
  transform = NULL,
  prediction = NULL,
  variable = NULL,
  variables = NULL,
  selection = NULL,
  log_prior_name = "lprior",
  log_lik_name = "log_lik",
  separator = "_",
  ...
)

# S3 method for class 'priorsense_data'
powerscale(
  x,
  component,
  alpha,
  moment_match = FALSE,
  k_threshold = NULL,
  resample = FALSE,
  transform = NULL,
  prediction = NULL,
  variable = NULL,
  variables = NULL,
  selection = NULL,
  log_prior_name = "lprior",
  log_lik_name = "log_lik",
  separator = "_",
  ...
)

powerscale_sequence(x, ...)

# Default S3 method
powerscale_sequence(
  x,
  lower_alpha = 0.8,
  upper_alpha = 1/lower_alpha,
  length = 3,
  variable = NULL,
  variables = NULL,
  component = c("prior", "likelihood"),
  moment_match = FALSE,
  k_threshold = 0.5,
  resample = FALSE,
  transform = NULL,
  prediction = NULL,
  auto_alpha_range = FALSE,
  symmetric = TRUE,
  prior_selection = NULL,
  likelihood_selection = NULL,
  ...
)

# S3 method for class 'priorsense_data'
powerscale_sequence(
  x,
  lower_alpha = 0.8,
  upper_alpha = 1/lower_alpha,
  length = 3,
  variable = NULL,
  variables = NULL,
  component = c("prior", "likelihood"),
  moment_match = FALSE,
  k_threshold = NULL,
  resample = FALSE,
  transform = NULL,
  prediction = NULL,
  auto_alpha_range = FALSE,
  symmetric = TRUE,
  prior_selection = NULL,
  likelihood_selection = NULL,
  ...
)
```

## Arguments

- x:

  A fitted model object.

- ...:

  Further arguments passed to internal functions.

- component:

  Component to be power-scaled (either "prior" or "likelihood"). For
  powerscale_sequence, this can be both "prior" and "likelihood".

- alpha:

  Value by which to power-scale specified component. (likelihood/prior).

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

- variable:

  Vector of variable names to return estimated posterior draws for. If
  `NULL` all variables will be included.

- variables:

  Alias of `variable`.

- selection:

  Vector specifying partitions of component to be included in
  power-scaling. Default is NULL, which takes all partitions. If this is
  a character, then it is appended to the variable name
  (`log_prior_name` or `log_lik_name`) with an `_` between them.

- log_prior_name:

  Character (case sensitive) specifying name of the variable storing the
  log prior evaluations

- log_lik_name:

  Character (case sensitive) specifying name of the variable storing the
  log likelihood evaluations

- separator:

  Character specifying the separator between the component name and the
  partition. The default is `"_"`.

- lower_alpha:

  Lower power-scaling alpha value in sequence.

- upper_alpha:

  Upper power-scaling alpha value in sequence.

- length:

  Total length of alpha sequence, including 1.0.

- auto_alpha_range:

  Boolean. Restrict range to ensure Pareto-k values below threshold?

- symmetric:

  Boolean. Should the alpha range be symmetrical around alpha = 1 on
  log-scale? If `TRUE`, the length of the range will be always be odd,
  and will be equal to `length - 1` if specified `length` is even.

- prior_selection:

  Vector specifying partitions of component to be included in
  power-scaling. Default is NULL, which takes all partitions. If this is
  a character, then it is appended to the variable name (specified by
  `log_prior_name`) with a separator between them. If numeric, then it
  is appended inside `[]`.

- likelihood_selection:

  Vector specifying partitions of component to be included in
  power-scaling. Default is NULL, which takes all partitions. If this is
  a character, then it is appended to the variable name (specified by
  `log_lik_name`) with a separator between them. If numeric, then it is
  appended inside `[]`.

## Value

A `powerscaled_draws` or `powerscaled_sequence` object, which contains
the estimated posterior draws resulting from the power-scaling
perturbations and details of the perturbation and estimation methods.

## Details

Power-scaling of the prior or likelihood is defined as exponentiating
the prior or likelihood to some value alpha. The effect on the posterior
is then estimated through Pareto-smoothed importance sampling and
optionally importance weighted moment matching. In order to perform the
calculations, the log-prior and log-likelihood evaluations must be
present in the object. For a general introduction to the package see see
`vignette("priorsense")`, and for details on the method see Kallioinen
et al. (2023). For details on Pareto-smoothed importance sampling and
importance weighted moment matching, see Vehtari et al. (2024) and
Paananen et al. (2021), respectively.

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

[powerscale-plots](https://n-kall.github.io/priorsense/reference/powerscale-plots.md)

## Examples

``` r
ex <- example_powerscale_model()

powerscale(ex$draws, component = "prior", alpha = 0.5)
#> # A draws_df: 1000 iterations, 4 chains, and 2 variables
#>     mu sigma
#> 1  9.5  0.76
#> 2  9.3  0.75
#> 3  9.3  0.67
#> 4  9.4  0.71
#> 5  9.4  0.73
#> 6  9.9  0.82
#> 7  9.6  0.82
#> 8  9.7  0.75
#> 9  9.6  0.76
#> 10 9.4  1.05
#> # ... with 3990 more draws
#> # ... hidden reserved variables {'.log_weight', '.chain', '.iteration', '.draw'}
#> 
#> power-scaling
#>  alpha: 0.5 
#>  scaled component: prior 
#>  selection: 
#>  pareto-k: 0.14 
#>  pareto-k threshold: 0.72 
#>  resampled: FALSE 
#>  transform: identity 

powerscale_sequence(ex$draws)
#> base draws:
#> # A draws_df: 1000 iterations, 4 chains, and 2 variables
#>     mu sigma
#> 1  9.5  0.76
#> 2  9.3  0.75
#> 3  9.3  0.67
#> 4  9.4  0.71
#> 5  9.4  0.73
#> 6  9.9  0.82
#> 7  9.6  0.82
#> 8  9.7  0.75
#> 9  9.6  0.76
#> 10 9.4  1.05
#> # ... with 3990 more draws
#> # ... hidden reserved variables {'.chain', '.iteration', '.draw'}
#> 
#> power-scaling
#>  alpha range: [0.8, 1.25]
#>  length of sequence: 3 
#>  scaled component: likelihood 
#>  scaled component: prior 
#>  transform: identity 
```
