# Power-scale gradients

Calculate the numerical derivative of posterior quantities/divergence
with respect to power-scaling the specified component (prior or
likelihood). This is done using importance sampling (and optionally
moment matching).

## Usage

``` r
powerscale_gradients(x, ...)

# Default S3 method
powerscale_gradients(
  x,
  log_prior_name = "lprior",
  log_lik_name = "log_lik",
  ...
)

# S3 method for class 'priorsense_data'
powerscale_gradients(
  x,
  variable = NULL,
  component = c("prior", "likelihood"),
  type = c("quantities", "divergence"),
  lower_alpha = 0.99,
  upper_alpha = 1.01,
  div_measure = "cjs_dist",
  measure_args = list(),
  moment_match = FALSE,
  k_threshold = NULL,
  resample = FALSE,
  transform = NULL,
  prediction = NULL,
  scale = FALSE,
  prior_selection = NULL,
  likelihood_selection = NULL,
  ...
)
```

## Arguments

- x:

  Model fit or draws object.

- ...:

  Further arguments passed to functions.

- log_prior_name:

  Character (case sensitive) specifying name of the variable storing the
  log prior evaluations

- log_lik_name:

  Character (case sensitive) specifying name of the variable storing the
  log likelihood evaluations

- variable:

  Variables to compute sensitivity of. If NULL (default) sensitivity is
  computed for all variables.

- component:

  Component to power-scale (prior or likelihood).

- type:

  type of sensitivity to measure ("distance", "quantity"). Multiple
  options can be specified at the same time.

- lower_alpha:

  lower power to scale component by, should be \< 1 (default is 0.9).

- upper_alpha:

  upper power to scale component by, should be \> 1 (default is 1.1).

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

- scale:

  logical scale quantity gradients by base posterior standard deviation.

- prior_selection:

  Numeric vector specifying which priors to consider.

- likelihood_selection:

  Numeric vector specifying which likelihoods to consider.

## Value

Maximum of the absolute derivatives above and below alpha = 1.

## Examples

``` r
ex <- example_powerscale_model()
drw <- ex$draws

powerscale_gradients(drw)
#> $multivariate_divergence
#> $multivariate_divergence$prior
#> NULL
#> 
#> $multivariate_divergence$likelihood
#> NULL
#> 
#> 
#> $divergence
#> $divergence$prior
#> # A tibble: 2 × 2
#>   variable cjs_dist
#>   <chr>       <dbl>
#> 1 mu          0.433
#> 2 sigma       0.360
#> 
#> $divergence$likelihood
#> # A tibble: 2 × 2
#>   variable cjs_dist
#>   <chr>       <dbl>
#> 1 mu          0.641
#> 2 sigma       0.674
#> 
#> 
#> $quantities
#> $quantities$prior
#> # A tibble: 2 × 7
#>   variable   mean median    sd    mad      q5    q95
#>   <chr>     <dbl>  <dbl> <dbl>  <dbl>   <dbl>  <dbl>
#> 1 mu       -0.312 -0.228 0.167 0.0916 -0.390  -0.211
#> 2 sigma     0.145  0.143 0.142 0.0480  0.0296  0.272
#> 
#> $quantities$likelihood
#> # A tibble: 2 × 7
#>   variable   mean median     sd    mad       q5    q95
#>   <chr>     <dbl>  <dbl>  <dbl>  <dbl>    <dbl>  <dbl>
#> 1 mu        0.397  0.299 -0.318 -0.167  0.643    0.173
#> 2 sigma    -0.246 -0.210 -0.292 -0.147 -0.00776 -0.451
#> 
#> 
#> $loadings
#> NULL
#> 
```
