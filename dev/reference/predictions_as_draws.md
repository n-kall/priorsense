# brms predictions as draws

Create predictions using brms functions and convert them into draws
format

## Usage

``` r
predictions_as_draws(
  x,
  predict_fn,
  prediction_names = NULL,
  warn_dims = getOption("priorsense.warn", TRUE),
  ...
)
```

## Arguments

- x:

  brmsfit object

- predict_fn:

  function for predictions

- prediction_names:

  optional names of the predictions

- warn_dims:

  throw a warning when coercing predict_fn's output from 3 margins to 2
  margins?

- ...:

  further arguments passed to predict_fn

## Value

draws array of predictions

## Examples

``` r
if (FALSE) { # \dontrun{
library(brms)

if ("log_prior_draws.brmsfit" %in% methods(log_prior_draws) &&
    ("log_lik_draws.brmsfit" %in% methods(log_lik_draws))) {
  fit <- brm(
    yield ~ N * P * K,
    data = npk,
    prior = prior(normal(0, 1), class = "b"),
    refresh = 0
  )

  powerscale_sensitivity(
      fit,
      variable = "_pred",
      prediction = function(x) predictions_as_draws(
                                 x, brms::posterior_epred
                               )
  )
}
} # }
```
