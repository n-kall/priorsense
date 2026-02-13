# Create data structure for priorsense

Create a data structure that contains all required data and functions
for priorsense

## Usage

``` r
create_priorsense_data(x, ...)

# Default S3 method
create_priorsense_data(
  x,
  fit = NULL,
  log_prior_fn = log_prior_draws,
  log_lik_fn = log_lik_draws,
  log_prior = NULL,
  log_lik = NULL,
  log_ratio_fn = NULL,
  log_prior_name = "lprior",
  log_lik_name = "log_lik",
  ...
)

# S3 method for class 'stanfit'
create_priorsense_data(x, ...)

# S3 method for class 'CmdStanFit'
create_priorsense_data(x, ...)

# S3 method for class 'draws'
create_priorsense_data(x, ...)

# S3 method for class 'rjags'
create_priorsense_data(x, ...)
```

## Arguments

- x:

  an object for which the method is defined or an object coercible to a
  [`posterior::draws`](https://mc-stan.org/posterior/reference/draws.html)
  object

- ...:

  arguments passed to methods

- fit:

  a model fit object (only used if x is not a fit object)

- log_prior_fn:

  function to derive log prior from x or fit (if not NULL)

- log_lik_fn:

  function to derive log likelihood from x or fit (if not NULL)

- log_prior:

  draws object from log prior, must be numeric and not include NA, NaN,
  Inf, -Inf or be constant

- log_lik:

  draws from log likelihood, must be numeric and not include NA, NaN,
  Inf, -Inf or be constant

- log_ratio_fn:

  function for moment matching

- log_prior_name:

  Character (case sensitive) specifying name of the variable storing the
  log prior evaluations

- log_lik_name:

  Character (case sensitive) specifying name of the variable storing the
  log likelihood evaluations

## Value

A `priorsense_data` object, which contains the data and functions to run
sensitivity analyses.

## Examples

``` r
x <- example_powerscale_model()
drw <- x$draws

psd <- create_priorsense_data(drw)
```
