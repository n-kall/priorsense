# Extract log prior draws

Extract log likelihood from fitted model and return as a draws object.

## Usage

``` r
log_prior_draws(x, ...)

# S3 method for class 'stanfit'
log_prior_draws(x, joint = FALSE, log_prior_name = "lprior", ...)

# S3 method for class 'CmdStanFit'
log_prior_draws(x, joint = FALSE, log_prior_name = "lprior", ...)

# S3 method for class 'draws'
log_prior_draws(x, joint = FALSE, log_prior_name = "lprior", ...)
```

## Arguments

- x:

  Model fit or draws object.

- ...:

  Arguments passed to individual methods.

- joint:

  Logical indicating whether to return the joint log prior or array.
  Default is FALSE.

- log_prior_name:

  Name of parameter in Stan model corresponding to log prior, default is
  "lprior".

## Value

A draws_array object containing log_prior values.

## Examples

``` r
ex <- example_powerscale_model()
drw <- ex$draws

log_prior_draws(drw)
#> # A draws_array: 1000 iterations, 4 chains, and 2 variables
#> , , variable = lprior[1]
#> 
#>          chain
#> iteration   1   2   3   4
#>         1 -45 -46 -44 -49
#>         2 -46 -46 -44 -45
#>         3 -44 -50 -47 -46
#>         4 -45 -49 -47 -46
#>         5 -42 -49 -47 -45
#> 
#> , , variable = lprior[2]
#> 
#>          chain
#> iteration    1    2    3    4
#>         1 -1.9 -1.9 -2.0 -1.9
#>         2 -1.9 -1.9 -1.9 -1.9
#>         3 -1.9 -1.9 -1.9 -1.9
#>         4 -1.9 -1.9 -1.9 -1.9
#>         5 -1.9 -1.9 -1.9 -1.9
#> 
#> # ... with 995 more iterations
```
