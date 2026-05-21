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
#> # A draws_array: 1000 iterations, 4 chains, and 3 variables
#> , , variable = lprior
#> 
#>          chain
#> iteration   1   2   3   4
#>         1 -48 -48 -51 -47
#>         2 -46 -47 -49 -48
#>         3 -46 -48 -50 -49
#>         4 -47 -49 -48 -48
#>         5 -47 -45 -50 -48
#> 
#> , , variable = lprior_mu
#> 
#>          chain
#> iteration   1   2   3   4
#>         1 -46 -47 -49 -45
#>         2 -44 -45 -47 -47
#>         3 -44 -46 -48 -48
#>         4 -45 -47 -46 -46
#>         5 -45 -43 -48 -46
#> 
#> , , variable = lprior_sigma
#> 
#>          chain
#> iteration    1    2    3    4
#>         1 -1.9 -1.9 -1.9 -1.9
#>         2 -1.9 -1.9 -1.9 -1.9
#>         3 -1.9 -1.9 -1.9 -1.9
#>         4 -1.9 -1.9 -1.9 -1.9
#>         5 -1.9 -1.9 -1.9 -1.9
#> 
#> # ... with 995 more iterations
```
