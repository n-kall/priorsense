# Extract log likelihood draws

Extract log likelihood from fitted model and return as a draws object.

## Usage

``` r
log_lik_draws(x, ...)

# S3 method for class 'stanfit'
log_lik_draws(x, joint = FALSE, log_lik_name = "log_lik", ...)

# S3 method for class 'CmdStanFit'
log_lik_draws(x, joint = FALSE, log_lik_name = "log_lik", ...)

# S3 method for class 'draws'
log_lik_draws(x, joint = FALSE, log_lik_name = "log_lik", ...)
```

## Arguments

- x:

  Model fit or draws object.

- ...:

  Arguments passed to individual methods.

- joint:

  Logical indicating whether to return the joint log likelihood or
  array. Default is FALSE.

- log_lik_name:

  Name of parameter in Stan model corresponding to log likelihood,
  default is "log_lik".

## Value

A draws_array object containing log_lik values.

## Examples

``` r
ex <- example_powerscale_model()
drw <- ex$draws

log_lik_draws(drw)
#> # A draws_array: 1000 iterations, 4 chains, and 25 variables
#> , , variable = log_lik[1]
#> 
#>          chain
#> iteration     1     2     3     4
#>         1 -0.65 -0.70 -0.71 -0.87
#>         2 -0.66 -0.83 -0.70 -0.87
#>         3 -0.56 -1.01 -0.54 -0.73
#>         4 -0.59 -0.80 -0.75 -0.62
#>         5 -0.62 -0.90 -0.73 -0.62
#> 
#> , , variable = log_lik[2]
#> 
#>          chain
#> iteration    1     2     3     4
#>         1 -1.1 -1.02 -0.74 -1.21
#>         2 -1.3 -1.19 -0.94 -1.10
#>         3 -1.4 -1.20 -0.81 -0.93
#>         4 -1.3 -0.99 -1.05 -1.05
#>         5 -1.3 -1.46 -0.89 -1.05
#> 
#> , , variable = log_lik[3]
#> 
#>          chain
#> iteration     1     2     3     4
#>         1 -0.76 -0.86 -1.10 -0.91
#>         2 -0.67 -0.89 -0.92 -0.98
#>         3 -0.58 -1.09 -0.90 -0.95
#>         4 -0.64 -0.98 -0.89 -0.77
#>         5 -0.67 -0.84 -0.98 -0.77
#> 
#> , , variable = log_lik[4]
#> 
#>          chain
#> iteration     1     2     3     4
#>         1 -0.76 -0.86 -1.10 -0.91
#>         2 -0.67 -0.89 -0.92 -0.98
#>         3 -0.58 -1.09 -0.90 -0.95
#>         4 -0.64 -0.98 -0.89 -0.77
#>         5 -0.67 -0.84 -0.98 -0.77
#> 
#> # ... with 995 more iterations, and 21 more variables
```
