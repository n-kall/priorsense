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
#>         1 -0.98 -0.44 -1.17 -0.76
#>         2 -0.98 -0.57 -1.11 -0.75
#>         3 -0.83 -0.67 -0.75 -0.96
#>         4 -0.98 -0.82 -0.77 -0.96
#>         5 -1.15 -0.78 -0.79 -0.76
#> 
#> , , variable = log_lik[2]
#> 
#>          chain
#> iteration    1     2     3     4
#>         1 -1.3 -1.06 -1.41 -0.83
#>         2 -1.2 -1.09 -1.42 -1.19
#>         3 -1.3 -0.65 -1.03 -1.17
#>         4 -1.3 -0.81 -1.00 -1.17
#>         5 -1.5 -0.85 -0.99 -1.27
#> 
#> , , variable = log_lik[3]
#> 
#>          chain
#> iteration     1     2     3     4
#>         1 -1.01 -0.66 -1.16 -1.06
#>         2 -1.05 -0.72 -1.09 -0.81
#>         3 -0.84 -1.17 -0.90 -1.04
#>         4 -1.02 -1.14 -0.95 -1.03
#>         5 -1.08 -1.07 -0.97 -0.79
#> 
#> , , variable = log_lik[4]
#> 
#>          chain
#> iteration     1     2     3     4
#>         1 -1.01 -0.66 -1.16 -1.06
#>         2 -1.05 -0.72 -1.09 -0.81
#>         3 -0.84 -1.17 -0.90 -1.04
#>         4 -1.02 -1.14 -0.95 -1.03
#>         5 -1.08 -1.07 -0.97 -0.79
#> 
#> # ... with 995 more iterations, and 21 more variables
```
