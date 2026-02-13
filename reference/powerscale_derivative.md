# Derivative with respect to power-scaling

Calculate the analytical derivative of a quantity with respect to
power-scaling prior or likelihood.

## Usage

``` r
powerscale_derivative(x, log_component, quantity = "mean", ...)
```

## Arguments

- x:

  draws object of posterior draws

- log_component:

  numeric vector of log likelihood or log prior values

- quantity:

  Character specifying quantity of interest (default is "mean"). Options
  are "mean", "sd", "var".

- ...:

  unused

## Value

Derivative of the quantity with respect to log2 of the power-scaling
factor (alpha).

## Examples

``` r
example_model <- example_powerscale_model()
draws <- example_model$draws
log_prior <- log_prior_draws(draws, joint = TRUE)
posterior::summarise_draws(
    posterior::subset_draws(draws, variable = c("mu", "sigma")),
    mean,
    mean_sens = ~powerscale_derivative(.x, log_prior, quantity = "mean")
)
#> # A tibble: 2 × 3
#>   variable  mean psens_mean
#>   <chr>    <dbl>      <dbl>
#> 1 mu       9.53      -0.313
#> 2 sigma    0.884      0.145
```
