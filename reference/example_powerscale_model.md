# Example models for power-scaling sensitivity

Provides example models (with data) that are ready for use with
power-scaling.

## Usage

``` r
example_powerscale_model(model = "univariate_normal", language = "stan")
```

## Arguments

- model:

  Character specifying which model code to return. Currently
  "univariate_normal" and "eight_schools" are implemented.

- language:

  Character specifying which modelling language to return. One of
  "stan", "jags", "nimble". Default is "stan".

## Value

List containing model code and corresponding data.

## Examples

``` r
ex_normal <- example_powerscale_model(model = "univariate_normal", language = "stan")

ex_eightschools <- example_powerscale_model(model = "eight_schools", language = "jags")
```
