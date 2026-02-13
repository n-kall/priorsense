# Example Stan model for power-scaling

Provides example models (with data) that are ready for use with
power-scaling.

## Usage

``` r
example_powerscale_model(model = "univariate_normal")
```

## Arguments

- model:

  Character specifying which model code to return. Currently
  "univariate_normal" and "eight_schools" are implemented.

## Value

List containing model code and corresponding data.

## Examples

``` r
ex_normal <- example_powerscale_model(model = "univariate_normal")

ex_eightschools <- example_powerscale_model(model = "eight_schools")
```
