# Cumulative Jensen-Shannon divergence

Computes the cumulative Jensen-Shannon distance between two samples.

## Usage

``` r
cjs_dist(
  x,
  y,
  x_weights = NULL,
  y_weights = NULL,
  metric = TRUE,
  unsigned = TRUE,
  ...
)
```

## Arguments

- x:

  numeric vector of draws from first distribution

- y:

  numeric vector of draws from second distribution

- x_weights:

  numeric vector (same length as x) of weights for the draws of the
  first distribution

- y_weights:

  numeric vector (same length as y) of weights for the draws of the
  second distribution

- metric:

  Logical; if TRUE, return square-root of CJS. Default is TRUE

- unsigned:

  Logical; if TRUE then return max of CJS(P(x) \|\| Q(x)) and CJS(P(-x)
  \|\| Q(-x)). This ensures invariance to transformations such as PCA.
  Default is TRUE

- ...:

  unused

## Value

distance value based on CJS computation.

## Details

The Cumulative Jensen-Shannon distance is a symmetric metric based on
the cumulative Jensen-Shannon divergence. The divergence CJS(P \|\| Q)
between two cumulative distribution functions P and Q is defined as:

\$\$CJS(P \|\| Q) = \sum P(x) \log \frac{P(x)}{0.5 (P(x) + Q(x))} +
\frac{1}{2 \ln 2} \sum (Q(x) - P(x))\$\$

The symmetric metric is defined as:

\$\$CJS\_{dist}(P \|\| Q) = \sqrt{CJS(P \|\| Q) + CJS(Q \|\| P)}\$\$

This has an upper bound of \\\sqrt{ \sum (P(x) + Q(x))}\\

## References

Nguyen H-V., Vreeken J. (2015). Non-parametric Jensen-Shannon
Divergence. In: Appice A., Rodrigues P., Santos Costa V., Gama J., Jorge
A., Soares C. (eds) Machine Learning and Knowledge Discovery in
Databases. ECML PKDD 2015. Lecture Notes in Computer Science, vol 9285.
Springer, Cham. `doi:10.1007/978-3-319-23525-7_11`

## Examples

``` r
x <- rnorm(100)
y <- rnorm(100, 2, 2)
cjs_dist(x, y, x_weights = NULL, y_weights = NULL)
#>       cjs 
#> 0.4260629 
```
