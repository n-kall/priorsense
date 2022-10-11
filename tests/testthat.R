library(testthat)
library(priorsense)
library(rstan)
#library(cmdstanr)

if (!identical(Sys.getenv("NOT_CRAN"), "true") && !(.Platform$OS.type == "windows")) {
  test_check("priorsense")
}
