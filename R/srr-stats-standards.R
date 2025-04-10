#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#' (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose TRUE
#'
#' 
#'
#' 
#'
#' @noRd
NULL

#' NA_standards
#'
#' Any non-applicable standards can have their tags changed from
#' `@srrstatsTODO` to `@srrstatsNA`, and placed together in this
#' block, along with explanations for why each of these standards have
#' been deemed not applicable.  (These comments may also be deleted at
#' any time.)
#' @srrstatsNA {G1.5} No performance claims are made about this
#'   software
#' @srrstatsNA {G1.6} There are no other implementations of this
#'   method in R packages.
#' @srrstatsNA {G2.4a} No conversion to integers required.
#' @srrstatsNA {G2.4d} *explicit conversion to factor via
#'   `as.factor()`*
#' @srrstatsNA {G2.5} No functions require factor type inputs.
#' @srrstatsNA {G2.9} posterior package is used for type conversion of
#'   draws objects, and has warnings for loss of information.
#' @srrstatsNA {G2.11} The `posterior` package is used to handle tabular data.
#' @srrstatsNA {G2.14b, G2.14c} Missing values result in error, or are handled by `posterior` package.
#' @srrstatsNA {G4.0} No files are written by priorsense.
#' @srrstatsNA {G5.4} priorsense is the first implementation.
#' @srrstatsNA {G5.4b} priorsense is the first implementation.
#' @srrstatsNA {G5.4c} priorsense is the first implementation.
#' @srrstatsNA {G5.10, G5.12} There are no extended tests in priorsense.
#' @srrstatsNA {G5.11} No downloads are required for tests.
#' @srrstatsNA {G5.11a} No downloads are required for tests.
#' @srrstatsNA {G2.12} tabular objects are handled via the `posterior` package
#' @srrstatsNA {BS1.0} The term "hyperparameter" is not used.
#' @srrstatsNA {BS1.1, BS1.2, BS1.2a, BS1.2b, BS1.2c} Model fitting is not part of the software package, the user should provide the posterior draws that are generated via some other means.
#' @srrstatsNA {BS1.3, BS1.3a, BS1.3b} No sampling algorithm is implemented, users should provide already generated posterior draws.
#' @srrstatsNA {BS1.4, BS1.5} No convergence checkers are implemented, as they are not needed.
#' @srrstatsNA {BS2.1, BS2.1a} No data is used, instead only the posterior draws
#' @srrstatsNA {BS2.2, BS2.3, BS2.4, BS2.5} No distributional parameters are handled in the software package, the model should be fit with some other software beforehand
#' @srrstatsNA {BS2.7, BS2.8, BS2.9, BS2.10, BS2.11} No sampling algorithm is implemented, users should provide already generated posterior draws.
#' @srrstatsNA {BS3.1, BS3.2} Perfect collinearity is not an issue for prior diagnostics
#' @srrstatsNA {BS4.6, BS4.7} No convergence checker implemented or needed
#' @srrstatsNA {BS4.0, BS4.1, BS4.2, BS4.3, BS4.4, BS4.5} The software does to generate draws from the posterior distribution, it analyses already generated ones
#' @srrstatsNA {BS5.0} The software does not generate draws requiring a seed or initial state
#' @srrstatsNA {BS5.1} Model fitting is not performed, so input data structure is not relevant
#' @srrstatsNA {BS5.4} No convergence checkers implemented or needed
#' @srrstatsNA {BS6.2} The software package does not create sequences of posterior draws
#' @srrstatsNA {BS6.5} The software does not create sequences of posterior draws
#' @srrstatsNA {BS7.0, BS7.1, BS7.2, BS7.3, BS7.4, BS7.4a} The software does not perform model fitting, so parameter recovery and prediction are not relevant
#' @noRd
NULL
