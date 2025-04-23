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
#' @srrstatsNA {G2.11} The `posterior` package is used to handle
#'   tabular data.
#' @srrstatsNA {G2.14b, G2.14c} Missing values result in error, or are
#'   handled by `posterior` package.
#' @srrstatsNA {G4.0} No files are written by priorsense.
#' @srrstatsNA {G5.4} priorsense is the first implementation.
#' @srrstatsNA {G5.4b} priorsense is the first implementation.
#' @srrstatsNA {G5.4c} priorsense is the first implementation.
#' @srrstatsNA {G5.10, G5.12} There are no extended tests in
#'   priorsense.
#' @srrstatsNA {G5.11} No downloads are required for tests.
#' @srrstatsNA {G5.11a} No downloads are required for tests.
#' @srrstatsNA {G2.12} tabular objects are handled via the `posterior`
#'   package
#'
#' @srrstatsNA {EA5.6} No bundled libraries
#' @srrstatsNA {EA5.6} No bundled libraries
#' @srrstatsNA {EA5.5} plots do not require units as they are unit-free quantities
#' @srrstatsNA {EA5.1} default typefaces are used
#' 
#' @noRd
NULL
