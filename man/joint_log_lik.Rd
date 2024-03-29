% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brms-functions.R, R/extract_joint_log_lik.R
\name{joint_log_lik_brmsfit}
\alias{joint_log_lik_brmsfit}
\alias{joint_log_lik}
\alias{joint_log_lik_stanfit}
\alias{joint_log_lik_CmdStanFit}
\title{Extract joint log likelihood from fitted model}
\usage{
joint_log_lik_brmsfit(x, ...)

joint_log_lik_stanfit(x, log_lik_name = "log_lik", ...)

joint_log_lik_CmdStanFit(x, log_lik_name = "log_lik", ...)
}
\arguments{
\item{x}{Model fit.}

\item{...}{Arguments passed to individual methods.}

\item{log_lik_name}{Name of parameter in Stan model
corresponding to log likelihood, default is "log_lik".}
}
\value{
A draws_array object containing log_prior values.
}
\description{
Extract joint log likelihood from fitted model
}
