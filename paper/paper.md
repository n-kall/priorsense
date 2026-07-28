---
title: 'priorsense: Efficient prior and likelihood sensitivity checks for Bayesian models in R'
tags:
  - R
  - Bayesian inference
  - Sensitivity analysis
  - Diagnostics
authors:
  - name: Noa Kallioinen
    corresponding: true
    orcid: 0000-0003-1586-8382
    affiliation: "1,4"
  - name: Topi Paananen
    orcid: 0000-0002-6542-407X
    affiliation: 2
  - name: Paul-Christian Bürkner
    orcid: 0000-0001-5765-8995
    affiliation: 3
  - name: Aki Vehtari
    orcid: 0000-0003-2164-9469
    affiliation: 4
affiliations:
 - name: University of Helsinki, Finland
   index: 1
 - name: Independent researcher
   index: 2
 - name: TU Dortmund University, Germany
   index: 3
 - name: ELLIS Institute Finland and Aalto University, Finland
   index: 4
bibliography: paper.bib
format:
  pdf:
    keep-tex: true
---

# Summary

`priorsense` is an R [@Rcoreteam] package that provides tools for prior and likelihood diagnostics and
sensitivity analysis of Bayesian models. It includes functions for performing
power-scaling sensitivity analysis. This is a way to check
how sensitive the posterior is to perturbations of the prior and likelihood and
diagnose the cause of sensitivity. The method does not require refitting the
model, so it is computationally efficient. Full details of the method
underlying power-scaling sensitivity analysis are described in
[@kallioinenDetectingDiagnosingPrior2024].


# Statement of need

The prior and likelihood are the underlying components of a Bayesian
model. Together with the data, the resulting posterior distribution is
derived. Sensitivity checks are an important part of the Bayesian workflow
[@Bayesian-Workflow:2026] as they can identify:

- whether the posterior is likely to meaningfully change with small changes to the
prior
- whether the likelihood is informative or not for some parameters of interest

Before `priorsense` was developed, sensitivity checks were commonly conducted in an
ad hoc manner and required expensive recomputations for each modification of the
prior or likelihood. `priorsense` fills a clear gap in the Bayesian workflow by
providing fast and efficient sensitivity checks that are widely applicable.

# State of the field

Bayesian sensitivity analysis has been studied for decades, however much of the
previous work was focused on posteriors that could be computed
analytically. While probabilistic programming and Markov chain Monte Carlo
methods have increased in popularity and use, tools for sensitivity checks that
can be used for complex intractable posteriors remain relatively rare. The main
other software package that has been developed is `adjustr` [@Adjustr], which focuses on
flexibility in manual alternative model specification. ArviZ [@Martin2026], a
Python suite for Bayesian model diagnostics, also includes diagnostics mirroring
those implemented in `priorsense`.

# Software design

The `priorsense` package provides tools for efficient sensitivity checks,
including numerical and graphical diagnostics. It is directly compatible with
`brms` [@burknerBrmsPackageBayesian2017], Stan
[@standevelopmentteamStanModellingLanguage2026], JAGS [@Plummer2003JAGS] and
NIMBLE [@nimble-article:2017; @nimble-software:2026], and can be used with
posterior draws from other sources.

`priorsense` functions primarily operate on a `priorsense_data` object, which
contains all the necessary data to perform the sensitivity checks. Fitted model
objects are first coerced to this data object, to ensure consistency regardless
of the origin of the posterior draws. The primary user-facing functions for
power-scaling sensitivity checks are:

- `powerscale_sensitivity()` for numerical diagnostics
- `powerscale_plot_dens()`, `powerscale_plot_ecdf()` and
  `powerscale_plot_quantities()` for graphical checks

These can be run directly on outputs from `brms`, Stan, JAGS and NIMBLE fitting
procedures, provided that the required log prior and log likelihood evaluations
are stored. The easiest way to save these is to include them in the model
code. This can be done simply in Stan, JAGS and NIMBLE, and is already included
in `brms` model output by default. Vignettes in the package documentation describe how to do this
for each supported language.

For efficient computation, power-scaling sensitivity analysis uses Pareto
smoothed importance sampling [@vehtariParetoSmoothedImportance2024] and
importance weighted moment matching [@paananenImplicitlyAdaptiveImportance2021] from the `iwmm` package [@iwmm]. `priorsense` relies on the well-established posterior package [@Bürkner2026] as a backend for manipulating posterior draws and for Pareto smoothed importance sampling. It is therefore compatible with any format of posterior draws that the posterior package supports. Plots are created with `ggplot2` [@ggplot2] with additional functions from ggdist [@ggdist] and `ggh4x` [@ggh4x]. Input checking is done with `checkmate` [@checkmate] and tests are implemented with `testthat` [@testthat].

# Research impact statement

The diagnostics provided by `priorsense` seamlessly fit into a modern Bayesian
workflow. It has been included in recommended workflows for Bayesian modelling [@Bayesian-Workflow:2026] and since its release, `priorsense` has been downloaded from CRAN tens-of-thousands of times and has been used for academic research in fields such as
ecology [@vankoBayesianIntegratedPopulation2026], medicine [@mazzinariHighPEEPRecruitment2024], psychology [@bezdicekInterplayCrossculturalPsychometric2024; @gijsenMappingCognitiveProcesses2024].


# AI usage disclosure

No generative AI tools were used by the authors in the development of this software or preparation of this manuscript.

# Acknowledgements

We acknowledge contributions from Frank Weber and Osvaldo Martin and software reviews from Simon Taylor and
Dylan Dijk.

The research behind the package was supported by:

- The Research Council of Finland Flagship Program "Finnish Center for
  Artificial Intelligence" (FCAI)
- Research Council of Finland project 340721
- The Finnish Foundation for Technology Promotion
- Deutsche Forschungsgemeinschaft (DFG, German Research Foundation) under
  Germany's Excellence Strategy - EXC 2075 – 390740016

# References

