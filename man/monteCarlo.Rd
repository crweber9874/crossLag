% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monteCarloCLPMRI.R
\name{monteCarlo}
\alias{monteCarlo}
\title{Run Monte Carlo simulations for CLPM, RI-CLPM, and Dual Change Models}
\usage{
monteCarlo(
  trials = 100,
  waves = 10,
  model = c("ols", "clpm"),
  data_generation = c("clpm", "ri-clpm", "latent-change-x", "latent-change-xy"),
  proportion.change_x = 0.5,
  proportion.change_y = 0.5,
  change.mean_x = 0.3,
  change.mean_y = 0.3,
  coupling_xy = 0,
  coupling_yx = 0,
  within_person_stability_x = 0.1,
  within_person_stability_y = 0.1,
  cross_lag_x = 0,
  cross_lag_y = 0,
  confound = 0,
  variance_x = 1,
  variance_y = 1,
  sample_size = 1000,
  ...
)
}
\arguments{
\item{trials}{The number of trials for the Monte Carlo simulation.}

\item{waves}{The number of waves (time points) in the model.}

\item{model}{Specify whether an "ols" or "clpm" model.}

\item{data_generation}{Specify the data generation method: "clpm", "ri-clpm", "latent-change-x", "latent-change-xy".}

\item{proportion.change_x}{Proportion of change in x.}

\item{proportion.change_y}{Proportion of change in y.}

\item{...}{Additional parameters.}
}
\value{
A data frame containing the results of the simulation.
}
\description{
Run Monte Carlo simulations for CLPM, RI-CLPM, and Dual Change Models
}
