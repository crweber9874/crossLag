% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphRI.R
\name{ri_graph}
\alias{ri_graph}
\title{Draw a tidySEM graph of a RI-CLPM model}
\usage{
ri_graph(model = fit, waves = c(3, 4, 5, 6))
}
\arguments{
\item{waves}{Number of waves in the fitted model.}

\item{fit}{A fitted lavaan model object.}
}
\value{
SEM plot
}
\description{
Create a simulated dataset of a cross-lagged model with a specified number of waves and structural parameters.
}
