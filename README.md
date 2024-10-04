# $\texttt{crossLag}$: The Cross Lagged Regression Design

### A package to estimate latent variable panel models and simulation designs

Christopher Weber, PhD

Unviversity of Arizona

School of Government and Public Policy

### Installation

```r
devtools::install_github("crweber9874/crossLag")
```

### Summary

This package develops an analytic framework to analyze panel data in the tradition of the Cross Lagged Panel Model -- the CLPM. This repository contains code written by Stanley Feldman, Adam Parish and Chris Weber to critique and improve upon the cross lag regression.

The **Cross-Lagged Panel Model (CLPM)** model was pioneered in Campbell (1963) as well as Campbell and Stanley (1963) and is an oft used, intuitive method to test **competing causal processes**. The model requires the estimation of lagged effects, including a lag of the dependent variable. By analyzing the lagged effects (the influence of one variable on the other at a later time point), researchers often purport to gain insights into the direction and strength of causal relationships.

In the language of causal reasoning, the Directed Acyclic Graph (DAG) for the CLR model can be expanded to incorporate commonly encountered data issues arising with the CLPM, mediation, confounding, and collision.

## [A Brief Presentation of the CLPM and RI-CLPM](https://crosslagregression.netlify.app/#/title-slide)

### Structure

- $\texttt{~/R}$. The functions used in the simulation and R-package.

- $\texttt{~/vignettes}$. Brief vignettes on data used in the simulation and the R-package.

- $\texttt{~/presentations}$. Presentations on the CLPM and the R-package.

- [SGPP Presentation, October 4, 2024](~/Dropbox/github_repos/crossLag_p/crossLag/presentations/sgpp_presentation24.html)

