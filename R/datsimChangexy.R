#' Simulate data from a latent change model, two variables.
#'
#' Create a simulated dataset for two variable, based on latent change score parameters.
#' This follows McArdle's review "Latent Variable Modeling of Differences and Changes with
#' Longitudinal data.
#' McArdle JJ. Latent variable modeling of differences and changes with longitudinal data. Annu Rev Psychol. 2009;60:577-605. doi: 10.1146/annurev.psych.60.110707.163612. PMID: 18817479.
#' @param waves The number of waves (time points) in the model.
#' @param latent.true.variance_x The true score variance, or the variance of the latent. Variable x.
#' @param observed.residual.var_x The residual variance; the variance of the observed data, from waves 2 through K. Variable x.
#' @param change.mean_x The mean of the latent change score factor. Variable x.
#' @param constant.change.var_x Two types of change are modeled, proportional and constant.  This is the constant change variance. Variable x.
#' @param proportion.change_x The proportional change estimate. This is the proportion of the previous wave's true score that is added to the current wave's true score. Variable x.
#' @param cov.change.true_x The covariance between the latent change and initial true score, the common factor at wave 1. Variable x.
#' @param latent.true.variance_y The true score variance, or the variance of the latent. Variable y.
#' @param observed.residual.var_y The residual variance; the variance of the observed data, from waves 2 through K. Variable y.
#' @param change.mean_y The mean of the latent change score factor. Variable y.
#' @param constant.change.var_y Two types of change are modeled, proportional and constant.  This is the constant change variance. Variable y.
#' @param proportion.change_y The proportional change estimate. This is the proportion of the previous wave's true score that is added to the current wave's true score. Variable y.
#' @param cov.change.true_y The covariance between the latent change and initial true score, the common factor at wave 1. Variable y.
#' @param coupling.xy The coupling score -- predicted effect of x on change in y
#' @param coupling.xy The coupling score -- predicted effect of y on change in x
#' @param cov.xy.wave1 The covariance between x and y at wave 1
#' @param cov.general The covariance between the general change factors for x and y.

#'
#' slope.cov
#' @param ... Additional arguments to pass to the `lavaan::simulateData` function.
#'
#' @return A list containing two elements:
#'    * `model`: The Lavaan model syntax used for data simulation.
#'    * `data`:  The simulated data in a data frame format.
#'

#' @export


simulate_latent_change_multiple= function(
    waves = 10,
    latent.true.variance_x = 1,
    observed.residual.var_x = 0.1,
    change.mean_x = 0.01,
    constant.change.var_x = 0.1,
    proportion.change_x = 0.02,
    cov.change.true_x = 0.1,
    latent.true.variance_y = 1,
    observed.residual.var_y = 0.1,
    change.mean_y = 0.1,
    constant.change.var_y = 0.5,
    proportion.change_y = 0.3,
    cov.change.true_y = 0.1,
    coupling_xy = 0.1,
    coupling_yx = 0.1,
    cov.xy.wave1 = 0.1,
    cov.general =  0.1,
    cov.x.general = 0.1,
    cov.y.general = 0.1,

    ...) {

  # Start with x
  #latent true scores (loadings = 1)
  model_string <- ""

  # Define the common factors
  for(w in 1:waves){
    model_string <- paste0(model_string, "    cf_x", w, " =~ 1*x", w, "\n")
  }

  # Latent true score means (initial free, others = 0)
  model_string <- paste0(model_string, "    cf_x1 ~ 1\n")
  for(w in 2:waves){
    model_string <- paste0(model_string, "    cf_x", w, " ~ 0*1\n")
  }

  # Latent true score variances (initial free, others = 0)
  model_string <- paste0(model_string, "    cf_x1 ~~", latent.true.variance_x,"*cf_x1\n")
  for(w in 2:waves){
    model_string <- paste0(model_string, "    cf_x", w, " ~~ 0*cf_x", w, "\n")
  }

  # Observed intercepts (fixed to 0)
  for(w in 2:waves){
    model_string <- paste0(model_string, "    x", w, " ~ 0*1\n")
  }

  # Observed residual variances (constrained to equality)
  for(w in 2:waves){
    model_string <- paste0(model_string, "    x", w, " ~~", observed.residual.var_x,"*x", w, "\n")
  }

  # Autoregressions (fixed = 1)
  for(w in 2:waves){
    model_string <- paste0(model_string, "    cf_x", w, " ~ 1*cf_x", w-1, "\n")
  }

  # Latent change scores (fixed = 1)
  for(w in 2:waves){
    model_string <- paste0(model_string, "    ld_x", w, " =~ 1*cf_x", w, "\n")
  }

  # Latent change score means (constrained to 0)
  for(w in 2:waves){
    model_string <- paste0(model_string, "    ld_x", w, " ~ 0*1\n")
  }

  # Latent change score variances (constrained to 0)
  for(w in 2:waves){
    model_string <- paste0(model_string, "    ld_x", w, " ~~ 0*ld_x", w, "\n")
  }

  # Constant change factor (loadings = 1)
  model_string <- paste0(model_string, "    general_x =~ 1*ld_x2\n")
  for(w in 3:waves){
    model_string <- paste0(model_string, "          + 1*ld_x", w, "\n")
  }

  # Constant change factor mean
  model_string <- paste0(model_string, "    general_x ~", change.mean_x, "*1\n")

  # Constant change factor variance
  model_string <- paste0(model_string, "    general_x ~~", constant.change.var_x, "*general_x\n")

  # Constant change factor covariance with the initial true score
  model_string <- paste0(model_string, "    general_x ~~", cov.change.true_x, "*cf_x1\n")

  # # Proportional effects (constrained equal)
  # for(w in 2:waves){
  #   model_string <- paste0(model_string, "    ld_x", w, " ~ ", proportion.change_x,"* cf_x", w-1, "\n")
  # }

  # Define the common factors
  for(w in 1:waves){
    model_string <- paste0(model_string, "    cf_y", w, " =~ 1*y", w, "\n")
  }

  # Latent true score means (initial free, others = 0)
  model_string <- paste0(model_string, "    cf_y1 ~ 1\n")
  for(w in 2:waves){
    model_string <- paste0(model_string, "    cf_y", w, " ~ 0*1\n")
  }

  # Latent true score variances (initial free, others = 0)
  model_string <- paste0(model_string, "    cf_y1 ~~", latent.true.variance_y,"*cf_y1\n")
  for(w in 2:waves){
    model_string <- paste0(model_string, "    cf_y", w, " ~~ 0*cf_y", w, "\n")
  }

  # Observed intercepts (fixed to 0)
  for(w in 2:waves){
    model_string <- paste0(model_string, "    y", w, " ~ 0*1\n")
  }

  # Observed residual variances (constrained to equality)
  for(w in 2:waves){
    model_string <- paste0(model_string, "    y", w, " ~~", observed.residual.var_y,"*y", w, "\n")
  }

  # Autoregressions (fixed = 1)
  for(w in 2:waves){
    model_string <- paste0(model_string, "    cf_y", w, " ~ 1*cf_y", w-1, "\n")
  }

  # Latent change scores (fixed = 1)
  for(w in 2:waves){
    model_string <- paste0(model_string, "    ld_y", w, " =~ 1*cf_y", w, "\n")
  }

  # Latent change score means (constrained to 0)
  for(w in 2:waves){
    model_string <- paste0(model_string, "    ld_y", w, " ~ 0*1\n")
  }

  # Latent change score variances (constrained to 0)
  for(w in 2:waves){
    model_string <- paste0(model_string, "    ld_y", w, " ~~ 0*ld_y", w, "\n")
  }

  # Constant change factor (loadings = 1)
  model_string <- paste0(model_string, "    general_y =~ 1*ld_y2\n")
  for(w in 3:waves){
    model_string <- paste0(model_string, "          + 1*ld_y", w, "\n")
  }

  # Constant change factor mean
  model_string <- paste0(model_string, "    general_y ~", change.mean_y, "*1\n")

  # Constant change factor variance
  model_string <- paste0(model_string, "    general_y ~~", constant.change.var_y, "*general_y\n")

  # Constant change factor covariance with the initial true score
  model_string <- paste0(model_string, "    general_y ~~", cov.change.true_y, "*cf_y1\n")
#
#   # Proportional effects (constrained equal)
#   for(w in 2:waves){
#     model_string <- paste0(model_string, "    ld_y", w, " ~ ", proportion.change_y,"* cf_y", w-1, "\n")
#   }

  ## Combining models

  # Coupling parameters
  for(w in 2:waves){
    model_string <- paste0(model_string, "    ld_x", w, " ~ ", coupling_xy,"* cf_y", w-1, "\n")
    model_string <- paste0(model_string, "    ld_y", w, " ~ ", coupling_yx,"* cf_x", w-1, "\n")
  }
  model_string <- paste0(model_string, "    general_x", " ~~ ", cov.general,"* general_y", "\n")
  model_string <- paste0(model_string, "    cf_x1~~", cov.xy.wave1,"* cf_y1", "\n")
# Covariance between wave 1 x and y and the general change score
  model_string <- paste0(model_string, "    cf_x1~~", cov.x.general,"* general_y", "\n")
  model_string <- paste0(model_string, "    cf_y1~~", cov.y.general,"* general_x", "\n")


  dat = lavaan::simulateData(model_string, ...)

  return(list(model = model_string, data = dat))

}

