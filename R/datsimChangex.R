#' Simulate data from a latent change model, one variable.
#'
#' Create a simulated dataset for one variable, based on latent change score parameters.
#' This follows McArdle's review "Latent Variable Modeling of Differences and Changes with
#' Longitudinal data.
#' McArdle JJ. Latent variable modeling of differences and changes with longitudinal data. Annu Rev Psychol. 2009;60:577-605. doi: 10.1146/annurev.psych.60.110707.163612. PMID: 18817479.
#' @param waves The number of waves (time points) in the model.
#' @param latent.true.variance The true score variance, or the variance of the latent
#' @param observed.residual.var The residual variance; the variance of the observed data, from waves 2 through K
#' @param change.mean The mean of the latent change score factor.
#' @param constant.change.var Two types of change are modeled, proportional and constant.  This is the constant change variance.
#' @param proportion.change The proportional change estimate. This is the proportion of the previous wave's true score that is added to the current wave's true score.
#' @param cov.change.true The covariance between the latent change and initial true score, the common factor at wave 1
#' @param ... Additional arguments to pass to the `lavaan::simulateData` function.
#'
#' @return A list containing two elements:
#'    * `model`: The Lavaan model syntax used for data simulation.
#'    * `data`:  The simulated data in a data frame format.
#'
#' @export



simulate_latent_change_single= function(
    waves = 10,
    latent.true.variance = 1,
    observed.residual.var = 0.1,
    change.mean = 0.1,
    constant.change.var = 0.5,
    proportion.change = 0.02,
    cov.change.true = 0.1,
    ...) {

 #latent true scores (loadings = 1)
    model_string <- ""

    # Define the common factors
    for(w in 1:waves){
      model_string <- paste0(model_string, "    cf", w, " =~ 1*x", w, "\n")
    }

    # Latent true score means (initial free, others = 0)
    model_string <- paste0(model_string, "    cf1 ~ 1\n")
    for(w in 2:waves){
      model_string <- paste0(model_string, "    cf", w, " ~ 0*1\n")
    }

    # Latent true score variances (initial free, others = 0)
    model_string <- paste0(model_string, "    cf1 ~~", latent.true.variance,"*cf1\n")
    for(w in 2:waves){
      model_string <- paste0(model_string, "    cf", w, " ~~ 0*cf", w, "\n")
    }

    # Observed intercepts (fixed to 0)
    for(w in 2:waves){
      model_string <- paste0(model_string, "    x", w, " ~ 0*1\n")
    }

    # Observed residual variances (constrained to equality)
    for(w in 2:waves){
      model_string <- paste0(model_string, "    x", w, " ~~", observed.residual.var,"*x", w, "\n")
    }

    # Autoregressions (fixed = 1)
    for(w in 2:waves){
      model_string <- paste0(model_string, "    cf", w, " ~ 1*cf", w-1, "\n")
    }

    # Latent change scores (fixed = 1)
    for(w in 2:waves){
      model_string <- paste0(model_string, "    ld", w, " =~ 1*cf", w, "\n")
    }

    # Latent change score means (constrained to 0)
    for(w in 2:waves){
      model_string <- paste0(model_string, "    ld", w, " ~ 0*1\n")
    }

    # Latent change score variances (constrained to 0)
    for(w in 2:waves){
      model_string <- paste0(model_string, "    ld", w, " ~~ 0*ld", w, "\n")
    }

    # Constant change factor (loadings = 1)
    model_string <- paste0(model_string, "    general =~ 1*ld2\n")
    for(w in 3:waves){
      model_string <- paste0(model_string, "          + 1*ld", w, "\n")
    }

    # Constant change factor mean
    model_string <- paste0(model_string, "    general ~", change.mean, "*1\n")

    # Constant change factor variance
    model_string <- paste0(model_string, "    general ~~", constant.change.var, "*general\n")

    # Constant change factor covariance with the initial true score
    model_string <- paste0(model_string, "    general ~~", cov.change.true, "*cf1\n")

    # Proportional effects (constrained equal)
    for(w in 2:waves){
      model_string <- paste0(model_string, "    ld", w, " ~ ", proportion.change,"* cf", w-1, "\n")
    }
    dat = lavaan::simulateData(model_string, ...)

    return(list(model = model_string, data = dat))
  }
