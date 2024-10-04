
#' Generate model syntax for a latent change score model
#'
#'
#' @param waves The number of waves (time points) in the model.
#' @param model_type Specify whether estimating a latent change score model for one variable or two.
#'
#' @return character string containing the Lavaan model syntax.
#'
#' @export
#'

model_syntax_change = function(
    waves = 10,
    model_type = c('latent_change_x', "latent_change_xy")) {

  if(model_type == "latent_change_x"){

      #latent true scores (loadings = 1)
  model_string <- ""

  # Define the common factors
  # This corresponds to the true score model
  for(w in 1:waves){
    model_string <- paste0(model_string, "    cf", w, " =~ 1*x", w, "\n")
  }

  # Lots of restrictions in this model
  # Set the Latent true score means (initial free, others = 0)
  # This is because of how change is constructed, as a factor.
  model_string <- paste0(model_string, "    cf1 ~ 1\n")
  for(w in 2:waves){
    model_string <- paste0(model_string, "    cf", w, " ~ 0*1\n")
  }

  # Latent true score variances (initial free, others = 0)
  # Again, this is because of how change is specified
  model_string <- paste0(model_string, "    cf1 ~~ start(5)*cf1\n")
  for(w in 2:waves){
    model_string <- paste0(model_string, "    cf", w, " ~~ 0*cf", w, "\n")
  }

  # Observed intercepts (fixed to 0)
  for(w in 2:waves){
    model_string <- paste0(model_string, "    x", w, " ~ 0*1\n")
  }

  # Observed residual variances
  for(w in 2:waves){
    model_string <- paste0(model_string, "    x", w, " ~~ sigma2_u*x", w, "\n")
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
  model_string <- paste0(model_string, "    general ~ start(15)*1\n")

  # Constant change factor variance
  model_string <- paste0(model_string, "    general ~~ general\n")

  # Constant change factor covariance with the initial true score
  model_string <- paste0(model_string, "    general ~~ cf1\n")

  # Proportional effects (constrained equal)
  for(w in 2:waves){
    model_string <- paste0(model_string, "    ld", w, " ~ start(-.2)*pi_m * cf", w-1, "\n")
  }
  }
  else{

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
    model_string <- paste0(model_string, "    cf_x1 ~~", "start(3)*cf_x1\n")
    for(w in 2:waves){
      model_string <- paste0(model_string, "    cf_x", w, " ~~ 0*cf_x", w, "\n")
    }

    # Observed intercepts (fixed to 0)
    for(w in 2:waves){
      model_string <- paste0(model_string, "    x", w, " ~ 0*1\n")
    }

    # Observed residual variances (constrained to equality)
    for(w in 2:waves){
      model_string <- paste0(model_string, "    x", w, " ~~","sigma2_u*x", w, "\n")
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
    model_string <- paste0(model_string, "    general_x ~", "start(15)*1\n")

    # Constant change factor variance
    model_string <- paste0(model_string, "    general_x ~~", "general_x\n")

    # Constant change factor covariance with the initial true score
    model_string <- paste0(model_string, "    general_x ~~", "cf_x1\n")


    # Proportional effects (constrained equal)
    for(w in 2:waves){
      model_string <- paste0(model_string, "    ld_x", w, " ~ start(-.2)*pi_m * cf_x", w-1, "\n")
    }


    # Define the common factors
    for(w in 1:waves){
      model_string <- paste0(model_string, "    cf_y", w, " =~ 1*x", w, "\n")
    }

    # Latent true score means (initial free, others = 0)
    model_string <- paste0(model_string, "    cf_y1 ~ 1\n")
    for(w in 2:waves){
      model_string <- paste0(model_string, "    cf_y", w, " ~ 0*1\n")
    }

    # Latent true score variances (initial free, others = 0)
    model_string <- paste0(model_string, "    cf_y1 ~~"," start(15)*cf_y1\n")
    for(w in 2:waves){
      model_string <- paste0(model_string, "    cf_y", w, " ~~ 0*cf_y", w, "\n")
    }

    # Observed intercepts (fixed to 0)
    for(w in 2:waves){
      model_string <- paste0(model_string, "    y", w, " ~ 0*1\n")
    }

    # Observed residual variances (constrained to equality)
    for(w in 2:waves){
      model_string <- paste0(model_string, "    y", w, " ~~","sigma2_u*y", w, "\n")
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
    model_string <- paste0(model_string, "    general_y ~", "start(15)*1\n")

    # Constant change factor variance
    model_string <- paste0(model_string, "    general_y ~~", "general_y\n")

    # Constant change factor covariance with the initial true score
    model_string <- paste0(model_string, "    general_y ~~", "cf_y1\n")

    # Proportional effects (constrained equal)
    for(w in 2:waves){
      model_string <- paste0(model_string, "    ld_y", w, " ~ start(-.2)*pi_m * cf_y", w-1, "\n")
    }


    ## Combining models

    # Coupling parameters
    for(w in 2:waves){
      model_string <- paste0(model_string, "    ld_x", w, " ~ ", " cf_y", w-1, "\n")
      model_string <- paste0(model_string, "    ld_y", w, " ~ ", " cf_x", w-1, "\n")
    }
    model_string <- paste0(model_string, "    general_x", " ~~ ","general_y", "\n")
    model_string <- paste0(model_string, "    cf_x1~~"," cf_y1", "\n")
    # Covariance between wave 1 x and y and the general change score
    model_string <- paste0(model_string, "    cf_x1~~"," general_y", "\n")
    model_string <- paste0(model_string, "    cf_y1~~"," general_x", "\n")

  }

  return(model_string)
}



