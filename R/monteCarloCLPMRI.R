#' Run Monte Carlo simulations for CLPM, RI-CLPM, and Dual Change Models
#'
#' @param trials The number of trials for the Monte Carlo simulation.
#' @param waves The number of waves (time points) in the model.
#' @param model Specify whether an "ols" or "clpm" model.
#' @param data_generation Specify the data generation method: "clpm", "ri-clpm", "latent-change-x", "latent-change-xy".
#' @param proportion.change_x Proportion of change in x.
#' @param proportion.change_y Proportion of change in y.
#' @param ... Additional parameters.
#'
#' @return A data frame containing the results of the simulation.
#'
#' @export
monteCarlo <- function(
    trials = 100,
    waves = 10,
    model = c("ols", "clpm"),
    data_generation = c(
      "clpm",
      "ri-clpm",
      "latent-change-x",
      "latent-change-xy"),
    ## Latent Change Parameters
    proportion.change_x = 0.5,
    proportion.change_y = 0.5,
    change.mean_x = 0.3,
    change.mean_y = 0.3,
    coupling_xy = 0,
    coupling_yx = 0,
    ###

    within_person_stability_x = 0.1,
    within_person_stability_y = 0.1,
    cross_lag_x = 0,
    cross_lag_y = 0,
    confound = 0,
    variance_x = 1,
    variance_y = 1,
    sample_size = 1000,
    ...
) {
  model <- match.arg(model)
  data_generation <- match.arg(data_generation)

  if (model == "ols") {
    if (data_generation == "ri-clpm") {
      # Create a data frame to store simulation parameters
      simulation_parameters <- expand.grid(
        confound = confound,
        within_person_stability_x = within_person_stability_x,
        within_person_stability_y = within_person_stability_y
      ) %>% as.data.frame()

      # Initialize an empty list to store results
      results <- list()

      # Loop through each combination of parameters and trials
      for (i in 1:nrow(simulation_parameters)) {
        for (j in 1:trials) {
          params <- simulation_parameters[i, ]

          # Simulate data using `simulate_riclpm` (make sure this function is defined)
          dat <- simulate_riclpm(
            waves = waves,
            stability.p = params$within_person_stability_x,
            stability.q = params$within_person_stability_y,
            cross.p = cross_lag_x,
            cross.q = cross_lag_y,
            beta.u = params$confound,
            sample.nobs = sample_size,
            variance.p = variance_x,
            variance.q = variance_y
          )$data %>%
            reshape_long_sim_cr() # Assuming this function is also defined

          # Fit the OLS model and extract coefficients
          model_fit <- lm(y ~ -1 + xlag + ylag, dat)
          xvalue <- coef(model_fit)[["xlag"]]
          yvalue <- coef(model_fit)[["ylag"]]

          # Store results in the list
          results[[length(results) + 1]] <- data.frame(
            confound = params$confound,
            within_person_stability_x = params$within_person_stability_x,
            within_person_stability_y = params$within_person_stability_y,
            xlag = xvalue,
            ylag = yvalue,
            trial = j,
            model = model,
            data_generation = data_generation
          )
        }
      }
      results_df <- do.call(rbind, results)
    } else if (data_generation == "latent-change-xy") {
      simulation_parameters <- expand.grid(
        p1 = proportion.change_x,
        p2 = proportion.change_y,
        p3 = change.mean_x,
        p4 = change.mean_y
      ) %>% as.data.frame()

      results <- list()

      # Loop through each combination of parameters and trials
      for (i in 1:nrow(simulation_parameters)) {
        for (j in 1:trials) {
          print(paste("Simulation", j, "of", trials))

          params <- simulation_parameters[i, ]
          dat <- simulate_latent_change_multiple(
            proportion.change_x = params$p1,
            proportion.change_y = params$p2
          )$data %>%
            reshape_long_sim_cr() %>% as.data.frame()

          # Fit the OLS model and extract coefficients
          model_fit <- lm(y ~ xlag + ylag, dat)
          xvalue <- coef(model_fit)[["xlag"]]
          yvalue <- coef(model_fit)[["ylag"]]

          # Store results in the list
          results[[length(results) + 1]] <- data.frame(
            confound = 0,
            proportion_change_x = params$p1,
            proportion_change_y = params$p2,
            change.mean_x = params$p3,
            change.mean_y = params$p4,
            xlag = xvalue,
            ylag = yvalue,
            trial = j,
            model = model,
            data_generation = data_generation
          )
        }
      }
      results_df <- do.call(rbind, results)
    }
  }
  return(results_df)
}

