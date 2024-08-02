# Hello, world!
#
# This is an example function named 'hello' 
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

generate_data <- function(waves = WAVES,
                          sample_size = SAMPLES,
                          instrument_slopes = INSTRUMENT_SLOPE,
                          unobserved_slopes = UNOBSERVED_SLOPE,
                          stability_slopes = STABILITY_SLOPE,
                          cross_lag_slopes = CROSS_LAG,  # Fix: Default for multiple waves
                          contemporaneous_slopes = CURRENT, # Fix: Default for multiple waves
                          corr.zu = CORR.ZU) {
  # # Input Checks (Improved)
  # stopifnot(
  #   length(cross_lag_slopes) == waves - 1,
  #   length(contemporaneous_slopes) == waves,
  #   waves >= 2  # Ensure at least two waves
  # )
  
  # Draw nodes without ancestors
  # Vary the correlation between the exogenous variables
  exogenous <- matrix(c(1, corr.zu, corr.zu, 1), nrow = 2)
  uz <- MASS::mvrnorm(sample_size, mu = c(0, 0), Sigma = exogenous)
  
  # Initialize Data (Simplified):
  # (u, z) -> (x1, u, z) -> y1
  data <- tibble(
    x1 = uz %*% c(unobserved_slopes, instrument_slopes) + rnorm(sample_size, 0, 1),
    y1 = uz[, 1] * unobserved_slopes + x1 * contemporaneous_slopes[1] + rnorm(sample_size, 0, 1)
  )
  
  # Grab X and Y Lags to generate feed forward predictions
  for (i in 2:waves) {
    x_lag <- data[[paste0("x", i - 1)]]
    y_lag <- data[[paste0("y", i - 1)]]
    # DGP  
    data <- data %>%
      mutate(
        !!paste0("x", i) := uz[, 1] * unobserved_slopes  + x_lag * stability_slopes + y_lag * cross_lag_slopes[1]  + rnorm(sample_size, 0, 1),
        !!paste0("y", i) := uz[, 1] * unobserved_slopes +  y_lag * stability_slopes + x_lag * cross_lag_slopes[2]  +  .data[[paste0("x", i)]] * contemporaneous_slopes[i] + rnorm(sample_size, 0, 1) 
      )
  }
  

  # Return data and parameters (Improved)
  list(
    data = data %>% select(matches("^(x|y|z|u)")),  # Select relevant variables
    parameters = list(
      waves = waves,
      sample_size = sample_size,
      instrument_slopes = instrument_slopes,
      unobserved_slopes = unobserved_slopes,
      stability_slopes = stability_slopes,
      cross_lag_slopes = cross_lag_slopes,
      contemporaneous_slopes = contemporaneous_slopes,
      corr.zu = corr.zu
    )
  )
}




