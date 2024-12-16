#' Simulate Pups Response
#'
#' This function simulates the response of pups based on the provided parameters.
#'
#' @param formula The formula for the model response.
#' @param fixed_effects_coefficients A named vector of fixed effects coefficients.
#' @param dams_per_group Number of dams per group.
#' @param ... Additional arguments passed to `simulate_dams`.
#' @param simulations Number of simulations.
#' @param batches Number of batches.
#' @param ideal Logical indicating whether to simulate ideal litters.
#' @param offspring_per_litter_group Number of offspring per litter group.
#' @param mean_litter_size Mean litter size.
#' @param min_litter_size Minimum litter size.
#' @param max_litter_size Maximum litter size.
#' @param pup_characteristics List of pup characteristics.
#' @param random_effects_sd List of random effects standard deviations.
#' @param random_effects_corr_matrix List of random effects correlation matrices.
#' @param distribution Distribution type for the model response.
#' @param sigma Standard deviation of the residuals.
#' @param weights Weights for the model response.
#' @param dispersion Dispersion parameter for the model response.
#'
#' @return A data frame with the simulated pups and their responses.
#' @export
simulate_pups_response <- function(
    formula,
    fixed_effects_coefficients,
    dams_per_group,
    ...,
    simulations = 1,
    batches = 1,
    ideal = TRUE,
    offspring_per_litter_group = 1,
    mean_litter_size = 6,
    min_litter_size = 1,
    max_litter_size = 16,
    pup_characteristics = list(
      sex = c(female = 1, male = 1)
    ),
    random_effects_sd = list(),
    random_effects_corr_matrix = list(),
    distribution = "gaussian",
    sigma = 1,
    weights = NULL,
    dispersion = NULL) {

  experimental_groups <- rlang::list2(...)

  # Simulate dams
  dams <- simulate_dams(
    dams_per_group = dams_per_group,
    !!!experimental_groups,
    simulations = simulations,
    batches = batches
  )

  if (ideal) {
    pups <- simulate_ideal_litters(
      dams,
      offspring_per_litter_group = 1,
      !!!pup_characteristics
    )
  } else {
    pups <- simulate_real_litters(
      dams,
      mean_litter_size = mean_litter_size,
      min_litter_size = min_litter_size,
      max_litter_size = max_litter_size,
      !!!pup_characteristics
    )
  }

  # Create temporary interaction factors based on random_effects_sd names
  # Create temporary interaction factors based on random_effects_sd names
  interaction_names <- vec_paste0("simulation.", names(random_effects_sd))
  pups[interaction_names] <- lapply(
    names(random_effects_sd), function(name) (pups$simulation):(pups[[name]])
  )

  # temp update of names
  names(random_effects_sd) <- interaction_names

  # Prepare additional arguments for model_response
  additional_args <- list()
  if (!is.null(sigma)) additional_args$sigma <- sigma
  if (!is.null(weights)) additional_args$weights <- weights
  if (!is.null(dispersion)) additional_args$dispersion <- dispersion

  pups$response <- model_response(
    pups,
    formula,
    fixed_effects_coefficients = fixed_effects_coefficients,
    random_effects_sd = random_effects_sd,
    random_effects_corr_matrix = random_effects_corr_matrix,
    distribution = "gaussian",
    #
    !!!additional_args
  )

  # Remove temporary interaction factors
  # Remove temporary interaction factors
  pups[interaction_names] <- NULL
  tibble::as_tibble(pups)
}



#' Apply Simulation Function
#'
#' This function applies a given function to a nested data frame with `nest_cols = "simulation"`.
#'
#' @param df A data frame to be nested and processed.
#' @param funct A function to be applied to each nested data frame.
#'
#' @return A data frame with the results of the applied function.
#' @export
apply_simulations <- function(df, funct) {
  result <- df_apply_nested(
    df, nest_cols = "simulation",
    func = funct
  )
  return(result)
}
