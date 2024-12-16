#' Simulate Dams
#'
#' This function simulates dams based on the provided parameters.
#'
#' @param dams_per_group A numeric vector indicating the number of dams per group.
#' @param ... Additional arguments representing biological groups.
#' @param batches An integer indicating the number of batches.
#' @param simulations An integer indicating the number of simulations.
#' @return A data frame containing the simulated dams.
simulate_dams <- function(dams_per_group, ..., batches = 1, simulations = 1) {
  # Basic Error Checking
  check_numeric_vector(dams_per_group)
  check_positive_values(dams_per_group)
  check_integer_vector(dams_per_group)

  check_numeric_vector(batches)
  check_positive_values(batches)
  check_integer_vector(batches)

  check_numeric_vector(simulations)
  check_positive_values(simulations)
  check_integer_vector(simulations)

  # Capture the args and do error checking
  biogroups <- rlang::list2(...)
  check_unique_names(biogroups)

  for (biogroup in biogroups) {
    check_character_vector(biogroup)
    check_unique_values(biogroup)
  }

  # Convert each item in biogroups to a factor
  biogroups <- lapply(biogroups, function(.x) factor(.x, levels = .x))

  # A bit of Math to determine how many dams per batch and then how many are
  # left over (in the case the number of dams per biogroup is not divisible by batches)
  n_dams_per_batch <- dams_per_group %/% batches
  n_dams_per_batch_remainder <- dams_per_group %% batches

  # Create factors for simulation and batch early
  simulation_factors <- factor(seq_len(simulations), levels = seq_len(simulations))
  batch_factors <- factor(seq_len(batches), levels = seq_len(batches))

  # Generate the Cartesian product -- all possible pairings of biological groups
  dams <- create_cartesian_product(
    simulation = simulation_factors,
    batch = batch_factors,
    !!!biogroups,
    .times = n_dams_per_batch
  )

  # If we need to create more and randomly assign them to batches
  if (n_dams_per_batch_remainder > 0) {
    remainder <- create_cartesian_product(
      simulation = simulation_factors,
      !!!biogroups,
      .times = n_dams_per_batch_remainder
    )
    remainder$batch <- sample(batch_factors, nrow(remainder), replace = TRUE)
    dams <- vctrs::vec_rbind(dams, remainder)
  }

  # Add a unique integer ID while grouping by the 'simulation' column
  litter_ids <- integer(nrow(dams))
  group_locs <- vctrs::vec_group_loc(dams$simulation)$loc
  litter_ids[unlist(group_locs)] <- unlist(lapply(group_locs, seq_along))
  dams$litter <- factor(litter_ids, levels = seq_len(max(litter_ids)))

  dams
}

#' Simulate Real Litters
#'
#' This function simulates real litters based on the provided parameters.
#'
#' @param dams A data frame containing the dams.
#' @param mean_litter_size A numeric value indicating the mean litter size.
#' @param ... Additional arguments representing biological groups.
#' @param min_litter_size An integer indicating the minimum litter size.
#' @param max_litter_size An integer indicating the maximum litter size.
#' @return A data frame containing the simulated offspring.
simulate_real_litters <- function(
    dams,
    mean_litter_size = 6,
    ...,
    min_litter_size = 1,
    max_litter_size = 16) {

  # Error checking
  check_numeric_vector(mean_litter_size)
  check_positive_values(mean_litter_size)
  check_integer_vector(mean_litter_size)

  check_numeric_vector(min_litter_size)
  check_positive_values(min_litter_size)
  check_integer_vector(min_litter_size)

  check_numeric_vector(max_litter_size)
  check_positive_values(max_litter_size)
  check_integer_vector(max_litter_size)

  # Extract the names and values from the ... arguments and perform error checking
  args <- rlang::list2(...)
  check_unique_names(args)

  for (.arg in args) {
    check_numeric_vector(.arg)
    check_unique_names(.arg)
    check_probability_vector(.arg)
  }

  # Pick a size for each litter
  # 1) Select whatever is smaller: the generated size of the max litter size
  # 2) Then, select whatever is larger: the prior size or the min litter size
  litter_size <- pmax(
    pmin(stats::rpois(nrow(dams), mean_litter_size), max_litter_size),
    min_litter_size
  )

  # Repeat each row the correct number of times to simulate offspring from litters
  offspring <- vctrs::vec_rep_each(dams, litter_size)

  # Add a new column for each name in the ... arguments and simulate from a multinomial distribution
  for (arg_name in rlang::names2(args)) {
    offspring[[arg_name]] <- generate_multinomial_factor(nrow(offspring), args[[arg_name]])
  }

  # Return
  offspring
}

#' Simulate Ideal Litters
#'
#' This function simulates ideal litters based on the provided parameters.
#'
#' @param dams A data frame containing the dams.
#' @param ... Additional arguments representing biological groups.
#' @param offspring_per_litter_group An integer indicating the number of offspring per litter group.
#' @return A data frame containing the simulated offspring.
simulate_ideal_litters <- function(
    dams,
    ...,
    offspring_per_litter_group = 1) {

  # Extract the names and values from the ... arguments and perform error checking
  args <- rlang::list2(...)

  # Error Checking of the types in the list
  args_unlisted <- unlist(args, recursive = FALSE)
  check_numeric_vector(args_unlisted)
  check_unique_names(args_unlisted)
  check_integer_vector(args_unlisted)

  # Create a list to store the results
  biogroups <- vector("list", length(args))
  names(biogroups) <- names(args)
  for (i in seq_along(args)) {
    .arg <- args[[i]]
    biogroups[[i]] <- factor(rep(rlang::names2(.arg), .arg), levels = rlang::names2(.arg))
  }

  # Expand all combinations of items
  # Generate the Cartesian product -- all possible pairings of biological groups
  offspring <- create_cartesian_product(
    litter = dams$litter, # factor(levels(dams$litter), levels = levels(dams$litter)),
    !!!biogroups,
    .times = offspring_per_litter_group
  )

  # Merge two items
  merge(offspring, dams, by = "litter")
}

#' Simulate Repeated Measurements
#'
#' This function simulates repeated measurements based on the provided parameters.
#'
#' @param animals A data frame containing the animals.
#' @param replicates Either a numeric value (integer or double) or a character vector indicating the replicates.
#' @param grouping_var A character string indicating the grouping variable.
#' @param id_col A character string indicating the ID column name.
#' @param column_name A character string indicating the column name for replicates.
#' @return A data frame containing the expanded data with repeated measurements.
simulate_repeated_measurements <- function(animals, replicates, grouping_var, id_col = "animal", column_name = "replicate") {
  # parse input
  if (rlang::is_double(replicates) || rlang::is_integer(replicates)) {
    check_integer_vector(replicates)
    labels <- vec_paste0("R", seq_len(replicates))
    num_replicates <- replicates
  } else if (rlang::is_character(replicates)) {
    check_unique_values(replicates)
    labels <- replicates
    num_replicates <- vctrs::vec_size(replicates)
  } else {
    cli::cli_abort("`replicates` must be either a numeric value (integer or double) or a character vector. Received input of type: {typeof(replicates)}. Please provide a valid input.")
  }

  # Safety checking
  if (any(colnames(animals) == column_name)) {
    cli::cli_abort("The column name provided already exists in the data frame. Please provide a different column name.")
  }

  # Create an ID column
  ids <- integer(nrow(animals))
  group_locs <- vctrs::vec_group_loc(animals[[grouping_var]])$loc
  ids[unlist(group_locs)] <- unlist(lapply(group_locs, seq_along))
  animals[[id_col]] <- factor(ids, levels = seq_len(max(ids)))

  # Create a data frame with the replicates
  expanded <- vctrs::vec_rep_each(animals, num_replicates)
  expanded[[column_name]] <- factor(vctrs::vec_rep(labels, times = nrow(animals)), levels = labels)
  expanded
}
