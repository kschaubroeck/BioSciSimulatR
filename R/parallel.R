#' Generate Cartesian Product and Apply Function Rowwise
#'
#' This function generates a Cartesian product of the input vectors or lists, applies a specified function to each row, and stores the results in a new column.
#'
#' @param ... Vectors or lists to create the Cartesian product from.
#' @param func A function to apply to each row of the data frame.
#' @param result_col The name of the new column to store the results.
#' @param startup_function An optional function to run before processing starts.
#' @param complete_function An optional function to run after each row is processed.
#' @param seed An integer seed for reproducibility.
#' @param parallel A logical value indicating whether to run the processing in parallel.
#' @param cores An integer specifying the number of cores to use for parallel processing.
#' @return A data frame with the results stored in a new column.
#' @export
df_apply_cartesian <- function(
    ...,
    func,
    result_col = "result",
    startup_function = NULL,
    complete_function = NULL,
    seed = 123,
    parallel = FALSE,
    cores = 1) {

  # Generate the Cartesian product
  data <- create_cartesian_product(!!!rlang::list2(...))

  # Apply the function rowwise and store the results in a new column
  data[[result_col]] <- lapply_rowwise(
    data = data,
    func = func,
    startup_function = startup_function,
    complete_function = complete_function,
    seed = seed,
    parallel = parallel,
    cores = cores
  )

  # Done
  tibble::as_tibble(data)
}

#' Apply a function to each row of a data frame
#'
#' This function applies a specified function to each row of a data frame, with options for parallel processing and additional startup and completion functions.
#'
#' @param data A data frame to process.
#' @param func A function to apply to each row of the data frame.
#' @param startup_function An optional function to run before processing starts.
#' @param complete_function An optional function to run after each row is processed.
#' @param seed An integer seed for reproducibility.
#' @param parallel A logical value indicating whether to run the processing in parallel.
#' @param cores An integer specifying the number of cores to use for parallel processing.
#' @return A list of results from applying the function to each row.
lapply_rowwise <- function(
    data,
    func,
    startup_function = NULL,
    complete_function = NULL,
    seed = 123,
    parallel = FALSE,
    cores = 1) {

  # Check if startup_function is supplied and parallel is FALSE
  if (!parallel && !is.null(startup_function)) {
    cli::cli_alert_info("The startup_function is ignored in non-parallel mode.")
  }

  # Define the function to process each row
  process_row <- function(row_index) {
    # Provide Console Updates if not parallel
    if (!parallel) {
      row_info <- glue::glue_collapse(glue::glue("{names(data)}: {data[row_index, ]}"), sep = "; ")
      cli::cli_alert_info(glue::glue("Processing row: ({row_info})"))
    }

    # Extract the row as a list
    data_row <- data[row_index, , drop = FALSE]
    data_list <- as.list(data_row, all.names = TRUE)

    # Ensure only one row is processed
    if (nrow(data_row) != 1) {
      cli::cli_abort("There are more than 1 row in this data. Something went wrong.")
    }

    # Setup seed
    seed <- seed + row_index
    data_list <- c(data_list, list(seed = seed))

    # Call the function
    result <- rlang::exec(func, !!!data_list)
    if (is.null(result)) {
      cli::cli_abort("Error: func returned NULL")
    }

    # Call the complete function if it is provided and is a function
    if (!is.null(complete_function) && is.function(complete_function)) {
      result <- rlang::exec(complete_function, result, seed)
      if (is.null(result)) {
        cli::cli_abort("Error: complete_function returned NULL")
      }
    }

    return(result)
  }

  # Run the processing either in parallel or sequentially
  if (!parallel) {
    return(lapply(seq_len(nrow(data)), process_row))
  }

  # Set up parallel backend
  cl <- parallel::makeCluster(cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  # Export necessary functions and variables to the worker nodes
  parallel::clusterExport(
    cl,
    varlist = c(
      "data",
      "seed",
      "func",
      "startup_function",
      "complete_function",
      "process_row"
    ),
    envir = environment()
  )

  # Execute startup function on each worker
  if (rlang::is_function(startup_function)) {
    parallel::clusterCall(cl, startup_function)
  }

  # Process the data in parallel
  parallel::parLapply(cl, seq_len(nrow(data)), process_row)
}

#' Nest Data and Apply Function to Each Nested Data Frame
#'
#' This function nests the data based on specified columns, applies a specified function to each nested data frame, and stores the results in a new column.
#'
#' @param data A data frame to nest.
#' @param nest_cols Columns to nest by.
#' @param func A function to apply to each nested data frame.
#' @param result_col The name of the new column to store the results.
#' @param startup_function An optional function to run before processing starts.
#' @param complete_function An optional function to run after each nested data frame is processed.
#' @param parallel A logical value indicating whether to run the processing in parallel.
#' @param cores An integer specifying the number of cores to use for parallel processing.
#' @return A data frame with the results stored in a new column.
df_apply_nested <- function(
    data,
    nest_cols,
    func,
    result_col = "result",
    data_col = "data",
    startup_function = NULL,
    parallel = FALSE,
    cores = 1) {

  # Check if startup_function is supplied and parallel is FALSE
  if (!parallel && !is.null(startup_function)) {
    cli::cli_alert_info("The startup_function is ignored in non-parallel mode.")
  }

  # Nest the data
  nested_data <- nest_data(data, nest_cols, data_col)

  # Run the processing either in parallel or sequentially
  if (!parallel) {
    nested_data[[result_col]] <- lapply(nested_data[[data_col]], func)
    return(nested_data)
  }

  # Set up parallel backend
  cl <- parallel::makeCluster(cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  # Export necessary functions and variables to the worker nodes
  parallel::clusterExport(
    cl,
    varlist = c(
      "data",
      "seed",
      "func",
      "startup_function",
      "process_row"
    ),
    envir = environment()
  )

  # Execute startup function on each worker
  if (rlang::is_function(startup_function)) {
    parallel::clusterCall(cl, startup_function)
  }

  # Par apply
  nested_data[[result_col]] <- parallel::parLapply(cl, nested_data[[data_col]], func)
  nested_data
}
