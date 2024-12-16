#' Check if a vector is a valid probability vector
#'
#' @param x A numeric vector.
#' @param arg The argument name.
#' @param call The calling environment.
#' @return Throws an error if the vector is not a valid probability vector.
check_probability_vector <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (any(x < 0) || any(x > 1)) {
    cli::cli_abort("{.arg {arg}} contains elements outside the range [0, 1]. Each element must be between 0 and 1.", call = call)
  }
  if (sum(x) != 1) {
    cli::cli_abort("The elements of {.arg {arg}} must sum to 1. The current sum is {sum(x)}.", call = call)
  }
}

#' Check if a vector contains only integers
#'
#' @param x A numeric vector.
#' @param arg The argument name.
#' @param call The calling environment.
#' @return Throws an error if the vector contains non-integer values.
check_integer_vector <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (any(x %% 1 != 0)) {
    cli::cli_abort("{.arg {arg}} contains non-integer values. All elements must be integers.", call = call)
  }
}

#' Check if a vector has unique names
#'
#' @param x A named vector.
#' @param arg The argument name.
#' @param call The calling environment.
#' @return Throws an error if the vector does not have unique names.
check_unique_names <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (length(x) > 0L && !rlang::is_named(x)) {
    cli::cli_abort("All elements of {.arg {arg}} must be named.", call = call)
  }
  if (vctrs::vec_duplicate_any(names(x))) {
    cli::cli_abort("The names of {.arg {arg}} must be unique.", call = call)
  }
}

#' Check if a vector contains values within a specific range
#'
#' @param x A numeric vector.
#' @param min The minimum value.
#' @param max The maximum value.
#' @param arg The argument name.
#' @param call The calling environment.
#' @return Throws an error if the vector contains values outside the specified range.
check_range_vector <- function(x, min, max, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (any(x < min) || any(x > max)) {
    cli::cli_abort("{.arg {arg}} must contain values in the range [{min}, {max}].", call = call)
  }
}

#' Check if a vector is numeric
#'
#' @param x A vector.
#' @param arg The argument name.
#' @param call The calling environment.
#' @return Throws an error if the vector is not numeric.
check_numeric_vector <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.numeric(x)) {
    cli::cli_abort("{.arg {arg}} must be a numeric vector.", call = call)
  }
}

#' Check if a vector contains only positive values
#'
#' @param x A numeric vector.
#' @param arg The argument name.
#' @param call The calling environment.
#' @return Throws an error if the vector contains non-positive values.
check_positive_values <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (any(x <= 0)) {
    cli::cli_abort("{.arg {arg}} must contain only positive values.", call = call)
  }
}

#' Check if Names in a Vector Match Names in a Data Frame
#'
#' This function checks if all names in a given vector are present in a data frame.
#' Optionally, it can also check if the order of names in the vector matches the order in the data frame.
#'
#' @param vec A named vector whose names are to be checked.
#' @param df A data frame whose column names are to be checked against the vector names.
#' @param check_order A logical value indicating whether to check if the order of names in the vector matches the order in the data frame. Default is FALSE.
#' @param vec_arg The argument name for the vector, used for error messaging. Default is the caller's argument name for `vec`.
#' @param df_arg The argument name for the data frame, used for error messaging. Default is the caller's argument name for `df`.
#' @param call The environment to use for error messaging. Default is the caller's environment.
#'
#' @return This function does not return a value. It is used for its side effect of throwing an error if the names do not match.
#' @importFrom rlang caller_arg caller_env names2
#' @importFrom vctrs vec_in
#' @importFrom cli cli_abort
check_names_match <- function(vec, df, check_order = FALSE, vec_arg = rlang::caller_arg(vec), df_arg = rlang::caller_arg(df), call = rlang::caller_env()) {
  # Get the names of the vector and the data.frame
  vec_names <- rlang::names2(vec)
  df_names <- colnames(df)

  # Check if all names in the vector are present in the data.frame
  if (!all(vctrs::vec_in(vec_names, df_names))) {
    missing_names <- setdiff(vec_names, df_names)
    cli::cli_abort(
      c(
        "Not all names in the vector are present in the data.frame.",
        "x" = "The following names are missing in {.arg {df_arg}}: {.val {missing_names}}"
      ),
      call = call
    )
  }

  # If order matters, check if the order matches
  if (check_order) {
    if (!all(vec_names == df_names)) {
      cli::cli_abort(
        c(
          "The order of names in the vector does not match the order in the data.frame.",
          "x" = "Names in {.arg {vec_arg}}: {.val {vec_names}}",
          "x" = "Names in {.arg {df_arg}}: {.val {df_names}}"
        ),
        call = call
      )
    }
  }
}

#' Check if Names in a Vector Need Sorting to Match Names in a Data Frame
#'
#' This function checks if the names in a given vector need to be sorted to match the order of names in a data frame.
#'
#' @param vec A named vector whose names are to be checked.
#' @param df A data frame whose column names are to be checked against the vector names.
#'
#' @return A logical value indicating whether the names in the vector need to be sorted to match the order in the data frame.
needs_sorting <- function(vec, df) {
  # Get the names of the vector and the data.frame
  vec_names <- rlang::names2(vec)
  df_names <- rlang::names2(df)

  # Check if the order of names in the vector matches the order in the data.frame
  return(!all(vec_names == df_names))
}

#' Check if a vector contains unique values
#'
#' @param x A vector.
#' @param arg The argument name.
#' @param call The calling environment.
#' @return Throws an error if the vector contains duplicate values.
check_unique_values <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (anyDuplicated(x)) {
    cli::cli_abort("{.arg {arg}} must contain unique values. Duplicate values found.", call = call)
  }
}

#' Check if a vector is of type character
#'
#' @param x A vector.
#' @param arg The argument name.
#' @param call The calling environment.
#' @return Throws an error if the vector is not of type character.
check_character_vector <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!rlang::is_character(x)) {
    cli::cli_abort("{.arg {arg}} must be a character vector.", call = call)
  }
}

#' Check if a vector is of type logical
#'
#' @param x A vector.
#' @param arg The argument name.
#' @param call The calling environment.
#' @return Throws an error if the vector is not of type logical.
check_logical_vector <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!rlang::is_logical(x)) {
    cli::cli_abort("{.arg {arg}} must be a logical vector.", call = call)
  }
}

#' Check if a vector is of type factor
#'
#' @param x A vector.
#' @param arg The argument name.
#' @param call The calling environment.
#' @return Throws an error if the vector is not of type factor.
check_factor_vector <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.factor(x)) {
    cli::cli_abort("{.arg {arg}} must be a factor vector.", call = call)
  }
}

#' Check if an object is a function
#'
#' @param x An object.
#' @param arg The argument name.
#' @param call The calling environment.
#' @return Throws an error if the object is not a function.
check_function <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!rlang::is_function(x)) {
    cli::cli_abort("{.arg {arg}} must be a function.", call = call)
  }
}
