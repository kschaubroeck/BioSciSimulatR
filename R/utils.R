#' Create Cartesian Product
#'
#' This function creates a Cartesian product of the input vectors or lists and replicates each row a specified number of times.
#'
#' @param ... Vectors or lists to create the Cartesian product from.
#' @param .times Integer. The number of times to replicate each row. Default is 1.
#' @return A data.table containing the Cartesian product of the input vectors or lists, with each row replicated .times times.
create_cartesian_product <- function(..., .times = 1) {
  cartesian_product <- vctrs::vec_expand_grid(
    !!!rlang::list2(...), .vary = "fastest"
  )

  # Do we need some repetitions?
  if (any(.times > 1)) {
    return(vctrs::vec_rep_each(cartesian_product, .times, error_call = rlang::current_env(), times_arg = ".times"))
  }

  cartesian_product
}

#' Convert Factor to Matrix
#'
#' This function converts a factor column in a data frame to a matrix using one-hot encoding.
#'
#' @param data A data frame containing the factor column.
#' @param fct The name of the factor column to be converted.
#' @return A matrix with one-hot encoding of the factor column.
#' @importFrom rlang enquo quo_name
#' @importFrom stats model.matrix.default
factor2matrix <- function(data, fct) {
  fct <- data[[fct]]
  Matrix::sparse.model.matrix(~ 0 + fct, data = data)
}

# paste0 but with safer recycling behavoir
# https://www.tidyverse.org/blog/2023/04/performant-packages/#rewriting-tidy-code
vec_paste0 <- function (...) {
  args <- vctrs::vec_recycle_common(...)
  rlang::exec(paste0, !!!args)
}

#' Nest Data by Specified Columns
#'
#' This function nests a data frame by specified columns.
#'
#' @param data A data frame or tibble to be nested.
#' @param nest_cols A character vector of column names to nest by.
#' @param data_col A character string specifying the name of the column to store the nested data. Defaults to "data".
#' @return A tibble with nested data.
nest_data <- function(data, nest_cols, data_col = "data") {
  res <- vctrs::vec_split(
    x = data[setdiff(colnames(data), nest_cols)],
    by = data[nest_cols]
  )

  vctrs::vec_cbind(
    res$key,
    tibble::new_tibble(rlang::set_names(list(res$val), data_col))
  )
}

align_terms <- function(fixed_effects, random_effects) {
  aligned_random_effects <- rlang::set_names(double(vctrs::vec_size(fixed_effects)), rlang::names2(fixed_effects))
  aligned_random_effects[rlang::names2(random_effects)] <- random_effects
  aligned_random_effects
}


###############################################
###############################################
###############################################
###############################################





# Function to evaluate the terms in the data frame environment
evaluate_terms <- function(effect_sizes, data) {
  lapply(vctrs::vec_names(effect_sizes), function(term) {
    levels(rlang::eval_tidy(rlang::parse_expr(term), data))
  }) |> stats::setNames(vctrs::vec_names(effect_sizes))
}

# Function to convert interaction terms to the desired format
convert_interaction_terms <- function(interaction_terms, prefixes) {
  vapply(interaction_terms, function(term) {
    parts <- strsplit(term, ":")[[1]]
    interleaved <- vctrs::vec_interleave(prefixes, parts, ":")
    paste(interleaved[-length(interleaved)], collapse = "")
  }, character(1))
}


# Function to replace values based on matching names
replace_values_with_names <- function(vec1, vec2) {
  matched_indices <- match(names(vec1), vec2)
  matched_names <- names(vec2)[matched_indices]

  # Replace only the matched names
  names(vec1)[!is.na(matched_indices)] <- matched_names[!is.na(matched_indices)]

  return(vec1)
}

