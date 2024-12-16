#' Filter and Sample Data by Groups
#'
#' This function filters and samples a data frame by specified groups.
#'
#' @param data A data frame to be filtered and sampled.
#' @param ... Columns to group by.
#' @param sample_size An integer specifying the number of samples to take from each group.
#' @param drop_fails Boolean that, if TRUE, will drop any columns that don't have enough samples
#'
#' @return A data frame that has been filtered and sampled by the specified groups.
random_filter_by_group <- function(data, ..., sample_size, drop_fails = FALSE) {
  # Error checking
  check_numeric_vector(sample_size)
  check_positive_values(sample_size)
  check_integer_vector(sample_size)

  # Capture the group columns and subset the vector
  group_cols_names <- vapply(rlang::quos(...), rlang::as_name, character(1))
  grp <- vctrs::vec_group_loc(data[, group_cols_names, drop = FALSE])

  # Sample the groups
  idx <- lapply(grp$loc, function(x) {
    if (vctrs::vec_size(x) < sample_size) {
      if (drop_fails) return(NULL)
      return(x)
    }
    if (vctrs::vec_size(x) == sample_size) return(x)
    sample(x, sample_size)
  })

  # Return the filtered and sampled data
  vctrs::vec_slice(data, unlist(idx))
}
