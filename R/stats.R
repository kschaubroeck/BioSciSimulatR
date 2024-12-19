
#' Simulate Model
#'
#' This function simulates effects based on the user's choice of distribution.
#'
#' @param data A data frame with the original data used to generate the model matrix.
#' @param fixed_effects_formula A formula specifying the model.
#' @param fixed_effects_coefficients A vector of fixed effects coefficients.
#' @param random_effects_sd A named list of standard deviations for each random effect (default is NULL).
#' @param random_effects_corr_matrix A named list of correlation matrices for each random effect (default is an empty list).
#' @param distribution A character string specifying the distribution ("gaussian", "binomial", "poisson", "gamma").
#' @param ... Additional parameters for the distribution functions.
#' @return A vector representing the simulated response.
model_response <- function(
    data,
    fixed_effects_formula,
    fixed_effects_coefficients,
    random_effects_sd = NULL,
    random_effects_corr_matrix = list(),
    distribution = "gaussian",
    ...) {

  # Check if the distribution is recognized
  if (!any(distribution == c("gaussian", "binomial", "poisson", "gamma"))) {
    cli::cli_abort("Unsupported distribution: {distribution}. Supported distributions are 'gaussian', 'binomial', 'poisson', and 'gamma'.")
  }

  # Check required variables
  args <- rlang::list2(...)

  # Check if 'sigma' is provided and is a positive numeric value
  if (distribution == "gaussian") {
    if (!rlang::has_name(args, "sigma")) {
      cli::cli_abort("Parameter 'sigma' is required for Gaussian distribution.")
    }
    check_numeric_vector(args$sigma)
    check_positive_values(args$sigma)
  }

  # Check if 'weights' is provided and is a positive integer vector
  if (distribution == "binomial") {
    if (!rlang::has_name(args, "weights")) {
      cli::cli_abort("Parameter 'weights' is required for Binomial distribution.")
    }
    check_numeric_vector(args$weights)
    check_integer_vector(args$weights)
  }

  # Check if 'dispersion' is provided and is a positive numeric value
  if (distribution == "gamma") {
    if (!rlang::has_name(args, "dispersion")) {
      cli::cli_abort("Parameter 'dispersion' is required for Gamma distribution.")
    }
    check_numeric_vector(args$dispersion)
    check_positive_values(args$dispersion)
  }

  # Generate model matrix from formula and data
  # Performance Improvement: Drop the attempt to name rows. This speeds up this step
  # by ~500ms in a few of my tests (16 million rows). However, some downstream steps are improved
  # even more, particularity when subsetting. Overall, there was about a ~5 second improvement
  # in simulation time these extreme simulations.
  model_matrix <- Matrix::sparse.model.matrix(
    fixed_effects_formula,
    data = data,
    row.names = FALSE
  )
  colnames(model_matrix)[colnames(model_matrix) == "(Intercept)"] <- "Intercept"

  # Make sure all beta values are in the model matrix
  check_names_match(fixed_effects_coefficients, model_matrix)

  # Check if random slopes and intercepts are located in the model matrix
  if (!rlang::is_empty(random_effects_sd)) {
    check_names_match(
      vctrs::list_unchop(random_effects_sd, name_spec = "{inner}"),
      model_matrix
    )
  }

  # Calculate expectation
  expectation <- calculate_expectation(
    data,
    model_matrix,
    fixed_effects_coefficients,
    random_effects_sd,
    random_effects_corr_matrix
  )

  # Generate response based on the specified distribution
  # Generate response based on the specified distribution
  response <- switch(distribution,
                     gaussian = rlang::exec(generate_response_gaussian, expectation, !!!args),
                     binomial = rlang::exec(generate_response_binomial, expectation, !!!args),
                     poisson = rlang::exec(generate_response_poisson, expectation, !!!args),
                     gamma = rlang::exec(generate_response_gamma, expectation, !!!args),
                     cli::cli_abort("Unsupported distribution: {distribution}. Supported distributions are 'gaussian', 'binomial', 'poisson', and 'gamma'."))

  response
}

#' Calculate Expectation
#'
#' This function calculates the expectation of a model given the fixed effects coefficients and random effects.
#'
#' @param data. A data frame with the original data used to generate the model matrix.
#' @param model_matrix A matrix representing the model.
#' @param fixed_effects_coefficients A vector of fixed effects coefficients.
#' @param random_effects_sd A named list of standard deviations for each random effect (default is NULL).
#' @param random_effects_corr_matrix A named list of correlation matrices for each random effect (default is an empty list).
#' @return A vector representing the calculated expectation.
calculate_expectation <- function(
    data,
    model_matrix,
    fixed_effects_coefficients,
    random_effects_sd = NULL,
    random_effects_corr_matrix = list()) {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Begin Fixed Effects
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Calculate Fixed Effects and sort them IF needed
  if (needs_sorting(fixed_effects_coefficients, model_matrix)) {
    # Ensure the correct order of fixed_effects_coefficients
    fixed_effects_coefficients <- fixed_effects_coefficients[match(names(fixed_effects_coefficients), colnames(model_matrix))]
    #model_matrix <- model_matrix[, rlang::names2(fixed_effects_coefficients)]
  }

  fixed_effects_component <- (model_matrix %*% fixed_effects_coefficients)[, 1]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Begin Random Effects
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!rlang::is_empty(random_effects_sd)) {
    # Make a sparse matrix whose dimensions match the model matrix. This will be the
    # sum of all the random effects of each sample/term.
    random_effects_component <- Matrix::Matrix(0, nrow = nrow(model_matrix), ncol = ncol(model_matrix), sparse = TRUE)

    for (i in seq_along(random_effects_sd)) {
      group <- names(random_effects_sd)[i]

      # Generate a random effects matrix for each group
      group_effect <- make_random_effect_matrix(
        data[[group]],
        align_terms(fixed_effects_coefficients, random_effects_sd[[group]]),
        random_effects_corr_matrix[[group]]
      )

      # We add the new effects to the random components matrix using the built in
      # functions from the Matrix Package for sparse matrices.
      # Performance Improvement: IN my large simulations (16 million items from 1000 simulations), the
      # drop unused levels component speeds up the process. I am guessing since it doesn't try to fiddle with a
      # a large factor.
      random_effects_component <- random_effects_component + Matrix::crossprod(Matrix::fac2sparse(data[[group]], drop.unused.levels = FALSE), group_effect)
    }

    # Now, we can do the multiplication ONLY once and it will apply to all terms correctly
    random_effects_component <- Matrix::rowSums(random_effects_component * model_matrix)
  } else {
    random_effects_component <- 0
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Mixed Effects
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fixed_effects_component + random_effects_component
}

#' Calculate Random Effect Matrix
#'
#' This function calculates a random effect matrix for each group defined by a factor in the data frame.
#'
#' @param group_factor A factor that defines the groups.
#' @param random_slopes A numeric vector of standard deviations for each random slope.
#' @param corr_matrix A correlation matrix.
#' @return A matrix of random effects for each group.
make_random_effect_matrix <- function(group_factor, random_slopes, corr_matrix) {
  group_levels <- levels(group_factor)

  # Sample from the multivariate norm
  dist <- sample_mvnorm(
    vctrs::vec_size(group_levels),
    cov_matrix = cov_matrix_from_corr(random_slopes, corr_matrix)
  )

  # Name, format, and return the matrix
  rownames(dist) <- group_levels
  colnames(dist) <- names(random_slopes)
  dist
}

#' Generate Gaussian Response
#'
#' Generates the response variable for a linear model with Gaussian errors.
#'
#' @param mu Numeric vector of means.
#' @param sigma Numeric value of standard deviation.
#' @return Numeric vector of response values.
generate_response_gaussian <- function(mu, sigma = 1) {
  mu + stats::rnorm(length(mu), 0, sigma)
}

#' Generate Binomial Response
#'
#' Generates the response variable for a binomial model with a specified link function.
#'
#' @param eta Numeric vector of linear predictors.
#' @param weights Numeric value of weights.
#' @param link Character string specifying the link function.
#' @return Numeric vector of response values.
generate_response_binomial <- function(eta, weights = 1, link = "logit") {
  linkinv <- switch(link,
                    logit = function(eta) 1 / (1 + exp(-eta)),
                    probit = function(eta) stats::pnorm(eta),
                    cloglog = function(eta) 1 - exp(-exp(eta)),
                    cli::cli_abort("Unsupported link function: {link}. Supported link functions are 'logit', 'probit', and 'cloglog'."))
  mu <- linkinv(eta)
  stats::rbinom(length(mu), size = weights, prob = mu)
}

#' Generate Poisson Response
#'
#' Generates the response variable for a Poisson model with a specified link function.
#'
#' @param eta Numeric vector of linear predictors.
#' @param link Character string specifying the link function.
#' @return Numeric vector of response values.
generate_response_poisson <- function(eta, link = "log") {
  linkinv <- switch(link,
                    log = function(eta) exp(eta),
                    identity = function(eta) eta,
                    sqrt = function(eta) eta^2,
                    cli::cli_abort("Unsupported link function: {link}. Supported link functions are 'log', 'identity', and 'sqrt'."))
  mu <- linkinv(eta)
  stats::rpois(length(mu), lambda = mu)
}

#' Generate Gamma Response
#'
#' Generates the response variable for a Gamma model with a specified link function.
#'
#' @param eta Numeric vector of linear predictors.
#' @param dispersion Numeric value of dispersion parameter.
#' @param link Character string specifying the link function.
#' @return Numeric vector of response values.
generate_response_gamma <- function(eta, dispersion = 1, link = "log") {
  linkinv <- switch(link,
                    log = function(eta) exp(eta),
                    identity = function(eta) eta,
                    inverse = function(eta) 1 / eta,
                    cli::cli_abort("Unsupported link function: {link}. Supported link functions are 'log', 'identity', and 'inverse'."))
  mu <- linkinv(eta)
  shape <- 1 / dispersion
  scale <- mu / shape
  stats::rgamma(length(mu), shape = shape, scale = scale)
}

#' Compute Covariance Matrix from Correlation Matrix
#'
#' Computes the covariance matrix from standard deviations and a correlation matrix.
#'
#' @param sds Numeric vector of standard deviations.
#' @param corr_matrix Numeric matrix of correlations.
#' @return Numeric covariance matrix.
cov_matrix_from_corr <- function(sds, corr_matrix = NULL) {
  if (is.null(corr_matrix)) {
    corr_matrix <- diag(length(sds))
  }
  corr_matrix[which(sds == 0), ] <- corr_matrix[,which(sds == 0)] <- 0
  corr_matrix * (sds %*% t(sds))
}

#' Sample from Multivariate Normal Distribution
#'
#' Generates samples from a multivariate normal distribution.
#'
#' @param n Integer number of samples.
#' @param cov_matrix Numeric covariance matrix.
#' @param mean Numeric vector of means.
#' @return Numeric matrix of samples.
sample_mvnorm <- function(n, cov_matrix, mean = NULL) {
  if (is.null(mean)) {
    mean <- rep(0, ncol(cov_matrix))
  }
  # TODO: Work on implementation of the sparse version of mvnorm and when it works
  # if (sum(colSums(cov_matrix) != 0) < 3) {
  #   return(mvrnorm_sparse(n = n, mu = mean, Sigma = cov_matrix))
  # }
  MASS::mvrnorm(n = n, mu = mean, Sigma = cov_matrix)
}


mvrnorm_sparse <-function(n = 1, mu, Sigma, tol=1e-6) {
  p <- length(mu)
  if(!all(dim(Sigma) == c(p,p))) stop("incompatible arguments")

  eS <- eigen(Sigma, symmetric = TRUE)
  ev <- eS$values
  if(!all(ev >= -tol*abs(ev[1L]))) stop("'Sigma' is not positive definite")

  # Determine non-empty columns and convert to numeric indices
  X <- Matrix::Matrix(drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p), sparse = TRUE) %*% Matrix::t(sparse_rnorm(n, p, which(colSums(Sigma) != 0)))

  nm <- names(mu)
  if (is.null(nm) && !is.null(dn <- dimnames(Sigma))) nm <- dn[[1L]]
  dimnames(X) <- list(nm, NULL)
  if (n == 1) drop(X) else Matrix::t(X)
}

sparse_rnorm <- function(nrow, ncol, non_empty_columns) {
  Matrix::sparseMatrix(
    i = vctrs::vec_rep_each(seq_len(nrow), times = length(non_empty_columns)),
    j = vctrs::vec_rep(non_empty_columns, times = nrow),
    x = stats::rnorm(vctrs::vec_size(non_empty_columns) * nrow),
    dims = c(nrow, ncol)
  )
}

#' Create Correlation Matrix
#'
#' This function creates a correlation matrix from a data frame with three columns: effect1, effect2, and correlation.
#'
#' @param correlation_data A data frame with three columns: effect1, effect2, and correlation.
#' @param arg The argument name to use in error messages.
#' @param call The environment to use for error messages.
#' @return A correlation matrix.
build_correlation_matrix <- function(correlation_data, arg = rlang::caller_arg(correlation_data), call = rlang::caller_env()) {
  if (ncol(correlation_data) != 3) {
    cli::cli_abort("The elements of {.arg {arg}} must contain 3 columns.", call = call)
  }

  effect1 <- correlation_data[[1]]
  effect2 <- correlation_data[[2]]
  correlation <- correlation_data[[3]]

  # TODO: Check for duplication (pairs of effects that match)

  if (!vctrs::vec_is(effect1, character()) || !vctrs::vec_is(effect2, character())) {
    cli::cli_abort("The first two columns of {.arg {arg}} must be character vectors.", call = call)
  }

  if (!vctrs::vec_is(correlation, numeric())) {
    cli::cli_abort("The third column of {.arg {arg}} must be numeric.", call = call)
  }

  effect1 <- correlation_data[[1]]
  effect2 <- correlation_data[[2]]
  correlation <- correlation_data[[3]]

  # Get names of all the effects
  effect_names <- vctrs::vec_unique(vctrs::vec_c(effect1, effect2))

  # Initialize the correlation matrix with the correct dimensions and names
  cor_matrix <- matrix(0, nrow = length(effect_names), ncol = length(effect_names), dimnames = list(effect_names, effect_names))

  # Fill the correlation matrix using vctrs functions
  idx1 <- vctrs::vec_match(effect1, effect_names)
  idx2 <- vctrs::vec_match(effect2, effect_names)

  cor_matrix[as.matrix(vctrs::vec_cbind(V1 = idx1, V2 = idx2))] <- correlation
  cor_matrix[as.matrix(vctrs::vec_cbind(V1 = idx2, V2 = idx1))] <- correlation

  # Set diagonal to 1
  diag(cor_matrix) <- 1
  cor_matrix
}

# Simple function to assign items randomly to groups using the probability of
# each group and a multinomial distribution (generalized binomial distribution)
generate_multinomial_factor <- function(n, prob_vector) {
  factor(
    names(prob_vector)[
      max.col(t(stats::rmultinom(n = n, size = 1, prob = prob_vector)))
    ],
    levels = names(prob_vector)
  )
}

