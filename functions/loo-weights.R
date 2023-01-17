#' Functions for Estimating Stacking Weights for Bayesian Models
#'
#' A function to calculate to model weights based on their predictive
#' performance via either loo or k-fold cross validation with optional
#' Bayesian Bootstrap.
#'
#' @aliases stacking_weights
#' @importFrom bayesboot rudirichlet
#' @importFrom purrr map2_dbl
#'
#' @param x A list of two or more model objects of class `brmsfit`. The
#' fitted models should contain either loo-cv or kfold criteria. See
#' `?brms::add_criterion` for further details.
#'
#' @param weights A string argument indicating the criteria to use for
#' estimating the model weights. Supported options include the default
#' `"loo"` for PSIS LOO-CV based weights but can also be set to `"kfold"`
#' which can be used for weights based on either held out validation or
#' exact LOO-CV.
#'
#' @param model_names a character vector containing the names of the models
#' the same length as `x`. Defaults to `NULL`, in which case each model weight
#' weight is assigned a name of the form M1, M2, and so on.
#'
#' @param bb_draws Logical argument indicating whether to apply Bayesian
#' Bootstrap via a uniform Dirichlet distribution to the weights prior to
#' scaling. Defaults to `FALSE` which assumes the weights are fixed. If
#' setting this to `TRUE` be sure to set the rng seed to ensure the weights
#' are locally reproducible.
#'
#' @param n An integer argument indicating the number of draws from the
#' uniform Dirichlet to use for Bayesian Bootstrap. This argument is passed
#' to `bayesboot::rudirichlet` and must be specified if `bb_draws` is `TRUE`.
#'
#' @param ... Additional arguments for future development. Currently unused.
#'
#' @return The function returns a numeric vector of weights corresponding to
#' each model which can be passed to the `brms::pp_average` or 
#' `brms::posterior_average` to obtain a stacked posterior distribution.
#'
#' @export stacking_weights
stacking_weights <- function(x,
                             weights = "loo",
                             bb_draws = FALSE,
                             model_names = NULL,
                             n = NULL,
                             seed = NULL,
                             ...) {

  # Validate the model list and weights arguments
  weights <- .check_weights(x, weights)

  # Find or create model names
  if (length(model_names) == length(x)) {
    model_names <- model_names
  }
  else {
    model_names <- paste("M", seq_along(x), sep = "")
  }

  # Extract the weights
  loo_ic_est <- purrr::map2_dbl(
    .x = seq_along(x),
    .y = weights,
    ~ x[[.x]]$criteria[[.y]]$estimates[3, 1]
  )

  # Calculate the relative difference in performance criteria
  ic_diffs <- loo_ic_est - min(loo_ic_est)

  # Relative model performance criteria
  out <- exp(-ic_diffs/2)

  # Optional Bayesian Bootstrap, which I'm pretty sure makes sense in this context
  if (isTRUE(bb_draws)) {
    set.seed(seed)
    ru <- bayesboot::rudirichlet(n, length(out))
    draws <- matrix(rep(out, each = n), ncol = length(out))
    out <- colMeans(draws * ru)
  }

  # Scale the weights to sum to 1
  out <- out/sum(out)

  # Apply model names
  names(out) <- model_names

  # Return the weights
  return(out)
}

# Helper Function to check whether model criterion is already stored
.check_weights <- function(x, weights) {

  # Check the class of the model objects
  for (i in seq_along(x)) {
    stopifnot(class(x[[i]]) == "brmsfit")
  }

  # Position of the stored criteria
  criteria <- lapply(x, function(model){
    match(weights, names(model$criteria))
  })

  # Check that all model objects contain the criteria
  stopifnot(isFALSE(anyNA(criteria)))

  # Coerce the positions to an integer
  criteria_pos <- as.numeric(criteria)

  # Return the vector of criteria positions
  return(criteria_pos)
}
