#' Posterior Contrasts for Bayesian Models fit with {brms}
#' 
#' This function generates posterior contrasts and provides the functionality
#' to approximate integrating over random effects in hierarchical models to
#' obtain population-averaged estimates.
#' 
#' @importFrom brms is.brmsfit, posterior_predict, ndraws
#' @importFrom dplyr select, mutate, matches
#' @importFrom tidyr expand_grid
#' @importFrom future plan, tweak, multisession, sequential
#' @importFrom furrr future_map,
#' @importFrom data.table data.table, rbindlist
#'
#' @param models A named list of model objects of class `brmsfit` for which to 
#' generate predictions.
#' 
#' @param X A string indicating the name of the treatment in the models.
#' 
#' @param G An optional string indicating the name of the grouping variable 
#' for the random effects structure in the model (e.g. "id", "country", etc.).
#' If `G` is not `NULL` (the default), `newlevels` must also be specified.
#' 
#' @param newlevels A vector of the same type as the grouping variable 
#' specified in `G`. The data frame passed to `data` is duplicated for each 
#' unique value in `newlevels`. If `newlevels` is not `NULL`, the arguments
#' `sample_new_levels = "gaussian"` and `allow_new_levels = TRUE` must also
#' be passed down to `brms::posterior_predict`.
#' 
#' @param data Data used to fit the model objects.
#' 
#' @param contrasts Values of `X` at which to generate predictions. Defaults to
#' `c(0, 1)` but could also be set to factor levels or any other set of 
#' contrasts.
#' 
#' @param cores Number of cores to use for parallel computation. If cores > 1,
#' future::multisession is used to speed computation via `{furrr}`. Requires
#' the `{furrr}` package be installed.
#' 
#' @param ... Additional arguments passed down to `brms::posterior_predict` 
#' such as `sample_new_levels` and `allow_new_levels`.
#'
#' @return A list of posterior contrasts for each model in `models`
#' 
#' @export posterior_contrasts
#'
posterior_contrasts <- function(models, 
                                X, 
                                G = NULL,
                                newlevels = NULL,
                                data,
                                contrasts = c(0, 1),
                                cores = 1,
                                ...) {
  
  # Check that all elements of models are of class brmsfit
  for (i in seq_along(models)) {
    brms_check <- is.brmsfit(models[[i]])
    stopifnot(
      "All elements in models must be of class brmsfit" = brms_check, 
      brms_check
    )
  }
  
  # Generate new groups to integrate over the random effects
  if (!is.null(G) & !is.null(newlevels)) {
    
    # Check that G is a string
    stopifnot(
      "G must be a string corresponding to the name of the 
        grouping variable in the data" = is.character(G),
      is.character(G)
    )
    
    # Y_i(X = 0, Z)
    lo <- data |> 
      mutate("{X}" := contrasts[1]) |>
      select(-matches(G)) |>
      expand_grid("{G}" := newlevels)
    
    # Y_i(X = 1, Z)
    hi <- df_lo |> 
      mutate("{X}" := contrasts[2])
    
  } else {
    
    # Y_i(X = 0, Z)
    lo <- data |> 
      mutate("{X}" := contrasts[1])
    
    # Y_i(X = 1, Z)
    hi <- data |> 
      mutate("{X}" := contrasts[2])
    
  }
  
  # Check if cores > 1
  if (cores > 1) {
    
    # Check if {furrr} is installed
    check_furrr <- any(grepl("furrr", installed.packages()[,1]))
    
    # If cores > 1 and {furrr} is not installed, throw an error
    stopifnot(
      "The {furrr} package is required for parallel computation." = check_furrr,
      check_furrr
    )
    
    # Fit models in parallel via future
    plan(tweak(multisession, workers = cores))
    
    # Posterior predictions for each data set
    out <- future_map(
      .x = models,
      ~ .get_posterior_predictions(hi, lo, model = .x, ...),
      .options = furrr_options(
        scheduling = 1,
        seed = TRUE,
        prefix = "prefix"
      ),
      .progress = TRUE
    )
    
    # Close the future session
    plan(sequential)
    
  } else {
    
    # Posterior predictions for each data set
    out <- lapply(models, function(x) {
      .get_posterior_predictions(hi, lo, model = x, ...)
    })
    
  }
  
  # Bind the draws into a single data table
  out <- rbindlist(out, idcol = "model")
  
  # Return the list of predictions
  return(out)
}

# Helper function to get posterior prediction contrasts
.get_posterior_predictions <- function(hi, lo, model, ...) {
  
  # Predictions for Y_i(X = 1, Z)
  EYX1 <- posterior_predict(model, newdata = hi, ...)
  
  # Predictions for Y_i(X = 0, Z)
  EYX0 <- posterior_predict(model, newdata = lo, ...)
  
  # Average over the observations for each draw in the prediction matrix
  out <- data.table(
    EYX1 = rowMeans(EYX1),
    EYX0 = rowMeans(EYX0),
    AME = rowMeans(EYX1 - EYX0),
    .draw = 1:ndraws(model)
  )
  
  # Return just the average contrast
  return(out)
}
