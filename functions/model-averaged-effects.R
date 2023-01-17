#' Estimating Model Averaged Marginal Effects and Contrasts for Bayesian Models
#' 
#' This function facilitates the estimation of model averaged and stacked 
#' marginal effects for Bayesian models and is designed to account for uncertainty 
#' in the process of model specification and selection when estimating average 
#' marginal effects and probability contrasts.
#'
#' @aliases model_averaged_ame
#' 
#' @import data.table
#'
#' @param x A datatable of posterior draws for the average marginal effects
#' or predictions such as that returned by the `posterior_contrasts` function.
#'
#' @param weights An \eqn{n \times m} matrix of posterior probability weights 
#' such as that returned by `bridgesampling::post_prob` or an m length vector
#' of stacking or pseudo-BMA weights.
#' 
#' @param summary A logical argument indicating whether to return the full 
#' draws for each row in `weights` or return the average for each model-draw 
#' pair. Defaults to `FALSE`
#
#' @param ... Additional arguments for future development, currently unused.
#'
#' @return A datatable containing a weighted average of the posterior draws 
#' for the model averaged marginal effects.
#' 
#' @export model_averaged_ame
#' 
model_averaged_ame <- function(x, weights, ndraws, summary = TRUE, ...) {
  
  # Nest the draws data frame
  draws <- x[, list(x.yz = list(.SD)), by=model]
  
  # If weights are a matrix, calculate the draws by row
  if (is.matrix(weights)) {
    
    # Construct a matrix of draw weights
    weighted_draws <- apply(
      weights, 
      MARGIN = 1, 
      FUN = function(x) {
        round_largest_remainder(x * ndraws)
      })
    
    # Initialize a list to store the draws in
    out <- list()
    
    # Loop over each column in the matrix
    for (i in 1:ncol(weighted_draws)) {
      
      # Randomly sample n rows for each model based on the weights
      draw_ids <- lapply(weighted_draws[, i], function(x){
        sample(sum(weighted_draws[,i]), size = x)
      })
      
      # Randomly sample n draws proportional to the weights
      out[[i]] <- lapply(seq_along(draws[, x.yz]), function(id) {
        draws[, x.yz[[id]][draw_ids[[id]]]]
      })
      
      # Bind the draws into a single data table per repetition
      out[[i]] <- rbindlist(out[[i]], idcol = "model")
    }
    
    # Combine everything into a single data table
    out <- rbindlist(out, idcol = "bridge_rep")
    
    # Average over the bridge sampling repetitions by draw
    if (isTRUE(summary)) {
      out = out[
        , keyby = .(.draw, model),
        lapply(.SD, mean),
        .SDcols = patterns("EY|AME")
      ]
    }
    
  } else if (is.vector(weights)) {
    # Construct a vector of draw weights
    weighted_draws <- round_largest_remainder(weights * ndraws)
    
    # Randomly sample n rows for each model based on the weights
    draw_ids <- lapply(weighted_draws, function(x){
      sample(sum(weighted_draws), size = x)
    })
    
    # Randomly sample n draws proportional to the weights
    out <- lapply(seq_along(draws[, x.yz]), function(id) {
      draws[, x.yz[[id]][draw_ids[[id]]]]
    })
    
    # Combine everything into a single data table
    out <- rbindlist(out, idcol = "model")
  }
  
  # Return the model averaged draws
  return(out)
  
}


# Function from the brms package to ensure weighted draws sum to n
round_largest_remainder <- function(x) {
  x <- as.numeric(x)
  total <- round(sum(x))
  out <- floor(x)
  diff <- x - out
  J <- order(diff, decreasing = TRUE)
  I <- seq_len(total - floor(sum(out)))
  out[J[I]] <- out[J[I]] + 1
  return(out)
}