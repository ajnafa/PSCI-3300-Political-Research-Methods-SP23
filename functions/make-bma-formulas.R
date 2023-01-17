#' A Function for building combinations of formulas for model averaging
#' 
#' @importFrom glue glue_collapse
#' @importFrom purrr map_chr
#' @importFrom stringr str_replace
#'
#' @param terms A named list of terms to create all unique combinations of.
#' See usage example for direction on how to specify each term
#' 
#' @param resp A string specifying the name of the response vector
#' 
#' @param base_terms An optional argument indicating additional terms to
#' include in all formulas. This can be used to place implicit constraints on 
#' the parameter space to reduce computational burden
#' 
#' @param ... Reserved for future development but currently
#' unused
#'
#' @return A character vector of model formulas containing all unique 
#' combinations of the elements in terms
#' 
#' @export bma_formulas
#' 
#' @examples
#' # Specify the input for the resp argument
#' response <- "fun"
#' 
#' # Specify the inputs for the terms argument
#' covariates <- list(
#'   age = c("", "age"),
#'   sex = c("", "sex"),
#'   rock = c("", "rock")
#'   )
#'   
#' # Build the formulas
#' model_forms <- bma_formulas(terms = covariates, resp = response)
#' 

bma_formulas <- function(terms, resp, base_terms = NULL, ...) {
  
  ## Validate the formula arguments
  stopifnot(exprs = {
    is.list(terms)
    is.character(resp)
  })
  
  ## Make a list of all unique combinations of lhs ~ rhs 
  formulas <- expand.grid(terms, stringsAsFactors = FALSE)
  
  ## Initialize a list to store the formulas in
  out <- list()
  
  ## Build a list of formulas for each pair formulas
  for (i in 1:nrow(formulas)) {
    out[[(i)]] <- glue_collapse(
      formulas[i, !is.na(formulas[i, ])], 
      sep = " + "
    )
  }
  
  # Paste the response term
  out <- paste(resp, "~", out)
  
  # If base_terms is non-null, add additional terms to all formulas
  if (!is.null(base_terms)) {
    out <- paste(out, base_terms, sep = " + ")
  }
  
  out <- map_chr(
    .x = out, 
    ~ str_replace(.x, "character\\(0\\)", "1")
  )
  
  return(out)
}
