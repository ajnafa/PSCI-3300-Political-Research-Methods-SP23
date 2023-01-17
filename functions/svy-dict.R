#' Building Searchable Codebooks from Variable/Value Labels
#'
#' This function can be used to build a searchable codebook tibble
#' based on variable and value labels in a data frame.
#'
#' @aliases svy_dict
#'
#' @param x An object of class data frame or tibble
#'
#' @param ... Additional arguments to be passed to \code{tibble::tibble}
#'
#' @return The function returns a data frame containing the variable name,
#' variable labels, value labels, and unique values for each column of \code{x}
#'
#' @importFrom sjlabelled get_label get_values get_labels
#' @importFrom tibble tibble
#' @export svy_dict
#' @export
svy_dict <- function(x, ...) {
  dict <- tibble(
    var_names = colnames(x),
    var_labs = sjlabelled::get_label(x),
    var_vals = sjlabelled::get_values(x),
    val_labs = sjlabelled::get_labels(x, values = "n"),
    ...
  )
  return(dict)
}