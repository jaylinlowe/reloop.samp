

#' Calculating the mode for a specified column of a data frame
#'
#' @param df a data frame to analyze
#' @param col the name of a column in `df` to calculate the mode for
#'
#' @return The value that appears most frequently in the `var` column of `df`
#' @export
#' @noRd
mode_calc <- function(df, col) {
  index <- which(colnames(df) == col)
  v <- df[, index]
  uniqv <- unique(v)
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}

#' Get vector of variable names
#'
#' @description
#' An easy way to get the correct variable names depending on the Shiny inputs.
#' If "All Variables" is the input, all column names are returned. The function
#' also returns an error if `variables` is a list including variable names and
#' "All Variables".
#'
#'
#' @param variables user specified input from Shiny function of what variables to select
#' @param X data frame of covariates
#'
#' @return A vector of variable names
#' @export
#' @noRd
get_var_names <- function(variables, X) {
  if (length(variables) == 1) {
    if (variables == "All Variables") {
      variables <- colnames(X)
    }
  }

  if (length(variables) > 1) {
    if ("All Variables" %in% variables) {
      stop("If 'All Variables' is selected, no other columns should be selected")
    }
  }
  return(variables)
}
