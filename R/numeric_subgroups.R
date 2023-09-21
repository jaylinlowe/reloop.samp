
#testing something
#' Divide observations into groups based on a single numeric variable
#'
#' @param Y vector of responses
#' @param X a matrix or dataframe of covariates
#' @param grouping_col A column in X, used to divide observations into subgroups
#' @param preds vector of predictions
#' @param max_groups number of groups to create
#'
#' @return A vector of subgroup assignment
#' @export
#'
numeric_subgroups <- function(Y, X, grouping_col, preds, max_groups = 10) {

  if(!grouping_col %in% colnames(X)) {
    stop("grouping_col must be the name of a column in X")
  }

  if (length(preds) != length(Y)) {
    stop("Y and X must be the same length")
  }
  if (length(Y) != nrow(X)) {
    stop("Incorrect dimensions for Y or X")
  }

  df <- data.frame(cbind(Y, X))

  df_num <- tryCatch({
    df$subgroups <- cut_number(df[[grouping_col]], max_groups)
    df_mod <- df},
    error = function(e) {
      sub_mode <- my_mode(df, grouping_col)
      df$is_mode <- ifelse(df[[grouping_col]] == sub_mode, 1, 0)
      df_not_mode <- filter(df, is_mode == 0)

      df_mod <- tryCatch({
        df_not_mode$subgroups <- cut_number(df_not_mode[[grouping_col]], max_groups-1)
        df_mode <- filter(df, is_mode == 1)
        df_mode$subgroups <- rep(sub_mode, nrow(df_mode))
        df_mod <- rbind(df_mode, df_not_mode)},
        error = function(e) {
          df$subgroups <- ifelse(df[[grouping_col]] == sub_mode, as.character(sub_mode), "not mode")
          df_mod <- df
        })
    })

  #ADJUST CODE - THIS SHOULD JUST RETURN A COLUMN WITH THE NUMERIC SPLITS, AND THEN WE RUN USER SPECIFIED GROUP
  #results <- user_specified_group(df_num[,1], df_num[,-1], grouping_col = "subgroups", preds, effect_size, alpha, beta)
  return(results$subgroups)
}
