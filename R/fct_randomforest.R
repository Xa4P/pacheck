#' Fit random forest metamodel
#'
#' @param df
#' @param y_var
#' @param x_vars
#' @param standardise
#' @param x_poly_2
#' @param x_poly_3
#' @param x_exp
#' @param x_log
#' @param x_inter
#'
#' @return
#' @export
#'
#' @examples
fit_rf_metamodel <- function(df,
                             y_var = NULL,
                             x_vars = NULL,
                             standardise = FALSE,
                             x_poly_2 = NULL,
                             x_poly_3 = NULL,
                             x_exp = NULL,
                             x_log = NULL,
                             x_inter = NULL) {
  # Flag errors
  if(length(y_var) > 1) {
    stop("Multiple outcomes provided to 'y'.")
  }
  if(partition < 0 || partition > 1) {
    stop("Proportion selected for training the metamodel should be between 0 (excluded) and 1 (included).")
  }
  if(partition == 1 && validation == TRUE) {
    stop("Cannot perform validation because all observations are included in the training set. Lower `partition` below 1.")
  }
  if(is.null(y_var)) {
    stop("Cannot perform linear regression because there is no value provided for 'y_var'.")
  }
  if(!is.null(x_inter) && length(x_inter) != 2 * round(length(x_inter) / 2)) {
    stop("The number of interaction terms is oneven.")
  }
  if(is.null(x_vars) && is.null(x_poly_2) && is.null(x_poly_3) && is.null(x_exp) && is.null(x_log)) {
    stop("Cannot perform linear regression because there is no value provided for the predictors.")
  }

}
