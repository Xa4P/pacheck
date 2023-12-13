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

  # Set up
  l_out <- list()
  set.seed(seed_num)

  # Set up
  l_out <- list()
  set.seed(seed_num)

  # Standardise inputs
  if(standardise == TRUE) {
    if(length(x_vars) > 1){
      df[, x_vars] <- lapply(df[, x_vars], function(i) (i - mean(i)) / sd(i))
    } else {
      df[, x_vars] <- (df[, x_vars] - mean(df[, x_vars])) / sd(df[, x_vars])
    }
  }

  # Transform inputs
  if(!is.null(x_poly_2)) {
    v_poly_2 <- paste("poly(", x_poly_2, ", 2)", collapse = " + ")
  } else {
    v_poly_2 <- NULL
  }
  if(!is.null(x_poly_3)) {
    v_poly_3 <- paste("poly(", x_poly_3, ", 3)", collapse = " + ")
    #x <- x[-which(x %in% v_poly_3)]
  } else {
    v_poly_3 <- NULL
  }
  if(!is.null(x_exp)) {
    v_exp <- paste("exp(", x_exp, ")", collapse = " + ")
    #x <- x[-which(x %in% v_exp)]
  } else {
    v_exp <- NULL
  }
  if(!is.null(x_log)) {
    v_log <- paste("log(", x_log, ")", collapse = " + ")
    #x <- x[-which(x %in% v_log)]
  } else {
    v_log <- NULL
  }
  if(!is.null(x_inter)) {
    pairs <- length(x_inter)/2
    pair_seq <- seq(1, pairs, 1)
    pair_seq <- pair_seq - 1
    v_inter <- vapply(pair_seq, function(x) {
      paste0(x_inter[2 * x + 1], ":", x_inter[2 * x + 2])
    }, character(1))
    v_inter <- c(v_inter, unique(x_inter))
  } else {
    v_inter <- NULL
  }

  v_x <- paste(unique(c(x_vars, v_poly_2, v_poly_3, v_exp, v_log, v_inter)), collapse = " + ")
  form <- as.formula(paste(y_var, "~", v_x))

}
