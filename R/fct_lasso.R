#' Fit LASSO metamodel
#'
#' @param df
#' @param y_var
#' @param x_vars
#' @param seed_num
#' @param pm_plot
#' @param pm_vars
#' @param fit_complete_model
#'
#' @import glmnet
#'
#' @return
#' @export
#'
#' @examples
fit_lasso_metamodel = function(df,
                               y_var = NULL,
                               x_vars = NULL,
                               seed_num = 1,
                               standardise = FALSE,
                               tune_plot = TRUE,
                               x_poly_2 = NULL,
                               x_poly_3 = NULL,
                               x_exp = NULL,
                               x_log = NULL,
                               x_inter = NULL
                               ){
  # Flag errors
  if(length(y_var) > 1) {
    stop("Multiple outcomes provided to 'y'.")
  }
  if(is.null(y_var)) {
    stop("Cannot perform random forest regression because there is no value provided for 'y_var'.")
  }
  if(!is.null(x_inter) && length(x_inter) != 2 * round(length(x_inter) / 2)) {
    stop("The number of interaction terms is uneven.")
  }
  if(is.null(x_vars) && is.null(x_poly_2) && is.null(x_poly_3) && is.null(x_exp) && is.null(x_log)) {
    stop("Cannot perform linear regression because there is no value provided for the predictors.")
  }
  if(!(tune_plot %in% c(TRUE,FALSE))){
    stop("Please choose TRUE or FALSE for tune_plot.")
  }

  # Remove any possible NA's
  df = na.omit(df)

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

  # Set up
  set.seed(seed_num)
  l_out = list(fit = NULL,
               lasso_coefs = NULL,
               model_info = list(x_vars = x_vars,
                                 y_var = y_var,
                                 form = form,
                                 data = df,
                                 type = "lasso")
  )

  # Fit model
  df_train = df
  x_train = model.matrix(form, df_train)
  y_train = df_train[,y_var]

  cv_out = glmnet::cv.glmnet(x_train,y_train)
  bestlam = cv_out$lambda.min

  lasso_fit = glmnet::glmnet(x_train,y_train,alpha=1,lambda=bestlam)

  if(tune_plot == TRUE){
    plot(cv_out)
  }
  l_out[1] = list(lasso_fit)
  l_out[2] = list(lasso_fit$beta)

  # Export
  return(l_out)


}
