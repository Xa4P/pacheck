#' Fit LASSO metamodel
#'
#' @param df a dataframe.
#' @param y_var character. Name of the output variable in the dataframe. This will be the dependent variable of the metamodel.
#' @param x_vars character or a vector for characters. Name of the input variable(s) in the dataframe. This will be the independent variable of the metamodel.
#' @param seed_num numeric. Determine which seed number to use to split the dataframe in fitting an validation sets.
#' @param standardise logical. Determine whether the parameter of the linear regression should be standardised. Default is FALSE.
#' @param tune_plot logical. Determine whether the plot of the results of tuning the lambda should be shown.
#' @param x_poly_2 character. character or a vector for characters. Name of the input variable in the dataframe. These variables will be exponentiated by factor 2.
#' @param x_poly_3 character. character or a vector for characters. Name of the input variable in the dataframe. These variables will be exponentiated by factor 3.
#' @param x_exp character. character or a vector for characters. Name of the input variable in the dataframe. The exponential of these variables will be included in the metamodel.
#' @param x_log character. character or a vector for characters. Name of the input variable in the dataframe. The logarithm of these variables will be included in the metamodel.
#' @param x_inter character. character or a vector for characters. Name of the input variables in the dataframe. This vector contains the variables for which the interaction should be considered. The interaction terms of two consecutive variables will be considered in the linear model; hence, the length of this vector should be even.
#'
#' @import glmnet
#'
#' @return a list
#' @export
#'
#' @examples
#' #Fit lasso metamodel with two variables using the probabilistic data
#' data(df_pa)
#' fit_lasso_metamodel(df = df_pa,
#'                  y_var = "inc_qaly",
#'                  x_vars = c("p_pfsd", "p_pdd"),
#'                  tune_plot = TRUE
#'                  )
#'
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
