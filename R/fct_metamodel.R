#' Fit linear metamodel
#' @description This function fits and provides summary statistics of a linear regression model fitted on the input and output values of a probabilistic analysis.
#' @param df a dataframe.
#' @param y_var character. Name of the output variable in the dataframe. This will be the dependent variable of the metamodel.
#' @param x_vars character or a vector for characters. Name of the input variable in the dataframe. This will be the independent variable of the metamodel.
#' @param standardise logical. Determine whether the parameter of the linear regression should be standardised. Default is FALSE.
#' @param partition numeric. Value between 0 and 1 to determine the proportion of the observations to use to fit the metamodel. Default is 1 (fitting the metamodel using all observations).
#' @param seed_num numeric. Determine which seed number to use to split the dataframe in fitting and validation sets.
#' @param validation logical or character. Determine whether to validate the linear model. Choices are "test_train_split" and "cross_validation".
#' @param show_intercept logical. Determine whether to show the intercept of the perfect prediction line (x = 0, y = 0). Default is FALSE.
#' @param x_poly_2 character. character or a vector for characters. Name of the input variable in the dataframe. These variables will be exponentiated by factor 2.
#' @param x_poly_3 character. character or a vector for characters. Name of the input variable in the dataframe. These variables will be exponentiated by factor 3.
#' @param x_exp character. character or a vector for characters. Name of the input variable in the dataframe. The exponential of these variables will be included in the metamodel.
#' @param x_log character. character or a vector for characters. Name of the input variable in the dataframe. The logarithm of these variables will be included in the metamodel.
#' @param x_inter character. character or a vector for characters. Name of the input variables in the dataframe. This vector contains the variables for which the interaction should be considered. The interaction terms of two consecutive variables will be considered in the linear model; hence, the length of this vector should be even.
#' @param folds numeric. Number of folds for the cross-validation. Default is 5.
#' @return A list containing the fit of the model and validation estimates and plots when selected.
#' @details Standardisation of the parameters is obtained by \deqn{(x - u(x)) / sd(x)}
#' where \eqn{x} is the variable value, \eqn{u(x)} the mean over the variable and \eqn{sd(x)} the standard deviation of \eqn{x}.
#' For more details, see \href{https://doi.org/10.1177/0272989X13492014}{Jalal et al. 2013}.
#'
#' @examples
#' # Fitting linear meta model with two variables using the probabilistic data
#' data(df_pa)
#' fit_lm_metamodel(df = df_pa,
#'                  y_var = "inc_qaly",
#'                  x_vars = c("p_pfsd", "p_pdd")
#'                  )
#'
#' @import ggplot2
#' @importFrom stats as.formula
#' @importFrom stats lm
#' @export
fit_lm_metamodel <- function(df,
                             y_var = NULL,
                             x_vars = NULL,
                             standardise = FALSE,
                             partition = 1,
                             seed_num = 1,
                             validation = FALSE,
                             folds = 5,
                             show_intercept = FALSE,
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
  if(partition == 1 && validation == "train_test_split") {
    stop("Cannot perform validation because all observations are included in the training set. Lower `partition` below 1.")
  }
  if(is.null(y_var)) {
    stop("Cannot perform linear regression because there is no value provided for 'y_var'.")
  }
  if(!is.null(x_inter) && length(x_inter) != 2 * round(length(x_inter) / 2)) {
    stop("The number of interaction terms is uneven.")
  }
  if(is.null(x_vars) && is.null(x_poly_2) && is.null(x_poly_3) && is.null(x_exp) && is.null(x_log)) {
    stop("Cannot perform linear regression because there is no value provided for the predictors.")
  }
  if(!(validation %in% c(FALSE,"cross_validation","train_test_split"))) {
    stop("Validation must be one of: FALSE, 'cross_validation','train_test_split'.")
  }
  if(folds < 1 || folds > nrow(df_pa)){
    stop("Folds must be bigger than 0 and smaller than or equal to the number of rows of the dataframe.")
  }

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

  # Validation statistics and plots
  if(validation == "cross_validation"){
    df_validation = df[sample(nrow(df)),]
    folds_ind = cut(seq(1,nrow(df_validation)),breaks=folds,labels=FALSE)

    r_squared_validation = rep(NA,folds)
    mae_validation = rep(NA,folds)
    mre_validation = rep(NA,folds)
    mse_validation = rep(NA,folds)

    for (i in 1:folds){
      test_indices = which(folds_ind==i)
      df_test = df_validation[test_indices,]
      df_train = df_validation[-test_indices,]

      # Fit on training data
      lm_fit <- lm(form, data = df_train)

      ## Fit in validation set
      v_y_predict          <- as.numeric(as.character(unlist(predict(lm_fit, newdata = df_test))))
      v_y_valid            <- as.numeric(as.character(df_test[, paste(y_var)]))
      r_squared_validation[i] <- cor(v_y_predict, v_y_valid) ^ 2
      mae_validation[i]       <- mean(abs(v_y_valid - v_y_predict))
      mre_validation[i]       <- mean(abs(v_y_valid - v_y_predict) / v_y_valid)
      mse_validation[i]       <- mean((v_y_valid - v_y_predict)^2)
    }

    ## Output: validation
    stats_validation = data.frame(
      Statistic = c("R-squared", "Mean absolute error", "Mean relative error", "Mean squared error"),
      Value     = round(c(mean(r_squared_validation), mean(mae_validation), mean(mre_validation), mean(mse_validation)), 3)
    )
    names(stats_validation)[names(stats_validation) == "Value"] <- "Value (method: cross-validation)"

    l_out <- list(fit = lm_fit,
                  stats_validation = stats_validation,
                  model_info = list(x_vars = x_vars,
                                    y_var = y_var,
                                    form = form,
                                    data = df,
                                    type = "lm")
    )
  }
  else if(validation == "train_test_split") {
    ## Partition data and fit to train data
    selection <- sample(1:nrow(df), size = round(nrow(df) * partition), replace = FALSE)
    df_fit    <- df[selection, ]
    df_valid  <- df[-selection, ]
    lm_fit <- lm(form, data = df_fit)

    ## Fit in validation set
    v_y_predict          <- as.numeric(as.character(unlist(predict(lm_fit, newdata = df_valid))))
    v_y_valid            <- as.numeric(as.character(df_valid[, paste(y_var)]))
    r_squared_validation <- cor(v_y_predict, v_y_valid) ^ 2
    mae_validation       <- mean(abs(v_y_valid - v_y_predict))
    mre_validation       <- mean(abs(v_y_valid - v_y_predict) / v_y_valid)
    mse_validation       <- mean((v_y_valid - v_y_predict)^2)

    ## Calibration plot: predicted versus observed
    df_plot <- data.frame(cbind(df_valid[, y_var], y_pred = v_y_predict))
    names(df_plot)[1] <- "y_var"
    p <- ggplot2::ggplot(ggplot2::aes(x = y_pred, y = y_var), data = df_plot) +
      ggplot2::geom_point(shape = 1) +
      ggplot2::xlab("Predicted values") +
      ggplot2::ggtitle(paste("Calibration plot for", y_var)) +
      ggplot2::ylab("Observed values") +
      geom_abline(intercept = 0, slope = 1) +
      ggplot2::theme_bw()

    if(show_intercept == TRUE) {
      p <- p +
        ggplot2::geom_abline(intercept = 0, slope = 1, colour = "orange")
    }

    ## Output: validation
    stats_validation = data.frame(
      Statistic = c("R-squared", "Mean absolute error", "Mean relative error", "Mean squared error"),
      Value     = round(c(r_squared_validation, mae_validation, mre_validation, mse_validation), 3)
    )
    names(stats_validation)[names(stats_validation) == "Value"] <- "Value (method: train/test split)"

    l_out <- list(fit = lm_fit,
                  stats_validation = stats_validation,
                  calibration_plot = p,
                  model_info = list(x_vars = x_vars,
                                    y_var = y_var,
                                    form = form,
                                    data = df,
                                    type = "lm"))
  }
  else {
    lm_fit <- lm(form, data = df)
    ## Output: no validation
    l_out <- list(fit = lm_fit,
                  model_info = list(x_vars = x_vars,
                                    y_var = y_var,
                                    form = form,
                                    data = df,
                                    type = "lm"))
  }

  # Export
  return(l_out)
}

#' Estimate decision sensitivy DSA using linear metamodel
#' @description This function performs a logistic regression analysis and determines the decision sensitivity to parameter value using the logistic regression. (STILL IN DEVELOPMENT)
#' @param df a dataframe. This dataframe should contain both dependent and independent variables.
#' @param y character. Name of the output variable in the dataframe. This will be the dependent variable of the logistic regression model.
#' @param x character or a vector for characters. Name of the input variable in the dataframe. This(these) will be the independent variable(s) of the logistic regression model.
#' @param y_binomial logical. Is `y` already a binomial outcome? Default is `FALSE.` If `TRUE`, the `y` variable will be used as such, otherwise, the `y` variable will be converted to a binomial variable using the `limit` argument.
#' @param limit numeric. Determines the limit when outcomes from `y` are categorised as 'success' (1) or not (0).
#' @details The method for these analyses is described in [Merz et al. 1992](https://doi.org/10.1177%2F0272989X9201200304).
#' @return A dataframe with the parameter values of the fitted logistic regression and the decision sensitivity associated with each parameter included in the logistic regression model.
#' @examples
#' # Determining decision sensitivity using a non-binomial outcome
#' data(df_pa)
#' df_pa$inmb <- df_pa$inc_qaly * 100000 - df_pa$inc_costs
#' estimate_decision_sensitivity(df = df_pa,
#'                               y = "inmb",
#'                               x = c("p_pfsd", "p_pdd"),
#'                               y_binomial = FALSE
#'                               )
#' @importFrom stats coefficients
#' @importFrom stats as.formula
#' @importFrom stats glm
#' @export
estimate_decision_sensitivity <- function(df,
                                          y,
                                          x,
                                          y_binomial = FALSE,
                                          limit = 0
){

  outcome_var <- if(y_binomial == TRUE) {
    df[, y]
  } else {
    ifelse(df[, y] > 0, 1, 0)
  }

  df <- data.frame(cbind(
    df,
    outcome_var
  ))

  names(df)[ncol(df)] <- "indep_var"

  if(length(x) > 1) {

    v_x <- paste(x, collapse = " + ")
    form <- as.formula(paste("indep_var", "~", v_x))
    glm_out <- glm(form, data = df, family = "binomial")

  } else {

    form <- as.formula(paste("indep_var", "~", x))
    glm_out <- glm(form, data = df, family = "binomial")

  }

  v_95CI <- summary(glm_out)$coefficients[, 2] * 1.96
  v_mean <- coefficients(glm_out)

  v_95CI <- v_95CI[-1] # remove intercept
  v_mean <- v_mean[-1] # remove intercept


  Low_CI <- round(v_mean - v_95CI, 3)
  High_CI <- round(v_mean + v_95CI, 3)
  v_diff <- High_CI - Low_CI

  Importance <-  paste(round((v_diff / sum(abs(v_diff))) * 100, 1), "%")

  df_out <- cbind(round(summary(glm_out)$coefficients[-1, ], 3),
                  Low_CI,
                  High_CI,
                  Importance)

  return(df_out)

}
