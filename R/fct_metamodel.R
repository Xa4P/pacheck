#' Fit linear metamodel
#'
#' @description This function fits and provides summary statistics of a linear regression model fitted on the input and output values of a probabilistic analysis.
#'
#' @param df a dataframe.
#' @param y_var character. Name of the output variable in the dataframe. This will be the dependent variable of the metamodel.
#' @param x_vars character or a vector for characters. Name of the input variable in the dataframe. This will be the independent variable of the metamodel.
#' @param standardise logical. Determine whether the parameter of the linear regression should be standardised. Default is FALSE.
#' @param partition numeric. Value between 0 and 1 to determine the proportion of the observations to use to fit the metamodel. Default is 1 (fitting the metamodel using all observations).
#' @param seed_num numeric. Determine which seed number to use to split the dataframe in fitting and validation sets.
#' @param validation logical or character. Determine whether to validate the RF model. Choices are "test_train_split" and "cross-validation". TRUE corresponds to "cross-validation", default is FALSE.
#' @param show_intercept logical. Determine whether to show the intercept of the perfect prediction line (x = 0, y = 0). Default is FALSE.
#' @param x_poly_2 character. character or a vector for characters. Name of the input variable in the dataframe. These variables will be exponentiated by factor 2.
#' @param x_poly_3 character. character or a vector for characters. Name of the input variable in the dataframe. These variables will be exponentiated by factor 3.
#' @param x_exp character. character or a vector for characters. Name of the input variable in the dataframe. The exponential of these variables will be included in the metamodel.
#' @param x_log character. character or a vector for characters. Name of the input variable in the dataframe. The logarithm of these variables will be included in the metamodel.
#' @param x_inter character. character or a vector for characters. Name of the input variables in the dataframe. This vector contains the variables for which the interaction should be considered. The interaction terms of two consecutive variables will be considered in the linear model; hence, the length of this vector should be even.
#' @param folds numeric. Number of folds for the cross-validation. Default is 5.
#'
#' @return A list containing the fit of the model and validation estimates and plots when selected.
#'
#' @details Standardisation of the parameters is obtained by \deqn{(x - u(x)) / sd(x)}
#' where \eqn{x} is the variable value, \eqn{u(x)} the mean over the variable and \eqn{sd(x)} the standard deviation of \eqn{x}.
#' For more details, see \href{https://doi.org/10.1177/0272989X13492014}{Jalal et al. 2013}.
#'
#' @examples
#' # Fitting meta model with two variables using the probabilistic data
#' data(df_pa)
#' fit_lm_metamodel(df = df_pa,
#'                  y_var = "inc_qaly",
#'                  x_vars = c("p_pfsd", "p_pdd")
#'                  )
#'
#' @import ggplot2
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
  if(partition == 1 && validation == TRUE || partition == 1 && validation == "train_test_split") {
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
  if(!(validation %in% c(TRUE,FALSE,"cross_validation","train_test_split"))) {
    stop("Validation must be one of: TRUE, FALSE, 'cross_validation','train_test_split'.")
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
  if(validation == TRUE || validation == "cross_validation"){
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

      lm_fit <- lm(form, data = df_train)

      ## Fit in validation set
      v_y_predict          <- as.numeric(as.character(unlist(predict(lm_fit, newdata = df_test))))
      v_y_valid            <- as.numeric(as.character(df_test[, paste(y_var)]))
      r_squared_validation[i] <- cor(v_y_predict, v_y_valid) ^ 2
      mae_validation[i]       <- mean(abs(v_y_predict - v_y_valid))
      mre_validation[i]       <- mean(abs(v_y_predict - v_y_valid) / abs(v_y_valid))
      mse_validation[i]     = mean((v_y_predict - v_y_valid)^2)
    }

    ## Output: validation
    stats_validation = data.frame(
      Statistic = c("R-squared", "Mean absolute error", "Mean relative error", "Mean squared error"),
      Value     = round(c(mean(r_squared_validation), mean(mae_validation), mean(mre_validation), mean(mse_validation)), 3)
    )
    names(stats_validation)[names(stats_validation) == "Value"] <- "Value (method: cross-validation)"

    l_out <- list(fit = lm_fit,
                  stats_validation = stats_validation
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
    mae_validation       <- mean(abs(v_y_predict - v_y_valid))
    mre_validation       <- mean(abs(v_y_predict - v_y_valid) / abs(v_y_valid))
    mse_validation       <- mean((v_y_predict - v_y_valid)^2)

    ## Calibration plot: predicted versus observed
    df_valid$y_pred <- v_y_predict
    p <- ggplot2::ggplot(ggplot2::aes_string(x = "y_pred", y = y_var), data = df_valid) +
      ggplot2::geom_point(shape = 1) +
      ggplot2::xlab("Predicted values") +
      ggplot2::ylab("Observed values") +
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
                  calibration_plot = p)
  }
  else {
    lm_fit <- lm(form, data = df)
    ## Output: no validation
    l_out <- list(fit = lm_fit)
  }

  # Export
  return(l_out)
}

#' Predict using a fitted metamodel
#'
#' @description This function computes a result using a pre-defined metamodel, and user-defined inputs to make the prediction.
#'
#' @param lm_metamodel a lm object. This object should use variables defined in `df`.
#' @param inputs a numeric value or vector of numeric values. These inputs value will be used for the prediction using the metamodel.
#'
#' @return A dataframe with the results of deterministic sensitivity analyses performed using parameter values of the linear metamodel. The dataframe contains the results using the lower and upper bound of the 95% Confidence Interval of the probabilistic parameters.
#'
#' @details The number of element of `inputs` should equal the number of predictors included in `lm_metamodel`.
#'
#' @examples
#' # Fitting meta modelwith two variables using the summary data
#' data(df_pa)
#' lm_res <- fit_lm_metamodel(df = df_pa,
#'                  y = "Inc_QALY",
#'                  x = c("p_pfsd", "p_pdd")
#'                  )
#'
#' # Predicting using this metamodel
#' predict_metamodel(lm_metamodel = lm_res,
#'                      inputs = c(0.75, 0.2)
#'                      )
#' @import stats
#' @export
predict_metamodel <- function(metamodel,
                              inputs){
  # Identify coefficient metamodel
  v_names <- metamodel$coefficients

  # Flag errors
  if(length(inputs) < length(v_names)) {
    stop("Number of inputs is lower than number of coefficients of the metamodel.")
  }

  if(length(inputs) > length(v_names)) {
    stop("Number of inputs is higher than number of coefficients of the metamodel.")
  }

  newdata <- data.frame(t(inputs))
  names(newdata) <- v_names

  pred <- stats::predict(metamodel$fit, newdata = newdata)
  names(pred) <- "prediction"

  df_out <- cbind(newdata, t(pred))
  return(df_out)
}

#' Perform DSA using linear metamodel
#'
#' @description This function performs deterministic sensitivity analyses (DSA) using the results of a linear metamodel.
#'
#' @param df a dataframe. This dataframe should contain both the dependent and independent variables of `lm_metamodel`.
#' @param lm_metamodel a lm object. This object should use variables defined in `df`.
#'
#' @details The details of the methods used in for these DSA are described in XXX.
#'
#' @return A dataframe with the results of deterministic sensitivity analyses performed using parameter values of the linear metamodel. The dataframe contains the results using the lower and upper bound of the 95% Confidence Interval of the probabilistic parameters.
#'
#' @examples
#' # Fitting meta model with two variables using the summary data
#' data(df_pa)
#' lm_res_2 <- fit_lm_metamodel(df = df_pa,
#'                  y = "Inc_QALY",
#'                  x = c("p_pfsd", "p_pdd")
#'                  )
#'
#' dsa_lm_metamodel(df = df_pa,
#'                  lm_metamodel = lm_res_2)
#'
#' @export
#'
dsa_lm_metamodel <- function(df,
                             lm_metamodel){

  df <- df[, names(lm_metamodel$model)[2:length(names(lm_metamodel$model))]]

  df_dsa <- data.frame(
    rbind(apply(df, 2, mean),
          apply(df, 2, function(x) quantile(x, 0.025)),
          apply(df, 2, function(x) quantile(x, 0.975))
    )
  )

  m_low <- matrix(NA,
                  ncol = 4,
                  nrow = ncol(df_dsa),
                  dimnames = list(names(df_dsa),
                                  c("Parameter", "Lower_Bound", "Lower_Bound_low", "Lower_Bound_upp")))
  m_upp <- matrix(NA,
                  ncol = 4,
                  nrow = ncol(df_dsa),
                  dimnames = list(names(df_dsa),
                                  c("Parameter", "Upper_Bound", "Upper_Bound_low", "Upper_Bound_upp")))


  for (i in 1:ncol(df_dsa)) {
    df_temp <- df_dsa[1,]
    df_temp[, i] <- df_dsa[2, i]
    v_res <- predict(lm_metamodel , df_temp,
                     interval = "confidence")

    m_low[i, ] <- c(names(df_dsa)[[i]], v_res)
  }

  for (i in 1:ncol(df_dsa)) {
    df_temp <- df_dsa[1,]
    df_temp[, i] <- df_dsa[3, i]
    v_res <- predict(lm_metamodel , df_temp,
                     interval = "confidence")

    m_upp[i, ] <- c(names(df_dsa)[[i]], v_res)
  }

  rownames(m_low) <- rownames(m_upp) <- NULL
  df_low <- as.data.frame(m_low)
  df_upp <- as.data.frame(m_upp)

  df_out <- merge(df_low, df_upp)
  df_out[, 2:ncol(df_out)] <- apply(df_out[, 2:ncol(df_out)], 2, function(x) as.numeric(as.character(x)))

  return(df_out)
}


#' Plot results of DSA in a Tornado diagram
#'
#' @description This function plots the results of the deterministic sensitivity analyses (DSA) in a Tornado diagram.
#'
#' @param df a dataframe. This dataframe should contain the results of the function \code{\link{dsa_lm_metamodel}}.
#' @param df_basecase a dataframe. This object should contain the original probabilistic analysis inputs and outputs, and the variable defined in `outcome`.
#' @param outcome character. Name of the output variable of the DSA.
#'
#' @details The code to draw the Tornado diagram was obtained from \url{https://stackoverflow.com/questions/55751978/tornado-both-sided-horizontal-bar-plot-in-r-with-chart-axes-crosses-at-a-given}{Stakoverflow}.
#' The `df` object should contain the following variables; "Parameters" (the parameters to include in the Tornado diagram), "Lower_Bound" (the model outcomes when using the lower bound of the parameter value), "Upper_Bound" (the model outcomes when using the upper bound of the parameter value).
#'
#' @return A ggplot graph.
#'
#' @examples
#' # Fitting meta model with two variables using the summary data
#' data(df_pa)
#' lm_res_2 <- fit_lm_metamodel(df = df_pa,
#'                  y = "Inc_QALY",
#'                  x = c("p_pfsd", "p_pdd")
#'                  )
#'
#' # Estimating DSA inputs
#' df_res_dsa <- dsa_lm_metamodel(df = df_pa,
#'                                lm_metamodel = lm_res_2)
#'
#' # Plotting Tornado diagram
#' plot_tornado(df = df_res_dsa,
#'              df_basecase = df_pa,
#'              outcome = "Inc_QALY")
#'
#' @import ggplot2
#' @import scales
#' @import magrittr
#' @import dplyr
#' @import tidyr
#' @export
plot_tornado <- function(df,
                         df_basecase,
                         outcome) {
  ##SOURCE tornado diagram: https://stackoverflow.com/questions/55751978/tornado-both-sided-horizontal-bar-plot-in-r-with-chart-axes-crosses-at-a-given

  df$UL_Difference <- df$Upper_Bound - df$Lower_Bound

  df <- df[, c("Parameter", "Lower_Bound", "Upper_Bound", "UL_Difference")]

  df <- df[order(df$UL_Difference, decreasing = TRUE),] #order
  df <- head(df, 15) # select 15 most influential parameters

  # original value of output
  base.value <- mean(df_basecase[, outcome])

  # get order of parameters according to size of intervals
  # (I use this to define the ordering of the factors
  # which I then use to define the positions in the plot)
  order.parameters <- df %>% arrange(abs(UL_Difference)) %>%
    mutate(Parameter=factor(x=Parameter, levels=Parameter)) %>%
    select(Parameter) %>% unlist() %>% levels()

  # width of columns in plot (value between 0 and 1)
  width <- 0.95

  # get data frame in shape for ggplot and geom_rect
  df.2 <- df %>%
    # gather columns Lower_Bound and Upper_Bound into a single column using gather
    gather(key='type', value='output.value', Lower_Bound:Upper_Bound) %>%
    # just reordering columns
    select(Parameter, type, output.value, UL_Difference) %>%
    # create the columns for geom_rect
    mutate(Parameter=factor(Parameter, levels=order.parameters),
           ymin=pmin(output.value, base.value),
           ymax=pmax(output.value, base.value),
           xmin=as.numeric(Parameter)-width/2,
           xmax=as.numeric(Parameter)+width/2)

  df.2 <- df.2[order(abs(df.2$UL_Difference)),]

  p_out <- ggplot() +
    geom_rect(data = df.2,
              aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
    theme_bw() +
    labs(y = outcome) +
    #scale_y_continuous(labels = dollar_format(prefix = "\u20ac ", suffix = "")) +
    theme(axis.title.y=element_text(colour = "black"), legend.position = 'bottom',
          legend.title = element_blank(),
          axis.title.x = element_text(size=8)) +
    geom_hline(yintercept = base.value) +
    scale_x_continuous(breaks = c(1:length(order.parameters)),
                       labels = order.parameters) +
    coord_flip()

  p_out

}

#' Estimate decision sensitivy DSA using linear metamodel
#'
#' @description This function performs a logistic regression analysis and determines the decision sensitivity to parameter value using the logistic regression.
#'
#' @param df a dataframe. This dataframe should contain both dependent and independent variables.
#' @param y character. Name of the output variable in the dataframe. This will be the dependent variable of the logistic regression model.
#' @param x character or a vector for characters. Name of the input variable in the dataframe. This(these) will be the independent variable(s) of the logistic regression model.
#' @param y_binomial logical. Is `y` already a binomial outcome? Default is `FALSE.` If `TRUE`, the `y` variable will be used as such, otherwise, the `y` variable will be converted to a binomial variable using the `limit` argument.
#' @param limit numeric. Determines the limit when outcomes from `y` are categorised as 'success' (1) or not (0).
#'
#' @details The method for these analyses is described in [Merz et al. 1992](https://doi.org/10.1177%2F0272989X9201200304).
#'
#' @return A dataframe with the parameter values of the fitted logistic regression and the decision sensitivity associated with each parameter included in the logistic regression model.
#'
#' @examples
#' # Determining decision sensitivity using a non-binomial outcome
#' data(df_pa)
#' df_pa$inmb <- df_pa$inc_qaly * 100000 - df_pa$inc_costs
#' estimate_decision_sensitivity(df = df_pa,
#'                               y = "inmb",
#'                               x = c("p_pfsd", "p_pdd"),
#'                               y_binomial = FALSE
#'                               )
#'
#' @export
#'
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
