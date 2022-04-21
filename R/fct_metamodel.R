#' Fit linear metamodel
#'
#' @description This function fits and provides summary statistics of a linear regression model fitted on the input and output values of a probabilistic analysis.
#'
#' @param df a dataframe.
#' @param y character. Name of the output variable in the dataframe. This will be the dependent variable of the metamodel.
#' @param x character or a vector for characters. Name of the input variable in the dataframe. This will be the independent variable of the metamodel.
#' @param standardise logical. Determine whether the parameter of the linear regression should be standardised. Default is FALSE.
#' @param partition numeric. Value between 0 and 1 to determine the proportion of the observations to use to fit the metamodel. Default is 1 (fitting the metamodel using all observations).
#' @param seed_num numeric. Determine which seed number to use to split the dataframe in fitting an validation sets.
#' @param validation logical. Determine whether R2 should be calculated on the validation set.
#'
#' @return A dataframe with summary data for selected inputs and outputs.
#'
#' @details Standardisation of the parameters is obtained by \deqn{(x - u(x)) / sd(x)}
#' where \eqn{x} is the variable value, \eqn{u(x)} the mean over the variable and \eqn{sd(x)} the standard deviation of \eqn{x}.
#' For more details, see \href{https://doi.org/10.1177/0272989X13492014}{Jalal et al. 2013}.
#'
#' @examples
#' #' # Fitting meta model with a single variable using the summary data
#' data(df_pa)
#' fit_lm_metamodel(df = df_pa,
#'                  y = "Inc_QALY",
#'                  x = "p_pfsd")
#'                  )
#'
#' # Fitting meta model with two variables using the summary data
#' data(df_pa)
#' fit_lm_metamodel(df = df_pa,
#'                  y = "Inc_QALY",
#'                  x = c("p_pfsd", "p_pdd")
#'                  )
#'
#' @export
#'
fit_lm_metamodel <- function(df,
                             y,
                             x,
                             standardise = FALSE,
                             partition = 1,
                             seed_num = 1,
                             validation = FALSE) {

  if(partition < 0 | partition > 1) {
    stop("Proportion selected for fitting the metamodel should be between 0 (excluded) and 1 (included)")
  }

  set.seed(seed_num)

  if(standardise == TRUE) {
    if(length(x) > 1){
      df[, x] <- lapply(df[, x], function(i) (i - mean(i)) / sd(i))
    } else {
      df[, x] <- (df[, x] - mean(df[, x])) / sd(df[, x])
    }
  }

  if(partition < 1) {
    selection <- sample(1:nrow(df), size = round(nrow(df) * partition), replace = FALSE)
    df_fit <- df[selection, ]
    validation <- TRUE
  } else {
    df_fit <- df
  }

  if(length(x) > 1) {

    v_x <- paste(x, collapse = " + ")
    form <- as.formula(paste(y, "~", v_x))
    lm_out <- lm(form, data = df_fit)

  } else {

    form <- as.formula(paste(y, "~", x))
    lm_out <- lm(form, data = df_fit)

  }

  if(validation == TRUE) {
    df_valid  <- df[-selection, ]
    v_y_valid <- predict.lm(lm_out, newdata = df_valid)
    r_squared_valid <- cor(v_y_valid, df_valid[, y]) ^ 2
    lm_out <- list(lm_out,
                   "R^2" = r_squared_valid)
  }

  return(lm_out)
}

#' Predict using linear metamodel
#'
#' @description This function computes a result using a pre-defined linear metamodel, and user-defined inputs to make the prediction.
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
#' predict_lm_metamodel(lm_metamodel = lm_res,
#'                      inputs = c(0.75, 0.2)
#'                      )
#' @import stats
#' @export
predict_lm_metamodel <- function(lm_metamodel,
                                 inputs){

  v_names <- names(lm_metamodel$coefficients[c(2:length(lm_metamodel$coefficients))])

  if(length(inputs) < length(v_names)) {
    stop("Number of inputs is lower than number of coefficients of the metamodel.")
  }

  if(length(inputs) > length(v_names)) {
    stop("Number of inputs is higher than number of coefficients of the metamodel.")
  }

  newdata <- data.frame(t(inputs))
  names(newdata) <- v_names

  pred <- stats::predict.lm(lm_metamodel, newdata = newdata)
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

  # Draw tornado diagram
  ##SOURCE tornado diagram: https://stackoverflow.com/questions/55751978/tornado-both-sided-horizontal-bar-plot-in-r-with-chart-axes-crosses-at-a-given

  #require(ggplot2)
  #require(scales)
  #require(tidyverse) # is this twice the same with "ggplot2"?

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
    labs(y = "Incremental Net Monetary Benefit") +
    scale_y_continuous(labels = dollar_format(prefix = "\u20ac ", suffix = "")) +
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
