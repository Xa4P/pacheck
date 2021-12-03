#' Fit linear metamodel
#'
#' @description This function fits and provides summary statistics of a linear regression model fitted on the input and output values of a probabilistic analysis.
#'
#' @param df a dataframe.
#' @param y character. Name of the output variable in the dataframe. This will be the dependent variable of the metamodel.
#' @param x character or a vector for characters. Name of the input variable in the dataframe. This will be the independent variable of the metamodel.
#'
#' @return A dataframe with summary data for selected inputs and outputs.
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
#'
fit_lm_metamodel <- function(df,
                             y,
                             x) {

  if(length(x) > 1) {

    v_x <- paste(x, collapse = " + ")
    form <- as.formula(paste(y, "~", v_x))
    lm_out <- lm(form, data = df)

  } else {

    form <- as.formula(paste(y, "~", x))
    lm_out <- lm(form, data = df)

  }

  return(lm_out)
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
#' @param df a dataframe. This dataframe should contain the results of the function \code{\link{fit_lm_metamodel}}.
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
#'
#' @export
#'
#'

plot_tornado <- function(df,
                         df_basecase,
                         outcome) {

  #Draw tornado diagram
  ##SOURCE tornado diagram: https://stackoverflow.com/questions/55751978/tornado-both-sided-horizontal-bar-plot-in-r-with-chart-axes-crosses-at-a-given

  require(ggplot2)
  require(scales)
  require(tidyr)

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
