#' Generate summary statistics
#' @description This function generates summary statistics of input and output values of a probabilistic analysis.
#' @param df a dataframe. This dataframe contains the probabilistic inputs and outputs of the health economic model.
#' @param vars a vector of strings. Contains the name of the variables to include in the summary statistics table. Default is NULL meaning all variables will be included.#' @return A dataframe with summary data for the selected variables. The returned summary statistics are:
#' \itemize{
#'   \item Mean
#'   \item Standard deviation
#'   \item 2.5th percentile
#'   \item 97.5th percentile
#'   \item Minimum
#'   \item Maximum
#'   \item Median
#'   \item Skewness
#'   \item Kurtosis
#' }
#' @examples
#' # Generating summary data of all inputs using the example dataframe
#' data(df_pa)
#' df_summary <- generate_sum_stats(df_pa)
#' @import assertthat
#' @import moments
#' @export
generate_sum_stats <- function(df,
                               vars = NULL
                               ){
  # Checks
  if(!is.null(vars)){
    assertthat::assert_that(all(vars %in% names(df_pa)),
                            msg = "At least one variable of 'vars' is not included in the dataframe")
  }

  # Initiation dataframes
  df_out <- df_select <- data.frame()
  if(!is.null(vars)){
    df_select <- data.frame(df[, vars])
  } else {
    df_select <- data.frame(df)
  }

  # Calculate summary statistics
  df_out <- data.frame(Parameter = if(length(vars) == 1) { vars } else{ names(df_select) },
                       Mean = apply(df_select, 2, mean),
                       SD = apply(df_select, 2, sd),
                       Percentile_2.5th = apply(df_select, 2, function(x) quantile(x, 0.025)),
                       Percentile_97.5th = apply(df_select, 2, function(x) quantile(x, 0.975)),
                       Minimum = apply(df_select, 2, min),
                       Maximum = apply(df_select, 2, max),
                       Median  = apply(df_select, 2, median),
                       Skewness = apply(df_select, 2, moments::skewness),
                       Kurtosis = apply(df_select, 2, moments::kurtosis)
                       )
  df_out[, 2:ncol(df_out)] <- apply(df_out[, 2:ncol(df_out)], 2, function(x) round(x, 3))
  rownames(df_out) <- NULL

  # Export
  return(df_out)
}

#' Generate correlation matrix
#' @description This function generates the correlation matrix of input and output values of a probabilistic analysis.
#' @param df a dataframe. This dataframe contains the probabilistic inputs and outputs of the health economic model.
#' @param vars a vector of strings. Contains the name of the variables to include in the correlation matrix. Default is NULL meaning all variables will be included.
#' @return A table with summary data for selected inputs and outputs.
#' @examples
#' # Generating summary data of all inputs using the example dataframe
#' data(df_pa)
#' generate_cor(df_pa)
#' @import assertthat
#' @export
generate_cor <- function(df,
                         vars = NULL){
  # Checks
  if(!is.null(vars)) {
    assertthat::assert_that(length(vars) > 1,
                            msg = "There should be two or more variables mentioned in 'vars'"
                            )
  }

  # Select variables
  df <- if(is.null(vars)) {
    df
  } else {
    data.frame(df[, vars])
  }

  # Correlation
  df_out <- cor(df)

  # Export
  return(df_out)
}

#' Visualise the distribution of a single parameter
#' @description This function plots the distribution of a single parameter.
#' @param df a dataframe.
#' @param param character. Name of variable of the dataframe for which the distribution should be plotted.
#' @param binwidth numeric. Determine the width of the bins to use, only applied in combination with "histogram". Default is 30 bins.
#' @param type character. Determine which plot to return: "histogram" for a histogram, "density" for a density plot. Default is "histogram".
#' @param dist character or vector of character. Determine which distribution to fit on the density plot.
#' @param user_dist character string. User-defined distribution to fit. Default value is NULL.
#' @param user_param_1 character string. First parameter of the user-defined distribution to fit.
#' @param user_param_2 character string. Second parameter of the user-defined distribution to fit.
#' @param user_mean numeric value. mean value to plot on the graph. Default is NULL
#' @details The available distributions are: "norm" (normal), "beta", "gamma", "lnorm" (lognormal). TO CHECK --> ask for mean and SD/SE for the user-defined distribution???
#' @return A ggplot2 graph.
#' @examples
#' # Generating histogram for the costs of progression-free health state, bins of 50 euros
#' data(df_pa)
#' vis_1_param(df = df_pa, param = "c_pfs", binwidth = 50)
#' @import assertthat
#' @import fitdistrplus
#' @import ggplot2
#' @export
vis_1_param <- function(df,
                        param = NULL,
                        binwidth = NULL,
                        type = "histogram",
                        dist = c("lnorm", "norm", "beta", "gamma"),
                        user_dist = NULL,
                        user_param_1 = NULL,
                        user_param_2 = NULL,
                        user_mean = NULL) {
  # Checks
  assertthat::assert_that(length(param) == 1,
                          msg = "Multiple variables provided for 'param' argument. Please provide only one variable.")
  if(!is.null(binwidth)){
    assertthat::assert_that(length(binwidth) == 1,
                            msg = "Multiple values provided for 'binwidth' argument. Please provide only one value.")
    assertthat::assert_that(is.numeric(binwidth),
                            msg = "'binwidth' is not a numeric value. Please provide a numeric value.")
  }
  assertthat::assert_that(type %in% c("histogram", "density"),
                          msg = "'type' argument is invalid. 'type' should be 'histogram' or 'density'.")
  assertthat::assert_that(all(dist %in% c("lnorm", "norm", "beta", "gamma")),
                          msg = "'dist' argument is invalid. 'dist' should be 'lnorm', 'norm', 'beta', or 'gamma'.")
  if(!is.null(user_dist)){
    assertthat::assert_that(length(user_dist) == 1,
                            msg = "Multiple values provided for 'user_dist' argument. Please provide only one value.")
    assertthat::assert_that(user_dist %in% c("lnorm", "norm", "beta", "gamma"),
                            msg = "'user_dist' argument is invalid. 'user_dist' should be 'lnorm', 'norm', 'beta', or 'gamma'.")
  }
  if(!is.null(user_param_1)){
    assertthat::assert_that(is.numeric(user_param_1),
                            msg = "'user_param_1' is not a numeric value. Please provide a numeric value.")
  }
  if (!is.null(user_param_2)) {
    assertthat::assert_that(is.numeric(user_param_2),
                            msg = "'user_param_2' is not a numeric value. Please provide a numeric value.")
  }
  if(!is.null(user_mean)) {
    assertthat::assert_that(is.numeric(user_mean),
                            msg = "'user_mean' is not a numeric value. Please provide a numeric value.")
  }

  # Fit distributions - only in case the density is plotted, because distributions not plotted on histogram
  if (type == "density") {
    if ("beta" %in% dist)  {
      if (any(df[, param] > 1) |
          any(df[, param] < 0)) {
        warning(
          "Beta distribution not fitted to the parameter values because the variable contains values below 0 and/or above 1.",
          immediate. = TRUE
        )
      } else {
        beta_dist  <- fitdistrplus::fitdist(df[, param], distr = "beta")
      }
    }
    if ("gamma" %in% dist) {
      if (any(df[, param] <= 0)) {
        warning(
          "Gamma distribution not fitted to the parameter values because the variable contains values below or equal to 0.",
          immediate. = TRUE
        )
      } else {
        gamma_dist <- fitdistrplus::fitdist(df[, param], distr = "gamma")
      }
    }
    if ("norm" %in% dist)  {
      norm_dist  <- fitdistrplus::fitdist(df[, param], distr = "norm")
    }
    if ("lnorm" %in% dist) {
      if (any(df[, param] <= 0)) {
        warning(
          "Log-normal distribution not fitted to the parameter values because the variable contains values below or equal to 0.",
          immediate. = TRUE
        )
      } else {
        lnorm_dist <- fitdistrplus::fitdist(df[, param], distr = "lnorm")
      }
    }
  }
  # Defining legend
  df_legend <- data.frame(
    dist_call = c("user", "norm", "beta", "gamma", "lnorm"),
    col = c("black", "orange", "red", "blue", "green")
  )
  if(!is.null(user_dist)) {
    dist <- c("user", dist)
  }
  df_legend <- df_legend[which(df_legend$dist_call %in% dist),]
  df_legend <- df_legend[order(df_legend$dist_call),]

  # Plot
  p <- ggplot2::ggplot(data = df, ggplot2::aes_string(x = param)) +
    ggplot2::theme_bw()
  if(type == "histogram") {
    p_out <- p + ggplot2::geom_histogram(binwidth = binwidth)
  } else if(type == "density") {
    p_out <- p +
      ggplot2::geom_histogram(ggplot2::aes(y = ..density..),
                              colour = "grey",
                              fill = "lightgrey",
                              binwidth = binwidth)
    if("beta" %in% dist &
       all(df[, param] <= 1) &
       all(df[, param] >= 0)) {
      df_beta <- data.frame(
        x = seq(from = min(df[, param]), to = max(df[, param]), by = 0.001),
        y = dbeta(seq(from = min(df[, param]), to = max(df[, param]), by = 0.001), beta_dist$estimate[[1]], beta_dist$estimate[[2]]))

      p_out <- p_out + ggplot2::geom_line(data = df_beta,
                                          ggplot2::aes(x = x,
                                                       y = y,
                                                       colour = "Beta")
      )
    }
    if("gamma" %in% dist &
       all(df[, param] > 0)) {
      df_gamma <- data.frame(
        x = seq(from = min(df[, param]), to = max(df[, param]), by = 0.001),
        y = dgamma(seq(from = min(df[, param]), to = max(df[, param]), by = 0.001), gamma_dist$estimate[[1]], gamma_dist$estimate[[2]]))

      p_out <- p_out + ggplot2::geom_line(data = df_gamma,
                                          ggplot2::aes(x = x,
                                                       y = y,
                                                       colour = "Gamma")
      )
    }
    if("norm" %in% dist) {
      df_norm <- data.frame(
        x = seq(from = min(df[, param]), to = max(df[, param]), by = 0.001),
        y = dnorm(seq(from = min(df[, param]), to = max(df[, param]), by = 0.001), norm_dist$estimate[[1]], norm_dist$estimate[[2]]))

      p_out <- p_out + ggplot2::geom_line(data = df_norm,
                                          ggplot2::aes(x = x,
                                          y = y,
                                          colour = "Normal")
                                          )
    }
    if("lnorm" %in% dist &
       all(df[, param] > 0)) {
      df_lnorm <- data.frame(
        x = seq(from = min(df[, param]), to = max(df[, param]), by = 0.001),
        y = dlnorm(seq(from = min(df[, param]), to = max(df[, param]), by = 0.001), lnorm_dist$estimate[[1]], lnorm_dist$estimate[[2]]))

      p_out <- p_out + ggplot2::geom_line(data = df_lnorm,
                                          ggplot2::aes(x = x,
                                                       y = y,
                                                       colour = "Lognormal")
                                          )
    }
    if("user" %in% dist) {
      df_user <- data.frame(
        x = seq(from = min(df[, param]), to = max(df[, param]), by = 0.001),

        if(user_dist == "norm") {
          y <- dnorm(seq(from = min(df[, param]), to = max(df[, param]), by = 0.001), user_param_1, user_param_2)
        } else if(user_dist == "beta") {
          y <- dbeta(seq(from = min(df[, param]), to = max(df[, param]), by = 0.001), user_param_1, user_param_2)
        } else if(user_dist == "gamma") {
          y <- dgamma(seq(from = min(df[, param]), to = max(df[, param]), by = 0.001), user_param_1, user_param_2)
        } else if(user_dist == "lnorm") {
          y <- dlnorm(seq(from = min(df[, param]), to = max(df[, param]), by = 0.001), user_param_1, user_param_2)

        }
      )
      p_out <- p_out + ggplot2::geom_line(data = df_user,
                                          ggplot2::aes(x = x,
                                          y = y,
                                          colour = "User")
                                          )
    }

  }

  # Export
  p_out <- p_out +
    ggplot2::scale_colour_manual(name = "Distributions",
                                 values = df_legend$col) +
    ggplot2::theme(legend.key = element_rect(fill = "lightgrey"))
  if(!is.null(user_mean)) {
    p_out <- p_out + ggplot2::geom_vline(xintercept = user_mean, lty = 3)
  }
  p_out
}

#' Check range
#' @description This function checks the probability that an input or output falls within a user-defined range.
#' @param df a dataframe.
#' @param param character string. Name of variable of the dataframe for which to check the range.
#' @param min_val numeric. Define the minimum value of the range.
#' @param max_val numeric. Define the maximum value of the range.
#' @return A numeric.
#' @details
#' If only `min_val` is specified, the proportion of iteration above this value will be computed.
#' If only `max_val` is specified, the proportion of iteration below this value will be computed.
#' @examples
#' # Checking how often the "u_pfs" values falls within 0.55 and 0.72.
#' data(df_pa)
#' check_range(df = df_pa,
#'             outcome = "u_pfs",
#'             min_val = 0.55,
#'             max_val = 0.72
#'                  ))
#' @import assertthat
#' @export
check_range <- function(df,
                        param,
                        min_val = NULL,
                        max_val = NULL) {
  # Checks
  assertthat::assert_that(!is.null(df),
                          msg = "No 'df' argument provided.")
  assertthat::assert_that(is.data.frame(df),
                          msg = "'df' argument is not a dataframe.")
  assertthat::assert_that(!is.null(param),
                          msg = "No 'param' argument provided.")
  assertthat::assert_that(is.character(param),
                          msg = "'param' argument is not a character.")
  assertthat::assert_that(!is.null(min_val) & !is.null(max_val),
                          msg = "At least one of the arguments 'min_val' or 'max_val' should be provided.")

  # Compute proportions below, above, both
  if(is.null(min_val) &
     !is.null(max_val)) {
    p_out <-
      round(length(which(df[, param] <= max_val)) / nrow(df) * 100, 4)
    n_out <-
      paste("The proportion of iterations below ",
            max_val,
            " is ",
            p_out,
            "%",
            sep = "")
  }  else if (!is.null(min_val) & is.null(max_val)) {
    p_out <-
      round(length(which(df[, param] >= min_val)) / nrow(df) * 100, 4)
    n_out <-
      paste("The proportion of iterations above ",
            min_val,
            " is ",
            p_out,
            "%",
            sep = "")
  } else if (!is.null(min_val) & !is.null(max_val)) {
    p_out <- round(length(which(df[, param] >= min_val &
                                  df[, param] <= max_val)) / nrow(df) * 100, 4)
    n_out <-
      paste("The proportion of iterations between ",
            min_val,
            " and ",
            max_val,
            " is ",
            p_out,
            "%",
            sep = "")
  }

  # Export
  return(n_out)
}

#' Visualise the distribution of two parameters
#' @description This function plots the distribution of two parameters in a scatterplot.
#' @param df a dataframe.
#' @param param_1 character. Name of variable of the dataframe to be plotted on the x-axis.
#' @param param_2 character. Name of variable of the dataframe to be plotted on the y-axis.
#' @param slope numeric. Default is NULL. If different than 0, plots a linear line with a user-defined intercept and the defined slope.
#' @param intercept numeric. Default is 0. Intercept of the user-defined slope.
#' @param check character. Default is NULL. When set to "param_2 > param_1", plots dot fulfilling the condition in red.
#' @param fit character. Designate the type of smooth model to fit to the relation of `param_1` (x) and `param_2` (y). It can take the values "lm, "glm", "gam", and "loess". A model will be fitted according to the methods described in \code{\link[ggplot2:geom_smooth]{ggplot2::geom_smooth()}}.
#' @return A ggplot graph.
#' @examples
#' # Generating plot for the costs of progression-free health state versus incremental costs
#' data(df_pa)
#' vis_2_params(df = df_pa, param_1 = "c_pfs", "inc_costs")
#' @import assertthat
#' @import ggplot2
#' @export
vis_2_params <- function(df,
                         param_1,
                         param_2,
                         slope = NULL,
                         intercept = 0,
                         check = NULL,
                         fit = NULL) {
  # Checks
  assertthat::assert_that(!is.null(df),
                          msg = "No 'df' argument provided.")
  assertthat::assert_that(is.data.frame(df),
                          msg = "'df' argument is not a dataframe.")
  assertthat::assert_that(!is.null(param_1),
                          msg = "No 'param_1' argument provided.")
  assertthat::assert_that(is.character(param_1),
                          msg = "'param_1' argument is not a character.")
  assertthat::assert_that(!is.null(param_2),
                          msg = "No 'param_2' argument provided.")
  assertthat::assert_that(is.character(param_2),
                          msg = "'param_2' argument is not a character.")
  if(!is.null(check)){
    assertthat::assert_that(is.character(check),
                            msg = "'check' argument is not a character.")
    assertthat::assert_that(check == "param_2 > param_1",
                            msg = "'check' argument is not valid. It should be set to 'param_2 > param_1'.")
  }
  if(!is.null(fit)){
    assertthat::assert_that(fit %in% c("lm", "glm", "gam", "loess"),
                            msg = "'fit' argument is not valid. It should be set to 'lm', 'glm', 'gam', or 'loess'.")
  }

  # Create backbone of the ggplot
  p <- ggplot2::ggplot(data = df, ggplot2::aes_string(x = param_1, y = param_2)) +
    ggplot2::theme_bw()

  # Add styling / options to plot
  ## Slope
  if(!is.null(slope)) {
    p <- p + ggplot2::geom_abline(intercept = intercept, slope = slope, lty = 2, colour = "orange")
  }
  ## % values of param_2 > param_1
  if(!is.null(check)) {
    if(check == "param_2 > param_1"){
      df$col <- ifelse(df[, param_2] > df[, param_1], "TRUE", "FALSE")
      df$col <- as.factor(df$col)
      p_true <- round(length(which(df$col == "TRUE"))/ length(df$col) * 100, 0)
      p_out <- p +
        ggplot2::geom_point(data = df, ggplot2::aes_string(x = param_1, y = param_2, colour = "col"), shape = 1) +
        ggplot2::scale_colour_manual(name = "Check",
                                     values = c("TRUE" = "Red",
                                                "FALSE" = "grey"
                                                )) +
        ggplot2::annotate("text", label = print(paste("P(TRUE):", p_true, "%")), x = min(df[, param_1]) + (max(df[, param_1]) - min(df[, param_1])) * 0.2,  y = max(df[, param_2]))
    }
  } else {
    p_out <- p + ggplot2::geom_point(shape = 1, colour = "grey")
  }

  ## Automatic slop
  if(!is.null(fit)){
      p_out <- p_out +
        ggplot2::geom_smooth(method = fit)
    }

  # Export
  p_out
}

#' Fit distribution to parameter
#' @description This function fits statistical distributions to a parameter.
#' @param df a dataframe.
#' @param param character. Name of variable of the dataframe on which to fit the distributions.
#' @param dist character or vector of character. Determine which distribution to fit on the density plot.
#' @details The available distributions are: "norm" (normal), "beta", "gamma", "lnorm" (lognormal). The arguments of the lists are "AIC" which contains the Akaike Information Criteria for each fitted distribution and "Dist_parameters" which contains the parameters of the fitted distributions.
#' @return A list with two objects #TODO: add what for objects!
#' @examples
#' # Fitting normal and beta distribution to the "u_pfs" variable of the example dataframe.
#' data(df_pa)
#' fit_dist(df = df_pa,
#'          param = "u_pfs",
#'          dist = c("norm", "beta"))
#' @import assertthat
#' @import fitdistrplus
#' @export
fit_dist <- function(df,
                     param,
                     dist = c("norm", "beta", "gamma", "lnorm")) {

  # Checks
  assertthat::assert_that(!is.null(df),
                          msg = "No 'df' argument provided.")
  assertthat::assert_that(is.data.frame(df),
                          msg = "'df' argument is not a dataframe.")
  assertthat::assert_that(!is.null(param),
                          msg = "No 'param' argument provided.")
  assertthat::assert_that(is.character(param),
                          msg = "'param' argument is not a character.")
  assertthat::assert_that(all(dist %in% c("lnorm", "norm", "beta", "gamma")),
                          msg = "'dist' argument is invalid. 'dist' should be 'lnorm', 'norm', 'beta', or 'gamma'.")

  # Set up objects to store statistical fit and parameters of fitted distributions
  l_dist <- l_gof <- vector(mode = "list", length = length(dist))
  names(l_dist) <- names(l_gof) <- dist
  l_out <- vector(mode = "list", length = 2)
  names(l_out) <- c("Statistical_fit", "Dist_parameters")
  m_stats <- matrix(NA,
                    ncol = 4,
                    nrow = length(dist),
                    dimnames = list(NULL,
                                    c("Distribution", "AIC", "BIC", "Kolmorogov-Smirnov")))
  m_stats[, "Distribution"] <- dist
  m_params <- matrix(NA,
                     ncol = 5,
                     nrow = length(dist),
                     dimnames = list(NULL,
                                     c("Distribution", "Name_param_1", "Value_param_1", "Name_param_2", "Value_param_2")))
  m_params[, "Distribution"] <- dist

  # Fit distribution and determine statistical fit of the fitted distributions
  for (i in 1:length(dist)){ # Use apply function here?
    l_dist[[i]] <- fitdistrplus::fitdist(df[, param], distr = dist[i])
  }
  for (i in 1:length(dist)){ # Use apply function here?
    l_gof[[i]] <- fitdistrplus::gofstat(l_dist[[i]])
  }
  for (i in 1:length(dist)){ # Use apply function here?
    m_stats[i, "AIC"] <- round(l_dist[[i]]$aic)
    m_stats[i, "BIC"] <- round(l_dist[[i]]$bic)
    m_stats[i, "Kolmorogov-Smirnov"] <- round(l_gof[[i]]$ks, 3)
  }
  for (i in 1:length(dist)){
    m_params[i, 2:ncol(m_params)] <-
      c(
        names(l_dist[[i]]$estimate)[1],
        round(l_dist[[i]]$estimate[1], 2),
        names(l_dist[[i]]$estimate)[2],
        round(l_dist[[i]]$estimate[2], 2)
      )
  }

  # Export
  l_out[[1]] <- as.data.frame(m_stats)
  l_out[[2]] <- as.data.frame(m_params)
  return(l_out)
}

#' Plot moving average
#' @description This function plots the moving average of a user-defined variable.
#' @param df a dataframe.
#' @param param character string. Name of variable of the dataframe for which to plot the moving average.
#' @param block_size numeric. Define the number of iterations at which the mean param has to be defined and plotted.
#' @param conv_limit numeric. Define the convergence limit, under which the relative change between block of iterations should lie.
#' @param breaks numeric. Number of iterations at which the breaks should be placed on the plot. Default is NULL, hence a tenth of the length of the vector `param` is used.
#' @param variance logical. Determine whether the variance of the vector should be plotted instead of the mean. Default is FALSE.
#' @return A ggplot graph.
#' @examples
#' # Checking the moving average of the incremental QALYs using the example data.
#' data(df_pa)
#' plot_convergence(df = df_pa,
#'                  param = "inc_qaly"
#'                  )
#' @import assertthat
#' @import ggplot2
#' @export
plot_convergence <- function(df,
                             param,
                             block_size = 500,
                             conv_limit = 0,
                             breaks = NULL,
                             variance = FALSE) {
  # Checks
  assertthat::assert_that(is.data.frame(df),
                          msg = "'df' argument is not a dataframe.")
  assertthat::assert_that(!is.null(param),
                          msg = "No 'param' argument provided.")
  assertthat::assert_that(is.character(param),
                          msg = "'param' argument is not a character.")
  assertthat::assert_that(block_size < length(df[, param]),
                          msg = "'block_size' is greater than the number of iterations. 'block_size' should be smaller than the number of rows in 'df'.")
  if(conv_limit > 1 ||
     conv_limit < 0) {
    stop("`conv_limit` should be a numeric between 0 and 1.")
  }
  if(is.null(breaks) ||
     breaks > length(df[, param])) {
    breaks <- round(length(df[, param]) / 10)
  }
  if(is.null(block_size) ||
     block_size > length(df[, param])) {
    block_size <- round(length(df[, param]) / 10)
  }
  l_output <- list()

  # Calculations
  v_output <- df[, param]
  v_av_mov  <- unname(cumsum(v_output)/c(1:length(v_output))) # moving average
  v_blocks <- seq(from = block_size, to = length(v_av_mov), by = block_size)
  v_av_blocks <- v_av_mov[v_blocks] # average at each block
  v_rel_diff_blocks <- c(0, abs(diff(v_av_blocks))/ v_av_blocks[c(1:(length(v_av_blocks)-1))]) # Check relative difference a block and the previous
  v_conv_rel <- which(v_rel_diff_blocks < conv_limit)
  v_var_param <- vapply(1:length(v_output), function (x){
    var(v_output[1:x])
  },
  numeric(1)
  )
  v_var_blocks <- v_var_param[v_blocks]
  v_var_rel_diff_blocks <- c(0, abs(diff(v_var_blocks))/ v_var_blocks[c(1:(length(v_var_blocks)-1))]) # Check relative difference a block and the previous

  # Dataframe for plotting the results
  df_plot <- data.frame(
    Iterations = v_blocks,
    Mean_value = v_av_blocks,
    Rel_diff = v_rel_diff_blocks,
    Variance = v_var_blocks,
    Rel_var_diff = v_var_rel_diff_blocks
  )
  names(df_plot)[2] <- param
  names(df_plot)[3] <- paste0("Relative_mean_diff_", param)
  names(df_plot)[4] <- paste0("Variance_", param)
  names(df_plot)[5] <- paste0("Relative_var_diff_", param)

  # Determine breaks for plot
  v_breaks <- seq(from = breaks, to = length(v_av_mov), by = breaks)
  v_breaks[length(v_breaks)] <- length(v_av_mov)

  # Plot
  if(variance == FALSE){
    if(conv_limit > 0) {
      p <- ggplot2::ggplot(data = df_plot, ggplot2::aes_string(x = "log(Iterations)", y = paste0("Relative_mean_diff_", param))) +
        ggplot2::geom_hline(yintercept = conv_limit,
                            colour = "orange",
                            linetype = "dashed")
    } else {
      p <- ggplot2::ggplot(data = df_plot, ggplot2::aes_string(x = "log(Iterations)", y = param))
    }
  } else {
    if(conv_limit > 0) {
      p <- ggplot2::ggplot(data = df_plot, ggplot2::aes_string(x = "log(Iterations)", y = paste0("Relative_var_diff_", param))) +
        ggplot2::geom_hline(yintercept = conv_limit,
                            colour = "orange",
                            linetype = "dashed")
    } else {
      p <- ggplot2::ggplot(data = df_plot, ggplot2::aes_string(x = "log(Iterations)", y = paste0("Variance_", param)))
    }
  }
  p_out <- p +
    ggplot2::xlab("Iterations (log scale)") +
    ggplot2::scale_x_continuous(breaks = log(v_breaks),
                                labels = v_breaks) +
    ggplot2::geom_line() +
    ggplot2::theme_bw()

  # Export
  return(p_out)
}

#' Check sum probabilities
#' @description This function checks whether the sum of user-defined probabilities is below or equal to 1
#' @param ... character vector. This character vector contains the name of the variables of which the sum will be checked.
#' @param df a dataframe.
#' @param digits numeric. Define the number of digits at which the sum of probabilities is rounded
#' @param check logical. Define which check to perform."lower" checks whether the sum of the selected variables is lower than or equal to 1 for each iteration. "equal" checks whether the sum of the selected variables is equal to 1 for each iteration. Default is "lower".
#' @param max_view numeric. Determines the number of iterations to display which do not fulfil the check. Default is 100.
#' @return A text.
#' @examples
#' # Checking whether the sum of the two probabilities is lower than or equal to 1
#' check_sum_probs("p_pfspd", "p_pfsd", df = df_pa, check = "lower")
#'
#' # Checking the sum of the two probabilities equals 1 using a vector to select them,
#' # Rounding off to two digits, and extending the number of iterations to display to 250.
#' check_sum_probs(c("p_pfspd", "p_pfsd"), df = df_pa, digits = 2, check = "equal", max_view = 250)
#' @import assertthat
#' @export
check_sum_probs <- function(..., df, digits = NULL, check = "lower", max_view = 100){

  # Gather inputs and checks
  l_vars <- list(...)
  v_vars <- unlist(l_vars, use.names = FALSE)
  assertthat::assert_that(all(v_vars %in% names(df)),
                          msg = "'...' contains names of variables that are not included in 'df'.")
  assertthat::assert_that(check %in% c("lower", "equal"),
                          msg = "'check' argument is invalid. It should be set to 'lower' or 'equal'.")
  if(!is.null(digits)) {
    assertthat::assert_that(is.numeric(digits), msg = "'digit' argument is invalid. It should be a numeric value.")
  }
  # Calculate sum
  v_calc <- rowSums(df[, as.character(v_vars)])

  if(!is.null(digits)) {
    v_calc <- round(v_calc, digits)
  }
  if(check == "lower") {
    v_id_higher <- which(v_calc > 1)
    if(length(v_id_higher) > 0) {
      max_view <- ifelse(max_view > length(v_id_higher), length(v_id_higher), max_view)
      return(paste("The sum of probabilities is higher than 1 in the following iterations:", paste(v_id_higher[1:max_view], collapse = ", ")))
    } else {
      return("The sum of probabilities in all iterations is lower or equal to 1")
    }
  } else if(check == "equal") {
    v_id_diff <- which(v_calc != 1)
    if(length(v_id_diff) > 0) {
      max_view <- ifelse(max_view > length(v_id_diff), length(v_id_diff), max_view)
      return(paste("The sum of probabilities is different than 1 in the following iterations:", paste(v_id_diff[1:max_view], collapse = ", ")))
    } else {
      return("The sum of probabilities in all iterations is equal to 1")
    }
  }

}


#' Check whether variables are strictly positive
#' @description This function checks whether variables are strictly positive (for instance for costs and relative risks inputs)
#' @param ... character vector. This character vector contains the name of the variables of which the sum will be checked.
#' @param df a dataframe.
#' @param max_view numeric. Determines the number of iterations to display which do not fulfil the check. Default is 50.
#' @return A dataframe.
#' @examples
#' # Checking whether a variable is strictly positive
#' check_positive("c_pfs", df = df_pa)
#'
#' # Checking whether two variables are strictly positive
#' # Descreasing the number of iterations to display to 20.
#' check_positive("c_pfs", "c_pd", df = df_pa)
#' @import assertthat
#' @import stringi
#' @export
check_positive <- function(..., df, max_view = 50){
  # Gather inputs and checks
  l_vars <- list(...)
  v_vars <- unlist(l_vars, use.names = FALSE)
  assertthat::assert_that(all(v_vars %in% names(df)),
                          msg = "'...' contains names of variables that are not included in 'df'.")
  assertthat::assert_that(is.numeric(max_view),
                          msg = "'max_view' argument is invalid. It should be a numeric value.")

  # Create list of negative values per input, or a single list when only 1 input is selected
  list_neg <- if(length(v_vars) > 1){
    lapply(df[, v_vars], function(x) which(x < 0))
  } else {
    list(which(df[, v_vars] < 0))
  }

  m_neg <- stringi::stri_list2matrix(list_neg, byrow = TRUE)
  n_col <- ifelse(ncol(m_neg) < max_view, ncol(m_neg), max_view)
  m_neg <- if(n_col == 0) {
    "None"} else {
      m_neg[, c(1:n_col)]
    }

  v_neg <- if(length(v_vars) > 1 & n_col > 0){
    apply(m_neg, 1, function(x) paste(x, collapse = ","))
  } else if(length(v_vars) == 1 & n_col > 0){
    paste(m_neg, collapse = ",")
  } else {
    rep("None", length(v_vars))
  }

  v_neg <- gsub(",NA", "", v_neg)
  v_neg <- gsub("NA", "None", v_neg)
  v_neg <- gsub(",", ", ", v_neg)

  df_res <- data.frame(
    Input = v_vars,
    Negative_values = v_neg
  )
  return(df_res)
}

#' Check sum variables
#'
#' @description This function checks whether the sum of selected variables are equal to another.
#'
#' @param ... character vector. This character vector contains the name of the variables of which the sum will be checked.
#' @param df a dataframe.
#' @param outcome character string. Name of variable of the dataframe which should equal the sum of variables mentioned in `...`.
#' @param digits Define the number of digits at which the sum and the `outcome` variables are rounded.
#'
#' @return A string.
#'
#' @examples
#' # Checking whether health state and adverse event costs equal the total discounted costs
#' check_sum_vars("t_costs_pfs_d_int", "t_costs_pd_d_int", "t_costs_ae_int",
#'                df = head(df_pa),
#'                outcome = "t_costs_d_int",
#'                digits = 0)
#'
#' @export
#'
check_sum_vars <- function(...,
                           df,
                           outcome,
                           digits = 3) {
  l_vars <- list(...)
  v_vars <- unlist(l_vars, use.names = FALSE)
  v_calc <- as.numeric(as.character(rowSums(df[, as.character(v_vars)])))
  v_outcome <- as.numeric(as.character(df[, outcome]))

  if (!is.null(digits)) {
    v_calc <- round(v_calc, digits)
    v_outcome <- round(v_outcome, digits)
  }

  v_diff <- which(v_calc != v_outcome)

  if(length(v_diff) == 0) {
    res <- paste("Sums of", paste(v_vars, collapse = ", ") ,"variables equal", outcome, "variabe")
  } else {
    res <- paste("Sums of", paste(v_vars, collapse = ", "), "variables does not equal", outcome, "variabe in the following iterations:", paste(v_diff, collapse = ", "))
  }
  return(res)
}


#' Check range
#'
#' @description Checks whether variables always fall within a given range.
#' @param df Data
#' @param vars Character vector of variables to check
#' @param min Minimum allowed value (default: 0)
#' @param max Maximum allowed value (default: 1)
#' @return List containing the results of the check (checks), and a tibble
#' of status and message for each test (messages).
#' @details Note that both the minimum and maximum are _inclusive_, that is,
#' the range is given as [min, max]. For _exclusive_ checks, e.g. (0,1], or
#' greater than 0, lesser than or equal to 1, the user will have to manually
#' give a minimum value with a certain error applied (e.g., 1e-6).
#'
#' The list of messages in the result contains a single line if the test passed,
#' or if a test failed for one or more variables, a line for each failure.
#' @import glue
#' @import dplyr
#' @examples
#' data(df_pa)
#' check_range(df_pa, c("u_pfs", "p_pfspd"))
#' @export
# check_range <- function(df, vars, min = 0, max = 1){
#   gte_min <- do_check(df, vars, ~all(.x >= min), glue::glue("greater than or equal to {min}"))
#   lte_max <- do_check(df, vars, ~all(.x <= max), glue::glue("less than or equal to {max}"))
#
#   return(list(checks = tibble(vars, min = gte_min$check, max = lte_max$check), messages = dplyr::bind_rows(gte_min$messages, lte_max$messages)))
# } #@KAREL: DIT ZORGT VOOR CONFLICTEN MET `check_range()` HIERBOVEN

do_check <- function(df, vars, check, label_check, template_ok = "all variables are {label_check}", template_fail = "{var} is not {label_check}") {
  pass <- summarise(df, across(!!vars, check)) %>% summarise(across(everything(), all))

  if (all(pass == TRUE)){
    messages <- tibble::tibble(ok = TRUE, message = glue::glue(template_ok))
  } else {
    messages <- tibble::tibble(ok = FALSE, message = glue::glue(template_fail, var = vars[!pass]))
  }
  return(list(check = unlist(pass), messages = messages))
}

#' Check binary
#'
#' @description This function checks whether the value of variables remain between 0 and 1 (for instance for utility and probability inputs)
#'
#' @param ... character vector. This character vector contains the name of the variables of which the sum will be checked.
#' @param df a dataframe.
#' @param max_view numeric. Determines the number of iterations to display which do not fulfil the check. Default is 50.
#'
#' @return A dataframe.
#'
#' @examples
#' # Checking whether a variable is strictly positive
#' check_binary("u_pfs", df = df_pa)
#'
#' # Checking whether two variables are strictly positive
#' # Descreasing the number of iterations to display to 20.
#' check_binary("u_pfs", "p_pfspd", df = df_pa)
#' @import stringi
#' @export
#'
check_binary <- function(..., df, max_view = 50) {

  l_vars <- list(...)
  v_vars <- unlist(l_vars, use.names = FALSE)

  # Create list of negative values per input, or a single list when only 1 input is selected
  list_neg <- if(length(v_vars) > 1){
    lapply(df[, v_vars], function(x) which(x < 0))
  } else {
    list(which(df[, v_vars] < 0))
  }

  m_neg <- stringi::stri_list2matrix(list_neg, byrow = TRUE)
  n_col <- ifelse(ncol(m_neg) < max_view, ncol(m_neg), max_view)
  m_neg <- if(n_col == 0) {
    "None"} else {
      m_neg[, c(1:n_col)]
    }

  v_neg <- if(length(v_vars) > 1 & n_col > 0){
    apply(m_neg, 1, function(x) paste(x, collapse = ","))
  } else if(length(v_vars) == 1 & n_col > 0){
    paste(m_neg, collapse = ",")
  } else {
    rep("None", length(v_vars))
  }

  v_neg <- gsub(",NA", "", v_neg)
  v_neg <- gsub("NA", "None", v_neg)
  v_neg <- gsub(",", ", ", v_neg)

  # Create list of negative values per input, or a single list when only 1 input is selected
  list_high <- if(length(v_vars) > 1){
    lapply(df[, v_vars], function(x) which(x > 1))
  } else {
    list(which(df[, v_vars] > 1))
  }

  m_high <- stringi::stri_list2matrix(list_high, byrow = TRUE)
  n_col  <- ifelse(ncol(m_high) < max_view, ncol(m_high), max_view)
  m_high <- if(n_col == 0) {
    "None"} else {
      m_high[, c(1:n_col)]
    }

  v_high <- if(length(v_vars) > 1 & n_col > 0){
    apply(m_high, 1, function(x) paste(x, collapse = ","))
  } else if(length(v_vars) == 1 & n_col > 0){
    paste(m_high, collapse = ",")
  } else {
    rep("None", length(v_vars))
  }

  v_high <- gsub(",NA", "", v_high)
  v_high <- gsub("NA", "None", v_high)
  v_high <- gsub(",", ", ", v_high)

  df_res <- data.frame(
    Input = v_vars,
    Negative_values = v_neg,
    Values_above_1 = v_high
  )
  return(df_res)
}

#' Check PSA inputs & outputs
#' @description This function checks whether the value of variables remain between 0 and 1 for utility and probability inputs, and are strictly positive for costs, hazard ratios, odds ratios, relative risks, and total outcomes of each strategy.
#' @param l_psa_darth a list of class 'psa' as obtained by the function [dampack::make_psa_obj()]
#' @param utility characters. String used at the start of the variables identifying utility inputs.
#' @param costs characters. String used at the start of the variables identifying cost inputs.
#' @param probs characters. String used at the start of the variables identifying probability inputs.
#' @param rr characters. String used at the start of the variables identifying relative risk inputs.
#' @param hr characters. String used at the start of the variables identifying hazard ratio inputs.
#' @param or characters. String used at the start of the variables identifying odds ratio inputs.
#' @param exclude vector of strings. Vector containing the name of the input variables not to include in the checks. Default is NULL, hence all variables from the 'parameters' dataframe are included.
#' @param v_outcome vector of strings. Vector containing the name of the output variables to include in the checks. Default values are 'effectiveness' and 'cost'.
#' @return A matrix containing the input and output variables that have been checked and the iterations wherein an erroneous value has been identified.
#' @export
check_psa_darth <- function(l_psa_darth,
                            utility = "u_",
                            costs = "c_",
                            probs = "p_",
                            rr = "rr_",
                            hr = "hr_",
                            or = "or_",
                            exclude = NULL,
                            v_outcome = c("effectiveness", "cost")) {
  # SET UP ----
  assertthat::assert_that(is.list(l_psa_darth), msg = "l_psa_darth is not a list")
  if (length(v_outcome) > 0) {
    assertthat::assert_that(all(v_outcome %in% names(l_psa_darth)), msg = "not all elements of 'v_outcomes' are included in the 'l_psa_darth' list")
  }
  if (length(exclude) > 0) {
    v_exclude   <- which(names(l_psa_darth$parameters) %in% exclude)
    l_psa_darth$parameters <- l_psa_darth$parameters[, -v_exclude]
  }
  collapse_vector <- function(x) {
    ifelse(length(x) == 0, paste0("none"), paste(x, collapse = ","))
  }

  # UTILITIES & PROBABILITIES ----
  v_zero_one <- c(grep(paste0("^", utility), names(l_psa_darth$parameters)), grep(paste0("^", probs), names(l_psa_darth$parameters)))
  m_error_zero_one <- matrix(
    data = NA,
    nrow = length(v_zero_one),
    ncol = 2,
    dimnames = list(NULL, c("Parameter", "Iterations_error"))
  )
  m_error_zero_one[, "Parameter"] <- names(l_psa_darth$parameters)[v_zero_one]
  check_zero_one <- sapply(v_zero_one, function(column) {
    v_error <- which(l_psa_darth$parameters[, column] < 0 |
                       l_psa_darth$parameters[, column] > 1)
    return(v_error)
  })

  m_error_zero_one[, "Iterations_error"] <- unlist(lapply(check_zero_one, collapse_vector))

  # POSITIVE INPUTS ----
  v_positive <- c(
    grep(paste0("^", costs), names(l_psa_darth$parameters)),
    grep(paste0("^", rr), names(l_psa_darth$parameters)),
    grep(paste0("^", hr), names(l_psa_darth$parameters)),
    grep(paste0("^", or), names(l_psa_darth$parameters))
  )
  m_error_positive <- matrix(
    data = NA,
    nrow = length(v_positive),
    ncol = 2,
    dimnames = list(NULL, c("Parameter", "Iterations_error"))
  )
  m_error_positive[, "Parameter"] <- names(l_psa_darth$parameters)[v_positive]
  check_positive <- sapply(v_positive, function(column) {
    v_error <- which(l_psa_darth$parameters[, column] < 0)
    return(v_error)
  })
  m_error_positive[, "Iterations_error"] <- unlist(lapply(check_positive, collapse_vector))

  # OUTCOMES ----
  m_error_outcome <- matrix(
    data = NA,
    nrow = length(v_outcome) * l_psa_darth$n_strategies,
    ncol = 2,
    dimnames = list(NULL, c("Parameter", "Iterations_error"))
  )
  m_error_outcome[, "Parameter"] <- paste(rep(v_outcome, each = l_psa_darth$n_strategies),
                                          l_psa_darth$strategies,
                                          sep = "_")
  check_outcome <- sapply(v_outcome, function(outcome) {
    m_error <- sapply(1:ncol(l_psa_darth[[outcome]]), function(column) {
      which(l_psa_darth[[outcome]][, column] < 0)
    })
    return(m_error)
  })
  m_error_outcome[, "Iterations_error"] <- unlist(lapply(check_outcome, collapse_vector))

  # COMBINE & EXPORT ----
  m_error_all <- rbind(m_error_zero_one, m_error_positive, m_error_outcome)
  return(m_error_all)
}


#' Check mean quality of life
#'
#' @description This function checks whether the mean quality of life outcome of each iteration remain between the maximum and minimum utility values of the specific iteration.
#'
#' @param df a dataframe.
#' @param t_ly character. Name of the variable containing the total undiscounted life years.
#' @param t_qaly character. Name of the variable containing the total undiscounted quality-adjusted life years.
#' @param u_values (vector of) character. Name(s) of the variable containing the utility values.
#' @param max_view numeric. Determines the number of iterations to display which do not fulfil the check. Default is 100.
#' @return A matrix.
#' @examples
#' # Check whether mean quality of life is within min-max utility values
#' check_mean_qol(df = df_pa,
#'                t_ly = "t_ly_comp",
#'                t_qaly = "t_qaly_comp",
#'                u_values = c("u_pfs", "u_pd")
#'                )
#'
#' @export
check_mean_qol <- function(df,
                           t_qaly,
                           t_ly,
                           u_values,
                           max_view = 100){
  n_sim <- nrow(df)
  m_res <- matrix(NA,
                  ncol = 2,
                  nrow = n_sim)
  m_res_fct <- matrix()

  for (i in 1:n_sim){

    m_res[i, 1] <- df[i, t_qaly] / df[i, t_ly] >= min(df[i, u_values])
    m_res[i, 2] <- df[i, t_qaly] / df[i, t_ly] <= max(df[i, u_values])
  }

  if(length(which(m_res != TRUE)) == 0) {
    m_res_fct <- matrix("None",
                        ncol = 2,
                        nrow = 1,
                        dimnames = list(" ",
                                        c("Mean_QoL_below_min",
                                          "Mean_QoL_above_max")
                        )
    )
  } else {
    n_it_below <- length(which(m_res[, 1] == FALSE))
    n_it_above <- length(which(m_res[, 2] == FALSE))

    max_view_1 <- ifelse(n_it_below < max_view, n_it_below, max_view)
    max_view_2 <- ifelse(n_it_above < max_view, n_it_above, max_view)

    v_it_below <- ifelse(n_it_below == 0, 0, paste(which(m_res[, 1] == FALSE)[1:max_view_1], collapse = ", "))
    v_it_above <- ifelse(n_it_above == 0, 0, paste(which(m_res[, 2] == FALSE)[1:max_view_2], collapse = ", "))

    m_res_fct <- matrix(c(n_it_below, n_it_above,
                          v_it_below, v_it_above),
                        ncol = 2,
                        nrow = 2,
                        byrow = TRUE,
                        dimnames = list(c("Number of iteration with issue",
                                          "Iteration number with issue"),
                                        c("Mean_QoL_below_min",
                                          "Mean_QoL_above_max")
                        )
    )
  }
  return(m_res_fct)
}

#' Perform quick checks of inputs and outputs
#'
#' @description This function performs multiple checks on user-defined columns.
#'
#' @param df a dataframe.
#' @param v_probs (a vector of) character. Name of variables containing probabilities.
#' @param v_utilities (a vector of) character. Name of the variables containing utility values.
#' @param v_costs (a vector of) character. Name of the variables containing cost estimates.
#' @param v_hr (a vector of) character. Name of the variables containing hazard ratios.
#' @param v_rr (a vector of) character. Name of the variables containing relative risks.
#' @param v_r (a vector of) character. Name of the variables containing rates.
#' @param v_outcomes (a vector of) character. Name of the variables containing outcomes of the model.
#'
#' @return A matrix.
#'
#' @examples
#' # Checking costs and utility values of the example data
#' do_quick_check(df = df_pa,
#'                v_utilities = c("u_pfs", "u_pd"),
#'                v_costs = c("c_pfs", "c_pd", "c_thx")
#'                )
#'
#' @import testthat
#' @export
#'
do_quick_check <- function(df,
                           v_probs = NULL,
                           v_utilities = NULL,
                           v_costs = NULL,
                           v_hr = NULL,
                           v_rr = NULL,
                           v_r = NULL,
                           v_outcomes = NULL
) {

  if(!is.null(v_probs)) {

    res_probs_1 <- testthat::test_that("All probabilities are positive", {
      for (i in v_probs) {
        for (j in 1:nrow(df)) {
          testthat::expect_gte(df[j, i], 0)
        }
      }
    }
    )
  } else {
    res_probs_1 <- "NOT PERFORMED"
  }

  if(!is.null(v_probs)) {

    res_probs_2 <- testthat::test_that("All probabilities are lower or equal to 1", {
      for (i in v_probs) {
        for (j in 1:nrow(df)) {
          testthat::expect_lte(df[j, i], 1)
        }
      }
    }
    )
  } else {
    res_probs_2 <- "NOT PERFORMED"
  }

  if(!is.null(v_utilities)) {

    res_utilities_1 <- testthat::test_that("All utility values are positive", {
      for (i in v_utilities) {
        for (j in 1:nrow(df)) {
          testthat::expect_gte(df[j, i], 0)
        }
      }
    }
    )
  } else {
    res_utilities_1 <- "NOT PERFORMED"
  }

  if(!is.null(v_utilities)) {

    res_utilities_2 <- testthat::test_that("All utility values are lower or equal to 1", {
      for (i in v_utilities) {
        for (j in 1:nrow(df)) {
          testthat::expect_lte(df[j, i], 1)
        }
      }
    }
    )
  } else {
    res_utilities_2 <- "NOT PERFORMED"
  }

  if(!is.null(v_costs)) {

    res_costs <- testthat::test_that("All costs parameters are positive", {
      for (i in v_costs) {
        for (j in 1:nrow(df)) {
          testthat::expect_gte(df[j, i], 0)
        }
      }
    }
    )
  } else {
    res_costs <- "NOT PERFORMED"
  }

  if(!is.null(v_hr)) {

    res_hr <- testthat::test_that("All hazard ratios are positive", {
      for (i in v_hr) {
        for (j in 1:nrow(df)) {
          testthat::expect_gte(df[j, i], 0)
        }
      }
    }
    )
  } else {
    res_hr <- "NOT PERFORMED"
  }

  if(!is.null(v_rr)) {

    res_rr <- testthat::test_that("All relative risks are positive", {
      for (i in v_rr) {
        for (j in 1:nrow(df)) {
          testthat::expect_gte(df[j, i], 0)
        }
      }
    }
    )
  } else {
    res_rr <- "NOT PERFORMED"
  }

  if(!is.null(v_r)) {

    res_r <- testthat::test_that("All rates are positive", {
      for (i in v_r) {
        for (j in 1:nrow(df)) {
          testthat::expect_gte(df[j, i], 0)
        }
      }
    }
    )
  } else {
    res_r <- "NOT PERFORMED"
  }

  if(!is.null(v_outcomes)) {

    res_outcomes <- testthat::test_that("All outcomes are positive", {
      for (i in v_outcomes) {
        for (j in 1:nrow(df)) {
          testthat::expect_gte(df[j, i], 0)
        }
      }
    }
    )
  } else {
    res_outcomes <- "NOT PERFORMED"
  }

  df_res <- data.frame(
    Test = c("All probabilities are positive",
             "All probabilities are lower or equal to 1",
             "All utility values are positive",
             "All utility values are lower or equal to 1",
             "All costs parameters are positive",
             "All hazard ratios are positive",
             "All relative risks are positive",
             "All rates are positive",
             "All outcomes are positive"),
    Result = c(res_probs_1,
               res_probs_2,
               res_utilities_1,
               res_utilities_2,
               res_costs,
               res_hr,
               res_rr,
               res_r,
               res_outcomes)
  )
  return(df_res)
}

#' Perform discounted and undiscounted results check
#'
#' @description This function performs multiple checks on user-defined columns.
#'
#' @param df a dataframe.
#' @param v_outcomes (a vector of) character. Name of the variables containing undiscounted outcomes of the model.
#' @param v_outcomes_d (a vector of) character. Name of the variables containing discounted outcomes of the model.
#'
#' @details The variables contained in `v_outcomes` and `v_outcomes_d` should be in the same order.
#'
#' @return A matrix.
#'
#' @examples
#' # Checking whether discounted QALYs are lower than undiscounted QALYs using the example data
#' do_discount_check(df = df_pa,
#'                   v_outcomes = c("t_qaly_comp", "t_qaly_int"),
#'                   v_outcomes_d = c("t_qaly_d_comp", "t_qaly_d_int")
#'                   )
#' @import testthat
#' @export
do_discount_check <- function(df,
                              v_outcomes = NULL,
                              v_outcomes_d = NULL
) {

  if(length(v_outcomes) != length(v_outcomes_d)){
    stop("Number of variables with discounted and undiscounted outcomes is different.")
  }

  res <- testthat::test_that("All discounted outcomes are lower than undiscounted outcomes", {

    for (i in c(1:length(v_outcomes))) {
      for (j in 1:length(df)) {
        testthat::expect_lt(df[j, v_outcomes_d[i]], df[j, v_outcomes[i]])
      }
    }
  }
  )

  df_res <- data.frame(
    Test = "All discounted outcomes are lower than undiscounted outcomes",
    Result = res
  )

  return(df_res)
}

#' Check parametric survival models
#'
#' @description This function checks whether the first of two parametric survival model is lower than a second parametric survival model.
#'
#' @param df a dataframe.
#' @param surv_mod_1 character. Name of the parametric model to use for the first survival model.
#' @param surv_mod_2 character. Name of the parametric model to use for the second survival model.
#' @param v_names_param_mod_1 (vector of) character. Name of the columns containing the parameter values for the first survival model.
#' @param v_names_param_mod_2 (vector of) character. Name of the columns containing the parameter values for the second survival model.
#' @param time a numerical vector. Determine at which time points survival probabilities have to be estimated for both survival models. For each of these time points, it will be checked whether the first survival model results in higher survival probabilities than the second survival model.
#' @param label_surv_1 character vector. The label to provide to the first survival curve (relevant for export).
#' @param label_surv_2 character vector. The label to provide to the second survival curve (relevant for export).
#' @param n_view integer. Number of iterations to mention in which the curves are crossing. Default is 10.
#'
#' @details The parametric models that can be used are the following: exponential (\code{\link[stats:pexp]{`exp`}}), Weibull (\code{\link[stats:pweibull]{`weibull`}}), gamma (\code{\link[stats:pgamma]{`gamma`}}), loglogistic (\code{\link[stats:plogis]{`logis`}}), and lognormal (\code{\link[stats:plnorm]{`lnorm`}}). All these functions are implemented following their distribution function as documented in the \link[=stats]{stats} package.
#'
#' @return A list. The first element is a message, the second element contains the number of the iterations in which the the first curve is higher than the second curve.
#'
#' @import glue
#' @export
check_surv_mod <- function(df,
                           surv_mod_1,
                           surv_mod_2,
                           v_names_param_mod_1,
                           v_names_param_mod_2,
                           time = seq(0, 5, 0.1),
                           label_surv_1 = "first survival",
                           label_surv_2 = "second survival",
                           n_view = 10
) {

  l_out <- list()

  v_check_cross <- vapply(1:nrow(df), function (x) {
    v_surv_1 <- 1 - do.call(paste0("p", surv_mod_1), c(list(time), unname(as.list(df[x, v_names_param_mod_1]))))
    v_surv_2 <- 1 - do.call(paste0("p", surv_mod_2), c(list(time), unname(as.list(df[x, v_names_param_mod_2]))))
    v_higher <- which(v_surv_1 > v_surv_2)
    out <- length(v_higher) > 0
    return(out)
  },
  logical(1)
  )
  v_n_cross <- which(v_check_cross == TRUE)
  v_n_cross_message <- ifelse(length(v_n_cross) > n_view, paste(paste(v_n_cross[1:n_view], collapse = ", "), "and more"), paste(v_n_cross, collapse = ", "))
  message_fail_template <- "Pay attention, the {label_surv_1} curve is higher than the {label_surv_2} curve in iterations {v_n_cross_message}"
  message_ok_template <- "The {label_surv_1} curve is lower than the {label_surv_2} curve in all iterations."
  message_ok <- glue::glue(message_ok_template)
  message_fail <- glue::glue(message_fail_template)
  message   <- ifelse(length(v_n_cross) > 0,
                      paste(message_fail),
                      paste(message_ok)
                      )
  l_out <- list(message = message,
                v_n_cross = v_n_cross)
  return(l_out)
}

#' Plot parametric survival models
#'
#' @description This function plots two parametric survival models based on he functional form of the model and their parameters.
#'
#' @param df a dataframe.
#' @param surv_mod_1 character. Name of the parametric model to use for the first survival model.
#' @param surv_mod_2 character. Name of the parametric model to use for the second survival model.
#' @param v_names_param_mod_1 (vector of) character. Name of the columns containing the parameter values for the first survival model.
#' @param v_names_param_mod_2 (vector of) character. Name of the columns containing the parameter values for the second survival model.
#' @param label_surv_1 character vector. The label to provide to the first survival curve (relevant for export).
#' @param label_surv_2 character vector. The label to provide to the second survival curve (relevant for export).
#' @param iteration integer. The row number of the iterations for which the parametric survival models have to be plotted.
#' @param time a numerical vector. Determine at which time points survival probabilities have to be estimated for both survival models. For each of these time points, it will be checked whether the first survival model results in higher survival probabilities than the second survival model.
#'
#' @details The parametric models that can be used are the following: exponential (\code{\link[stats:pexp]{`exp`}}), Weibull (\code{\link[stats:pweibull]{`weibull`}}), gamma (\code{\link[stats:pgamma]{`gamma`}}), loglogistic (\code{\link[stats:plogis]{`logis`}}), and lognormal (\code{\link[stats:plnorm]{`lnorm`}}). All these functions are implemented following their distribution function as documented in the \link[=stats]{stats} package.
#'
#' @return A ggplot object.
#'
#' @import glue
#' @export
plot_surv_mod <- function(df,
                          surv_mod_1,
                          surv_mod_2,
                          v_names_param_mod_1,
                          v_names_param_mod_2,
                          label_surv_1 = "surv_mod_1",
                          label_surv_2 = "surv_mod_2",
                          iteration,
                          time = seq(0, 5, 1)
){

  df_plot <- data.frame(
    Time = rep(time, 2),
    Label = c(rep(label_surv_1, length(time)),rep(label_surv_2, length(time))),
    Survival = c(1 - do.call(paste0("p", surv_mod_1), c(list(time), unname(as.list(df[iteration, v_names_param_mod_1])))),
                 1 - do.call(paste0("p", surv_mod_2), c(list(time), unname(as.list(df[iteration, v_names_param_mod_2]))))
    )
  )

  p <- ggplot2::ggplot(ggplot2::aes(x = Time,
                                    y = Survival,
                                    col = Label),
                       data = df_plot) +
    ggplot2::geom_line() +
    ggplot2::xlab("Time") +
    ggplot2::ylab("P(Survival)") +
    ggplot2::theme_bw()

  return(p)
}
