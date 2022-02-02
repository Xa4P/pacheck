#' Generate summary statistics
#'
#' @description This function generates summary statistics of input and output values of a probabilistic analysis.
#'
#' @param df a dataframe.
#' @param v_params character or vector of character. Vector of names of the inputs and outputs for which to return summary statistics. Default is "ALL" which returns summary values for all inputs and outputs in the dataframe.
#'
#' @return A dataframe with summary data for selected inputs and outputs.
#'
#' @examples
#' # Generating summary data of all inputs using the example dataframe
#' data(df_pa)
#' df_summary <- generate_sum_stats(df_pa)
#'
generate_sum_stats <- function(df,
                               v_params = "ALL"){

  df <- if(v_params == "ALL") {
    df
  } else {
    data.frame(df[, v_params])
  }

  df_out <- data.frame(Parameter = if(length(v_params) == 1 & v_params!= "ALL") { v_params } else{ names(df) },
                           Mean = apply(df, 2, mean),
                           SD = apply(df, 2, sd),
                           Percentile_2.5th = apply(df, 2, function(x) quantile(x, 0.025)),
                           Percentile_97.5th = apply(df, 2, function(x) quantile(x, 0.975)),
                           Minimum = apply(df, 2, min),
                           Maximum = apply(df, 2, max)
  )

  df_out[, 2:ncol(df_out)] <- apply(df_out[, 2:ncol(df_out)], 2, function(x) round(x, 3))
  rownames(df_out) <- NULL

  return(df_out)

}


#' Generate correlation matrix
#'
#' @description This function generates the correlation matrix of input and output values of a probabilistic analysis.
#'
#' @param df a dataframe.
#' @param v_params character or vector of character. Vector of names of the inputs and outputs for which to return the correlation matrix. Default is "ALL" which returns the correlation matrix for all inputs and outputs in the dataframe.
#'
#' @return A table with summary data for selected inputs and outputs.
#'
#' @examples
#' # Generating summary data of all inputs using the example dataframe
#' data(df_pa)
#' generate_cor(df_pa)
#'
#' @export
#'
#'
generate_cor <- function(df,
                         v_params = "ALL"){ # doesn't work with 1 parameter!

  df <- if(v_params == "ALL") {
    df
  } else {
    data.frame(df[, v_params])
  }

  df_out <- cor(df)

  return(df_out)

}


#' Visualise the distribution of a single parameter
#'
#' @description This function plots the distribution of a single parameter.
#'
#' @param df a dataframe.
#' @param param character. Name of variable of the dataframe for which the distribution should be plotted.
#' @param binwidth numeric. Determine the width of the bins to use, only applied in combination with "histogram". Default is 30 bins.
#' @param type character. Determine which plot to return: "histogram" for a histogram, "density" for a density plot. Default is "histogram".
#' @param dist character or vector of character. Determine which distribution to fit on the density plot.
#' @param user_dist character string. User-defined distribution to fit. Default value is NULL.
#' @param user_param_1 character string. First parameter of the user-defined distribution to fit.
#' @param user_param_2 character string. Second parameter of the user-defined distribution to fit.
#' @param user_mean numeric value. mean value to plot on the graph. Default is NULL
#'
#' @details The available distributions are: "norm" (normal), "beta", "gamma", "lnorm" (lognormal). TO CHECK --> ask for mean and SD/SE for the user-defined distribution???
#'
#' @return A ggplot graph.
#'
#' @examples
#' # Generating histogram using the example dataframe for the costs of progression-free health state, and bins of 50 euros.
#' data(df_pa)
#' vis_1_param(df = df_pa, param = "c_pfs", binwidth = 50)
#'
#' @export
#'
#'
vis_1_param <- function(df,
                        param = "u_pfs",
                        binwidth = NULL,
                        type = "histogram",
                        dist = c("lnorm", "norm", "beta"),
                        user_dist = NULL,
                        user_param_1 = NULL,
                        user_param_2 = NULL,
                        user_mean = NULL) {
  require(ggplot2, quietly = TRUE)
  require(fitdistrplus, quietly = TRUE)

  if("beta" %in% dist) {beta_dist <- fitdist(df[, param], distr = "beta")}
  if("gamma" %in% dist) {gamma_dist <- fitdist(df[, param], distr = "gamma")}
  if("norm" %in% dist) {norm_dist <- fitdist(df[, param], distr = "norm")}
  if("lnorm" %in% dist) {lnorm_dist <- fitdist(df[, param], distr = "lnorm")}

  df_legend <- data.frame(
    dist_call = c("user", "norm", "beta", "gamma", "lnorm"),
    col = c("black", "orange", "red", "blue", "green")
  )

  if(!is.null(user_dist)) {
    dist <- c("user", dist)
  }


  df_legend <- df_legend[which(df_legend$dist_call %in% dist),]
  df_legend <- df_legend[order(df_legend$dist_call),]

  p <- ggplot(data = df, aes_string(x = param)) +
    theme_bw()

  if(type == "histogram") {
    p_out <- p + geom_histogram(binwidth = binwidth)

  } else if(type == "density") {
    p_out <- p +
      geom_histogram(aes(y = ..density..),
                     colour = "grey",
                     fill = "lightgrey")

    if("beta" %in% dist) {
      df_beta <- data.frame(
        x = seq(from = min(df[, param]), to = max(df[, param]), by = 0.001),
        y = dbeta(seq(from = min(df[, param]), to = max(df[, param]), by = 0.001), beta_dist$estimate[[1]], beta_dist$estimate[[2]]))

      p_out <- p_out + geom_line(data = df_beta,
                                 aes(x = x,
                                     y = y,
                                     colour = "Beta")
      )
    }

    if("gamma" %in% dist) {
      df_gamma <- data.frame(
        x = seq(from = min(df[, param]), to = max(df[, param]), by = 0.001),
        y = dgamma(seq(from = min(df[, param]), to = max(df[, param]), by = 0.001), gamma_dist$estimate[[1]], gamma_dist$estimate[[2]]))

      p_out <- p_out + geom_line(data = df_gamma,
                                 aes(x = x,
                                     y = y,
                                     colour = "Gamma")
      )
    }

    if("norm" %in% dist) {
      df_norm <- data.frame(
        x = seq(from = min(df[, param]), to = max(df[, param]), by = 0.001),
        y = dnorm(seq(from = min(df[, param]), to = max(df[, param]), by = 0.001), norm_dist$estimate[[1]], norm_dist$estimate[[2]]))

      p_out <- p_out + geom_line(data = df_norm,
                                 aes(x = x,
                                     y = y,
                                     colour = "Normal")
                                 )
    }

    if("lnorm" %in% dist) {
      df_lnorm <- data.frame(
        x = seq(from = min(df[, param]), to = max(df[, param]), by = 0.001),
        y = dlnorm(seq(from = min(df[, param]), to = max(df[, param]), by = 0.001), lnorm_dist$estimate[[1]], lnorm_dist$estimate[[2]]))

      p_out <- p_out + geom_line(data = df_lnorm,
                                 aes(x = x,
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

      p_out <- p_out + geom_line(data = df_user,
                                 aes(x = x,
                                     y = y,
                                     colour = "User")
                                 )
    }

  }

  p_out <- p_out +
    scale_colour_manual(name = "Distributions",
                        values = df_legend$col) +
    theme(legend.key = element_rect(fill = "lightgrey"))

  if(!is.null(user_mean)) {
    p_out <- p_out + geom_vline(xintercept = user_mean, lty = 3)
  }

  p_out
}

#' Check range
#'
#' @description This function checks the probability that an input or output falls within a user-defined range.
#'
#' @param df a dataframe.
#' @param outcome character string. Name of variable of the dataframe for which to plot the moving average.
#' @param min_val numeric. Define the minimum value of the range.
#' @param max_val numeric. Define the maximum value of the range.
#'
#' @return A numeric.
#'
#' @examples
#' # Checking how often the "u_pfs" values falls within 0.55 and 0.72.
#' data(df_pa)
#' check_range(df = df_pa,
#'             outcome = "u_pfs",
#'             min_val = 0.55,
#'             max_val = 0.72
#'                  ))
#'
#' @export
#'
#'
check_range <- function(df,
                        outcome,
                        min_val,
                        max_val) {

  n_out <- round(length(which(df[, outcome] >= min_val &
                                df[, outcome] <= max_val)) / nrow(df) * 100, 0)

  return(n_out)
}

#' Visualise the distribution of two parameters
#'
#' @description This function plots the distribution of two parameters in a scatterplot.
#'
#' @param df a dataframe.
#' @param param_1 character. Name of variable of the dataframe to be plotted on the x-axis.
#' @param param_2 character. Name of variable of the dataframe to be plotted on the y-axis.
#' @param slope numeric. Default is NULL. If different than 0, plots a linear line with a user-defined intercept and the defined slope.
#' @param intercept numeric. Default is 0. Intercept of the user-defined slope.
#' @param check character. Default is NULL. When set to "param_2 > param_1", plots dot fulfilling the condition in red.
#' @param fit character. Designate the type of smooth model to fit to the relation of `param_1` (x) and `param_2` (y). It can take the values "lm, "glm", "gam", and "loess". A model will be fitted according to the methods described in \code{\link{[ggplot2]{method}}}.
#'
#' @return A ggplot graph.
#'
#' @examples
#' # Generating plot using the example dataframe for the costs of progression-free health state, and bins of 50 euros.
#' data(df_pa)
#' vis_1_param(df = df_pa, param = "c_pfs", binwidth = 50)
#'
vis_2_params <- function(df,
                         param_1,
                         param_2,
                         slope = NULL,
                         intercept = 0,
                         check = NULL,
                         fit = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }

  p <- ggplot2::ggplot(data = df, ggplot2::aes_string(x = param_1, y = param_2)) +
    ggplot2::theme_bw()

  if(!is.null(slope)) {
    p <- p + ggplot2::geom_abline(intercept = intercept, slope = slope, lty = 2, colour = "orange")
  }

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

  if(!is.null(fit)){
    if(fit == "lm") {
    p_out <- p_out +
      ggplot2::geom_smooth(method = "lm")
    }
    if(fit == "glm") {
      p_out <- p_out +
        ggplot2::geom_smooth(method = "glm")
    }
    if(fit == "loess") {
      p_out <- p_out +
        ggplot2::geom_smooth(method = "loess")
    }
    if(fit == "gam") {
      p_out <- p_out +
        ggplot2::geom_smooth(method = "gam")
    }
  }

  p_out
}

#' Fit distribution to parameter
#'
#' @description This function fits statistical distributions to a parameter.
#'
#' @param df a dataframe.
#' @param param character. Name of variable of the dataframe on which to fit the distributions.
#' @param dist character or vector of character. Determine which distribution to fit on the density plot.
#'
#' @details The available distributions are: "norm" (normal), "beta", "gamma", "lnorm" (lognormal). The arguments of the lists are "AIC" which contains the Akaike Information Criteria for each fitted distribution and "Dist_parameters" which contains the parameters of the fitted distributions.
#'
#' @return A list with two arguments
#'
#' @examples
#' # Fitting normal and beta distribution to the "u_pfs" variable of the example dataframe.
#' data(df_pa)
#' fit_dist(df = df_pa,
#'          param = "u_pfs",
#'          dist = c("norm", "beta"))
#'
#' @export
#'
#'
fit_dist <- function(df,
                     param,
                     dist = c("norm", "beta", "gamma", "lnorm")) {

  require(fitdistrplus)

  l_dist <- vector(mode = "list", length = length(dist))
  names(l_dist) <- dist

  l_out <- vector(mode = "list", length = 2)
  names(l_out) <- c("AIC", "Dist_parameters")

  m_stats <- matrix(NA,
                    ncol = 2,
                    nrow = length(dist),
                    dimnames = list(NULL,
                                    c("Distribution", "AIC")))
  m_stats[, "Distribution"] <- dist

  m_params <- matrix(NA,
                     ncol = 5,
                     nrow = length(dist),
                     dimnames = list(NULL,
                                     c("Distribution", "Name_param_1", "Value_param_1", "Name_param_2", "Value_param_2")))
  m_params[, "Distribution"] <- dist


  for (i in 1:length(dist)){
    l_dist[[i]] <- fitdist(df[, param], distr = dist[i])
  }

  for (i in 1:length(dist)){
    m_stats[i, "AIC"] <- round(l_dist[[i]]$aic)
  }

  for (i in 1:length(dist)){
    m_params[i, 2:ncol(m_params)] <- c(names(l_dist[[i]]$estimate)[1], round(l_dist[[i]]$estimate[1], 2), names(l_dist[[i]]$estimate)[2], round(l_dist[[i]]$estimate[2], 2))
  }

  l_out[[1]] <- as.data.frame(m_stats)
  l_out[[2]] <- as.data.frame(m_params)

  return(l_out)

}


#' Plot moving average
#'
#' @description This function plots the moving average of a outcome variable
#'
#' @param df a dataframe.
#' @param outcome character string. Name of variable of the dataframe for which to plot the moving average.
#' @param block_size numeric. Define the number of iterations at which the mean outcome has to be defined and plotted. NOT USED YET!
#' @param conv_limit numeric. Define the convergence limit, under which the relative change between block of iterations should lie.  NOT USED YET!
#'
#' @return A ggplot graph.
#'
#' @examples
#' # Checking the moving average of the incremental QALYs using the example data.
#' data(df_pa)
#' plot_convergence(df = df_pa,
#'                  outcome = "Inc_QALY"
#'                  ))
#'
plot_convergence <- function(df,
                             outcome,
                             block_size = 500,
                             conv_limit = 0.01) {
  require(ggplot2)

  l_output <- list()
  v_output <- df[, outcome]

  v_av_mov  <- unname(cumsum(v_output)/c(1:length(v_output))) # moving average
  v_blocks <- seq(from = block_size, to = length(v_av_mov), by = block_size)
  v_av_blocks <- v_av_mov[v_blocks] # average at each block

  v_rel_diff_blocks <- abs(c(v_av_blocks[1], diff(v_av_blocks))/ v_av_blocks) # Check relative difference a block and the previous

  v_conv_rel <- which(v_rel_diff_blocks < conv_limit)

  l_output <- list(v_av_mov = v_av_mov,
                   v_blocks = v_blocks,
                   v_av_blocks = v_av_blocks,
                   v_rel_diff_blocks = v_rel_diff_blocks,
                   v_conv_rel = v_conv_rel)

  # Dataframe for plotting the results
  df_plot <- data.frame(
    Iterations = c(1:length(l_output$v_av_mov)),
    Res = l_output$v_av_mov
  )
  names(df_plot)[2] <- outcome

  # Determine breaks for plot
  v_breaks <- vector()
  v_breaks[1] <- block_size
  for (i in 2:length(l_output$v_av_mov)) {
    v_breaks[i] <- v_breaks[i - 1] * 3

    if(v_breaks[i] >= length(l_output$v_av_mov)) {
      break
    }
  }
  v_breaks[length(v_breaks)] <- length(l_output$v_av_mov)

  # Plot
  p_out <- ggplot(data = df_plot, aes_string(x = "log(Iterations)", y = outcome)) +
    xlab("Iterations (log scale)") +
    scale_x_continuous(breaks = log(v_breaks),
                       labels = v_breaks) +
    geom_line() +
    theme_bw()


  return(p_out)
}

#' Check sum probabilities
#'
#' @description This function checks whether the sum of user-defined probabilities is below or equal to 1
#'
#' @param ... character vector. This character vector contains the name of the variables of which the sum will be checked.
#' @param df a dataframe.
#' @param digits numeric. Define the number of digits at which the sum of probabilities is rounded
#' @param check logical. Define which check to perform."lower" checks whether the sum of the selected variables is lower than or equal to 1 for each iteration. "equal" checks whether the sum of the selected variables is equal to 1 for each iteration. Default is "lower".
#' @param max_view numeric. Determines the number of iterations to display which do not fulfil the check. Default is 100.
#'
#' @return A text.
#'
#' @examples
#' # Checking whether the sum of the two probabilities is lower than or equal to 1
#' check_sum_probs("p_pfspd", "p_pfsd", df = df_pa, check = "lower")
#'
#' # Checking the sum of the two probabilities equals 1 using a vector to select them,
#' # Rounding off to two digits, and extending the number of iterations to display to 250.
#' check_sum_probs(c("p_pfspd", "p_pfsd"), df = df_pa, digits = 2, check = "equal", max_view = 250)
#'
check_sum_probs <- function(..., df, digits = NULL, check = "lower", max_view = 100){

  l_vars <- list(...)
  v_vars <- unlist(l_vars, use.names = FALSE)
  v_calc <- rowSums(df[, as.character(v_vars)])

  if(!is.null(digits)) {
    v_calc <- v_calc <- round(v_calc, digits)
  }

  if(check == "lower") {

    v_id_higher <- which(v_calc > 1)
    if(length(v_id_higher) > 0) {
      return(paste("The sum of probabilities is higher than 1 in the following iterations:", paste(v_id_higher[1:max_view], collapse = ", ")))
    } else {
      return("The sum of probabilities in all iterations is lower or equal to 1")
    }

  } else if(check == "equal") {

    v_id_diff <- which(v_calc != 1)
    if(length(v_id_diff) > 0) {
      return(paste("The sum of probabilities is different than 1 in the following iterations:", paste(v_id_diff[1:max_view], collapse = ", ")))
    } else {
      return("The sum of probabilities in all iterations is equal to 1")
    }
  }

}


#' Check whether variable is strictly positive
#'
#' @description This function checks whether variables are strictly positive (for instance for costs and relative risks inputs)
#'
#' @param ... character vector. This character vector contains the name of the variables of which the sum will be checked.
#' @param df a dataframe.
#' @param max_view numeric. Determines the number of iterations to display which do not fulfil the check. Default is 50.
#'
#' @return A dataframe.
#'
#' @examples
#' # Checking whether a variable is strictly positive
#' check_positive("c_pfs", df = df_pa)
#'
#' # Checking whether two variables are strictly positive
#' # Descreasing the number of iterations to display to 20.
#' check_positive("c_pfs", "c_pd", df = df_pa)
#'
check_positive <- function(..., df, max_view = 50){

  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop(
      "Package \"stringi\" must be installed to use this function.",
      call. = FALSE
    )
  }
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

  df_res <- data.frame(
    Input = v_vars,
    Negative_values = v_neg
  )
  return(df_res)
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
#'
check_binary <- function(..., df, max_view = 50){

  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop(
      "Package \"stringi\" must be installed to use this function.",
      call. = FALSE
    )
  }

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
