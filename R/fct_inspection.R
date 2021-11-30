#' Generate summary statistics
#'
#' @description This function generates summary statistics of input and output values of a probabilistic analysis
#'
#' @param df A dataframe.
#' @param v_params character or vector of character. Vector of names of the inputs and outputs for which to return summary statistics. Default is "ALL" which returns summary values for all inputs and outputs in the dataframe.
#'
#' @return A dataframe with summary data for selected inputs and outputs.
#'
#' @examples
#' # Generating summary data of all inputs using the example dataframe
#' data(df_pa)
#' df_summary <- generate_sum_stats(df_pa)
#'
#' @export
#'
#'

generate_sum_stats <- function(df,
                               v_params = "ALL"){ # doesn't work with 1 parameter!

  df <- if(v_params == "ALL") {
    df
  } else {
    data.frame(df[, v_params])
  }

  df_out <- data.frame(Parameter = if(length(v_params) == 1){v_params} else{ colnames(df) },
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

#' Visualise the distribution of a single parameter
#'
#' @description This function plots the distribution of a single parameter.
#'
#' @param df a dataframe.
#' @param param character. Name of variable of the dataframe for which the distribution should be plotted.
#' @param binwidth numeric. Determine the width of the bins to use, only applied in combination with "histogram". Default is 30 bins.
#' @param type character. Determine which plot to return: "histogram" for a histogram, "density" for a density plot. Default is "histogram".
#'
#' @return A ggplot graph.
#'
#' @examples
#' # Generating plot using the example dataframe for the costs of progression-free health state, and bins of 50 euros.
#' data(df_pa)
#' vis_1_param(df = df_pa, param = "c_pfs", binwidth = 50)
#'
#' @export
#'
#'
vis_1_param <- function(df,
                        param,
                        binwidth = NULL,
                        type = "histogram") {
  require(ggplot2)

  p <- ggplot(data = df, aes_string(x = param)) +
    theme_bw()

  if(type == "histogram") {
    p_out <- p + geom_histogram(binwidth = binwidth)
  } else if(type == "density") {
    p_out <- p + geom_density()
  }

  p_out
}


#' Visualise the distribution of a two parameters
#'
#' @description This function plots the distribution of two parameters in a scatterplot.
#'
#' @param df a dataframe.
#' @param param_1 character. Name of variable of the dataframe to be plotted on the x-axis.
#' @param param_2 character. Name of variable of the dataframe to be plotted on the y-axis.
#' @param slope numeric. Default is NULL. If different than 0, plots a linear line with intercept 0 and the defined slope.
#' @param check character. Default is NULL. When set to "param_2 > param_1", plots dot fulfilling the condition in red.
#'
#' @return A ggplot graph.
#'
#' @examples
#' # Generating plot using the example dataframe for the costs of progression-free health state, and bins of 50 euros.
#' data(df_pa)
#' vis_1_param(df = df_pa, param = "c_pfs", binwidth = 50)
#'
#' @export
#'
#'
vis_2_params <- function(df,
                         param_1,
                         param_2,
                         slope = NULL,
                         check = NULL) {
  require(ggplot2)

  p <- ggplot(data = df, aes_string(x = param_1, y = param_2)) +
    theme_bw()

  if(!is.null(slope)) {
    p <- p + geom_abline(intercept = 0, slope = slope, lty = 2, colour = "orange")
  }

  if(!is.null(check)) {

    if(check == "param_2 > param_1"){

      df$col <- ifelse(df[, param_2] > df[, param_1], "TRUE", "FALSE")
      df$col <- as.factor(df$col)

      p_true <- round(length(which(df$col == "TRUE"))/ length(df$col) * 100, 0)

      p_out <- p +
        geom_point(data = df, aes_string(x = param_1, y = param_2, colour = "col"), shape = 1) +
        scale_colour_manual(name = "Check",
                            values = c("TRUE" = "Red",
                                       "FALSE" = "grey"
                            )) +
        annotate("text", label = print(paste("P(TRUE):", p_true, "%")), x = min(df[, param_1]) + (max(df[, param_1]) - min(df[, param_1])) * 0.2,  y = max(df[, param_2]))

    }

  } else {
    p_out <- p + geom_point(shape = 1, colour = "grey")
  }

  p_out
}

#' Incremental cost-effectiveness plot
#'
#' @description This function plots the incremental cost-effectiveness plot.
#'
#' @param df a dataframe.
#' @param param_1 character. Name of variable of the dataframe to be plotted on the x-axis.
#' @param param_2 character. Name of variable of the dataframe to be plotted on the y-axis.
#' @param wtp numeric. Default is NULL. If different than NULL, plots a linear line with intercept 0 and the defined slope.
#'
#' @return A ggplot graph.
#'
#' @examples
#' # Generating plot using the example dataframe, and a willlingness-to-pay threshold of 80,0000 euros.
#' data(df_pa)
#' plot_ice(df = df_pa, param_1 = "Inc_QALY", param_1 = "Inc_Costs")
#'
#' @export
#'
#'
plot_ice <- function(df,
                     param_1,
                     param_2,
                     wtp = NULL) {
  require(ggplot2)

  p_out <- ggplot(data = df, aes_string(x = param_1, y = param_2)) +
    geom_point(shape = 1, colour = "grey") +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    xlab ("Incremental Effects") +
    ylab("Incremental Costs") +
    theme_bw()


  if(!is.null(wtp)) {p_out <- p_out + geom_abline(intercept = 0, slope = wtp, lty = 2, colour = "blue")}

  p_out
}
