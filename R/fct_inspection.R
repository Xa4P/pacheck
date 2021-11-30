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
#' # Generating summary data of all inputs
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
#' @description This function helps visualise the distribution of a single parameter
#'
#' @param df a dataframe.
#' @param param character. Name of variable of the dataframe for which the distribution should be plotted.
#' @param binwidth numeric. Determine the width of the bins to use, only applied in combination with "histogram". Default is 30 bins.
#' @param type character. Determine which plot to return: "histogram" for a histogram, "density" for a density plot. Default is "histogram".
#'
#' @return A ggplot graph.
#'
#' @examples
#' # Generating plot using the default data for the costs of progression-free health state, and bins of 50 euros.
#' data(df_pa)
#' vis_1_param(df = df_pa, param = "c_pfs", binwidth = 50)
#'
#' @export
#'
#'
# Visualisation of 1 parameter histogram
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
