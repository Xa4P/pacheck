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

#' Plotting the incremental cost-effectiveness plane.
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
#' Generating plot using the example dataframe, and a willlingness-to-pay threshold of 80,0000 euros.
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
  require(scales)

  p_out <- ggplot(data = df, aes_string(x = param_1, y = param_2)) +
    geom_point(shape = 1, colour = "grey") +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    xlab ("Incremental Effects") +
    ylab("Incremental Costs") +
    scale_y_continuous(labels = dollar_format(prefix = "\u20ac ", suffix = "")) +
    theme_bw()


  if(!is.null(wtp)) {p_out <- p_out + geom_abline(intercept = 0, slope = wtp, lty = 2, colour = "blue")}

  p_out
}

#' Calculate cost-effectiveness probabilities.
#'
#' @description This function calculates the probabilities that each strategy is the most cost effective.
#'
#' @param df a dataframe.
#' @param e_int character. Name of variable of the dataframe containing total effects of the intervention strategy.
#' @param e_comp character. Name of variable of the dataframe containing total effects of the comparator strategy.
#' @param c_int character. Name of variable of the dataframe containing total costs of the intervention strategy.
#' @param c_comp character. Name of variable of the dataframe containing total costs of the comparator strategy.
#' @param v_wtp vector of numerical values. Vectors of willingness-to-pay threshold for which the probabilities of cost effectiveness have to be defined. Default is 0:100,000 by increments of 1,000.
#'
#' @return A ggplot graph.
#'
#' @examples
#' # Calculate probabilities of cost effectiveness using the example dataframe, for willlingness-to-pay thresholds of 0 to 50,0000 euros.
#' data("df_pa")
#'calculate_ceac(df_pa,
#'               e_int = "QALY_int",
#'               e_comp = "QALY_comp",
#'               c_int = "Costs_int",
#'               c_comp = "Costs_comp",
#'               v_wtp = seq(from = 0, to = 50000, by = 1000))
#'
#' @export
#'
#'
calculate_ceac <- function (df, e_int, e_comp, c_int, c_comp, v_wtp = seq(from = 0, to = 100000, by = 1000)){

  m_res <- matrix(NA, nrow = length(v_wtp), ncol = 3,
                dimnames = list(v_wtp, c("WTP_threshold", "Prob_int", "Prob_comp")))
  m_tmp <- matrix(NA, nrow = length(df[, e_int]), ncol = 2, #temp stores NB for each iteration
                dimnames = list(c(1:length(df[, e_int])),c("res_int", "res_comp")))

  for (i in 1:length(v_wtp)){
    m_tmp[,"res_int"]   <- df[, e_int] * v_wtp[i] - df[, c_int]   #compute NB for intervention at wtp threshold
    m_tmp[,"res_comp"]  <- df[, e_comp] * v_wtp[i] - df[, c_comp] #compute NB for comparator at wtp threshold

    num_CE <- length(which(m_tmp[, "res_int"] > m_tmp[, "res_comp"])) #number of iterations in which intervention is cost effective
    p_trt  <- num_CE/length(df[, e_int]) #probability trt has a higher NB than comp

    m_res[i,] <- cbind(v_wtp[i], p_trt, 1 - p_trt) #vector of wtp and probabilities is stored
  }

  rownames(m_res) <- NULL

  return(m_res)
}

#' Plot cost-effectiveness acceptability curves.
#'
#' @description This function calculates the probabilities that each strategy is the most cost effective.
#'
#' @param df a dataframe.
#' @param wtp vector of numerical values. Name of variable of the dataframe containing the willingness-to-pay thresholds at which the probability of cost effectiveness have been defined.
#'
#' @return A ggplot graph.
#'
#' @examples
#' # Plot CEAC based on results from calculate_ceac()
#' data("df_pa")
#' m_res_ceac <- calculate_ceac(df_pa,
#'               e_int = "QALY_int",
#'               e_comp = "QALY_comp",
#'               c_int = "Costs_int",
#'               c_comp = "Costs_comp")
#' df_ceac_p <- as.data.frame(m_res_ceac)
#'
#' plot_ceac(df = df_ceac_p,
#'           v_wtp = "WTP_threshold")
#'
#' @export
#'
#'
plot_ceac <- function(df,
                      wtp) {
  require(ggplot2)
  require(reshape2)
  require(scales)

  df_graph <- melt(data = df_ceac_p, id.vars = wtp)

  p_out <- ggplot(data = df_graph, aes_string(x = wtp, y = "value", colour = "variable")) +
    geom_line() +
    xlab ("Willingness-to-pay threshold") +
    ylab("Probability of cost effectiveness") +
    scale_x_continuous(labels = dollar_format(prefix = "\u20ac ", suffix = "")) +
    scale_colour_manual(name = "Strategy",
                        values = c(Prob_int = "grey", Prob_comp = "orange")) +
    theme_bw()

  p_out
}

#' Calculate NMB and NHB.
#'
#' @description This function calculates the Net Monetary Benefits (NMB) and Net Health Benefits (NHB) for each strategy and the incremental NMB and NHB.
#'
#' @param df a dataframe.
#' @param e_int character. Name of variable of the dataframe containing total effects of the intervention strategy.
#' @param e_comp character. Name of variable of the dataframe containing total effects of the comparator strategy.
#' @param c_int character. Name of variable of the dataframe containing total costs of the intervention strategy.
#' @param c_comp character. Name of variable of the dataframe containing total costs of the comparator strategy.
#' @param wtp numerical values. Willingness-to-pay thresholds to use for NMB and NHB calculations.
#'
#' @return A dataframe.
#'
#' @examples
#' # Calculate NB's at a willingness-to-pay threshold of 80000 per unit of effects
#' data("df_pa")
#' calculate_nb(df_pa,
#'              e_int = "QALY_int",
#'              e_comp = "QALY_comp",
#'              c_int = "Costs_int",
#'              c_comp = "Costs_comp",
#'              wtp = 80000)
#'
#' @export
#'
#'
calculate_nb <- function(df,
                    e_int,
                    e_comp,
                    c_int,
                    c_comp,
                    wtp) {

  df_out <- data.frame(df,
                       NMB_int = df[, e_int] * wtp - df[, c_int],
                       NMB_comp = df[, e_comp] * wtp - df[, c_comp],
                       iNMB = (df[, e_int] - df[, e_comp]) * wtp - (df[, c_int] - df[, c_comp]),
                       NHB_int = df[, e_int] - df[, c_int] / wtp,
                       NHB_comp = df[, e_comp] - df[, c_comp] / wtp,
                       iNHB = (df[, e_int] - df[, e_comp]) - (df[, c_int] - df[, c_comp]) / wtp
                       )

  return(df_out)
}
