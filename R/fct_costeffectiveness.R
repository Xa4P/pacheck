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
#' # Generating plot using the example dataframe, and a willlingness-to-pay threshold of 80,0000 euros.
#' data(df_pa)
#' plot_ice(df = df_pa,
#'          param_1 = "Inc_QALY",
#'          param_2 = "Inc_Costs")
#'
plot_ice <- function(df,
                     param_1,
                     param_2,
                     wtp = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop(
      "Package \"scales\" must be installed to use this function.",
      call. = FALSE
    )
  }

  p_out <- ggplot2::ggplot(data = df, ggplot2::aes_string(x = param_1, y = param_2)) +
    ggplot2::geom_point(shape = 1, colour = "grey") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::xlab ("Incremental Effects") +
    ggplot2::ylab("Incremental Costs") +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = "\u20ac ", suffix = "")) +
    ggplot2::theme_bw()


  if(!is.null(wtp)) {p_out <- p_out + ggplot2::geom_abline(intercept = 0, slope = wtp, lty = 2, colour = "blue")}

  p_out
}



#' Summary statistics of the incremental cost-effectiveness plane.
#'
#' @description This function computes the probability that the probabilistic outcome is in each of the quadrant.
#'
#' @param df a dataframe.
#' @param inc_e character string. Name of variable containing the incremental effects.
#' @param inc_c character string. Name of variable containing the incremental costs
#'
#' @return A dataframe.
#'
#' @examples
#' # Generating statistics of the incremental cost-effectiveness plane using the example data.
#' data(df_pa)
#' plot_ice(df = df_pa,
#'          inc_e = "Inc_QALY",
#'          inc_c = "Inc_Costs")
#'
#' @export
#'
#'
summary_ice <- function(df,
                        inc_e,
                        inc_c) {

  df_out <- data.frame(
    Quadrant = c("NorthEast (more effective, more expensive)", "SouthEast (more effective, less expensive)",
                 "NorthWest (less effective, more expensive)", "SouthWest (less effective, less expensive)"),
    Percentage = c(paste(round(length(which(df[, inc_e] > 0 & df[, inc_c] > 0)) / nrow(df) * 100, 0), "%", sep = ""),
                   paste(round(length(which(df[, inc_e] > 0 & df[, inc_c] < 0)) / nrow(df) * 100, 0), "%", sep = ""),
                   paste(round(length(which(df[, inc_e] < 0 & df[, inc_c] > 0)) / nrow(df) * 100, 0), "%", sep = ""),
                   paste(round(length(which(df[, inc_e] < 0 & df[, inc_c] < 0)) / nrow(df) * 100, 0), "%", sep = "")
    )
  )
    return(df_out)

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
#' @return A dataframe.
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
  df_res <- as.data.frame(m_res)

  return(df_res)
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

  df_graph <- melt(data = df, id.vars = wtp)

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
