#' Plotting the incremental cost-effectiveness plane.
#'
#' @description This function plots the incremental cost-effectiveness plane.
#'
#' @param df a dataframe.
#' @param e_int character. Name of variable of the dataframe containing total effects of the intervention strategy.
#' @param e_comp character. Name of variable of the dataframe containing total effects of the comparator strategy.
#' @param c_int character. Name of variable of the dataframe containing total costs of the intervention strategy.
#' @param c_comp character. Name of variable of the dataframe containing total costs of the comparator strategy.
#' @param col character. Name of variable of the dataframe to use to colour (in blue) the plotted dots. Default is NULL which results in grey dots.
#' @param n_it (vector of) numeric value(s). Designate which iteration should be coloured in the colour red.
#' @param wtp numeric. Default is NULL. If different than NULL, plots a linear line with intercept 0 and the defined slope.
#'
#' @return A ggplot graph.
#'
#'
#' @examples
#' # Generating plot using the example dataframe, and a willlingness-to-pay threshold of 80,0000 euros.
#' data(df_pa)
#' plot_ice(df = df_pa,
#'          e_int = "t_qaly_d_int",
#'          e_comp = "t_qaly_d_comp",
#'          c_int = "t_costs_d_int",
#'          c_comp = "t_costs_d_comp",
#'          wtp = 8000)
#'
#' @import ggplot2
#' @import scales
#' @export
#'
plot_ice <- function(df,
                     e_int,
                     e_comp,
                     c_int,
                     c_comp,
                     col = NULL,
                     n_it = NULL,
                     wtp = NULL) {

  df$inc_costs   <- df[, c_int] - df[, c_comp]
  df$inc_effects <- df[, e_int] - df[, e_comp]

  p_out <- if(is.null(col)){
    ggplot2::ggplot(data = df, ggplot2::aes_string(x = "inc_effects", y = "inc_costs")) +
      ggplot2::geom_point(shape = 1, colour = "grey") +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::geom_vline(xintercept = 0) +
      ggplot2::xlab ("Incremental effects") +
      ggplot2::ylab("Incremental costs") +
      ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = "\u20ac ", suffix = "")) +
      ggplot2::geom_point(data = df[n_it, ], ggplot2::aes_string(x = "inc_effects", y = "inc_costs"), colour = "red", shape = 1) +
      ggplot2::theme_bw()
    } else {
        ggplot2::ggplot(data = df, ggplot2::aes_string(x = "inc_effects", y = "inc_costs", colour = col)) +
          ggplot2::geom_point(shape = 1) +
          ggplot2::geom_hline(yintercept = 0) +
          ggplot2::geom_vline(xintercept = 0) +
          ggplot2::xlab ("Incremental effects") +
          ggplot2::ylab("Incremental costs") +
          ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = "\u20ac ", suffix = "")) +
          ggplot2::geom_point(data = df[n_it, ], ggplot2::aes_string(x = "inc_effects", y = "inc_costs"), colour = "red", shape = 1) +
          ggplot2::theme_bw()
      }

  if(!is.null(wtp)) {p_out <- p_out + ggplot2::geom_abline(intercept = 0, slope = wtp, lty = 2, colour = "orange")}

  p_out
}

#' Plot cost-effectiveness plane.
#'
#' @description This function plots the cost-effectiveness plane.
#'
#' @param df a dataframe.
#' @param e_int character. Name of variable of the dataframe containing total effects of the intervention strategy.
#' @param e_comp character. Name of variable of the dataframe containing total effects of the comparator strategy.
#' @param c_int character. Name of variable of the dataframe containing total costs of the intervention strategy.
#' @param c_comp character. Name of variable of the dataframe containing total costs of the comparator strategy.
#'
#' @return A ggplot graph.
#'
#' @examples
#' # Plot cost effectiveness plane
#' data("df_pa")
#' plot_ce(df_pa,
#'         e_int = "t_qaly_d_int",
#'         e_comp = "t_qaly_d_comp",
#'         c_int = "t_costs_d_int",
#'         c_comp = "t_costs_d_comp"
#'         )
#'
#' @import ggplot2
#' @import scales
#' @export
#'
plot_ce <- function (df, e_int, e_comp, c_int, c_comp){
  ggplot2::ggplot(data = df, ggplot2::aes_string(x = e_int, y = c_int, colour = factor("Intervention"))) +
    ggplot2::geom_point(shape = 1) +
    ggplot2::stat_ellipse(data = df, ggplot2::aes_string(x = e_int, y = c_int, colour = factor("Mean intervention"))) +
    ggplot2::geom_point(data = df, ggplot2::aes_string(x = mean(df[, e_int]), y = mean(df[, c_int]), colour = factor("Mean intervention")), shape = 2) +
    ggplot2::geom_point(data = df, ggplot2::aes_string(x = e_comp, y = c_comp, colour = factor("Comparator")), shape = 1) +
    ggplot2::stat_ellipse(data = df, ggplot2::aes_string(x = e_comp, y = c_comp, colour = factor("Mean comparator"))) +
    ggplot2::geom_point(data = df, ggplot2::aes_string(x = mean(df[, e_comp]), y = mean(df[, c_comp]), colour = factor("Mean comparator")), shape = 2) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::xlab ("Total effects") +
    ggplot2::ylab("Total costs") +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = "\u20ac ", suffix = "")) +
    ggplot2::scale_colour_manual(name = "Strategy",
                                 # labels = c("Intervention", "Comparator", "Mean intervention", "Mean comparator"),
                                 # breaks = c(1, 2, 3, 4),
                                 values = c(Intervention = "grey",
                                            Comparator = "orange",
                                            `Mean intervention` = "black",
                                            `Mean comparator` = "blue")
    ) +
    ggplot2::theme_bw()
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
#'          inc_e = "inc_qaly",
#'          inc_c = "inc_costs")
#'
#' @export
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
#' @return A dataframe with three columns: "WTP_threshold", "Prob_int", "Prob_comp".
#' "WTP_threshold" contains the willingness-to-pay thresholds at which the probability of cost effectiveness has been calculated for both strategies.
#' "Prob_int" contains the probability that the intervention strategy is cost effective at a given willingness-to-pay threshold.
#' "Prob_comp" contains the probability that the comparator strategy is cost effective at a given willingness-to-pay threshold.
#'
#' @examples
#' # Calculate probabilities of cost effectiveness using the example dataframe, for willlingness-to-pay thresholds of 0 to 50,0000 euros.
#' data("df_pa")
#'calculate_ceac(df_pa,
#'               e_int = "t_qaly_d_int",
#'               e_comp = "t_qaly_d_comp",
#'               c_int = "t_costs_d_int",
#'               c_comp = "t_costs_d_comp",
#'               v_wtp = seq(from = 0, to = 50000, by = 1000))
#'
#' @export
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
#' @param wtp character Name of variable of the dataframe containing the willingness-to-pay thresholds at which the probability of cost effectiveness have been defined.
#'
#' @return A ggplot graph.
#'
#' @examples
#' # Plot CEAC based on results from calculate_ceac()
#' data("df_pa")
#' m_res_ceac <- calculate_ceac(df_pa,
#'                              e_int = "t_qaly_d_int",
#'                              e_comp = "t_qaly_d_comp",
#'                              c_int = "t_costs_d_int",
#'                              c_comp = "t_costs_d_comp")
#' df_ceac_p <- as.data.frame(m_res_ceac)
#'
#' plot_ceac(df = df_ceac_p,
#'           v_wtp = "WTP_threshold")
#'
#' @export
#'
plot_ceac <- function(df,
                      wtp) {
  require(ggplot2, quietly = TRUE)
  require(reshape2, quietly = TRUE)
  require(scales, quietly = TRUE)

  df_graph <- reshape2::melt(data = df, id.vars = wtp)

  p_out <- ggplot2::ggplot(data = df_graph, ggplot2::aes_string(x = wtp, y = "value", colour = "variable")) +
    ggplot2::geom_line() +
    ggplot2::xlab ("Willingness-to-pay threshold") +
    ggplot2::ylab("Probability of cost effectiveness") +
    ggplot2::scale_x_continuous(labels = scales::dollar_format(prefix = "\u20ac ", suffix = "")) +
    ggplot2::scale_colour_manual(name = "Strategy",
                        values = c(Prob_int = "grey", Prob_comp = "orange")) +
    ggplot2::theme_bw()

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
#'              e_int = "t_qaly_d_int",
#'              e_comp = "t_qaly_d_comp",
#'              c_int = "t_costs_d_int",
#'              c_comp = "t_costs_d_comp",
#'              wtp = 80000)
#'
#' @export
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

#' Plot (i)NMB or (i)NHB.
#'
#' @description This function plots the Net Monetary Benefits (NMB) and Net Health Benefits (NHB) for each strategy and the incremental NMB and NHB.
#'
#' @param df a dataframe. Output of `calculate_nb()`
#' @param NMB logical. Should the (i)NMBs be plotted? Default is TRUE, if FALSE, (i)NHBs are plotted.
#' @param comparators logical. Should the NMB/NHB for each comparator be plotted? Default is TRUE.
#' @param incremental logical. Should the incremental NMB/NHB be plotted? Default is FALSE
#'
#' @return A ggplot graph.
#'
#' @examples
#' # Calculate NB's at a willingness-to-pay threshold of 80000 per unit of effects
#' data("df_pa")
#' df_nmb <- calculate_nb(df_pa,
#'              e_int = "t_qaly_d_int",
#'              e_comp = "t_qaly_d_comp",
#'              c_int = "t_costs_d_int",
#'              c_comp = "t_costs_d_comp",
#'              wtp = 80000)
#'
#' # Plot NMB's for each comparator
#' plot_nmb(df = df_nmb,
#'          NMB = TRUE,
#'          comparators = TRUE)
#'
#' @import ggplot2
#' @export
#'
plot_nb <- function(df,
                    NMB = TRUE,
                    comparators = TRUE,
                    incremental = FALSE) {
  p <- ggplot2::ggplot() +
    ggplot2::theme_bw()

  if(NMB == TRUE){
    if(comparators == TRUE){
      p <- p + ggplot2::geom_density(data = df, ggplot2::aes_string(x = "NMB_int", colour = factor("Intervention"))) +
        ggplot2::geom_density(data = df, ggplot2::aes_string(x = "NMB_comp", colour = factor("Comparator"))) +
        ggplot2::xlab("Net monetary benefit")
    }
    if(incremental == TRUE) {
      p <- p + ggplot2::geom_density(data = df, ggplot2::aes_string(x = "iNMB", colour = factor("Incremental")))
    }
    p <- p + ggplot2::scale_colour_manual("Strategy",
                                          values = c(Intervention = "grey",
                                                     Comparator = "orange",
                                                     Incremental = "black"
                                          )
    )
  } else {
    if(comparators == TRUE){
    p <- p + ggplot2::geom_density(data = df, ggplot2::aes_string(x = "NHB_int", colour = factor("Intervention"))) +
      ggplot2::geom_density(data = df, ggplot2::aes_string(x = "NHB_comp", colour = factor("Comparator"))) +
      ggplot2::xlab("Net health benefit")
    }
    if(incremental == TRUE) {
      p <- p + ggplot2::geom_density(data = df, ggplot2::aes_string(x = "iNHB", colour = factor("Incremental")))
    }
    p <- p + ggplot2::scale_colour_manual("Strategy",
                                          values = c(Intervention = "grey",
                                                     Comparator = "orange",
                                                     Incremental = "black"
                                          )
    )
  }
  p
}
