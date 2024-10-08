#' Plotting the incremental cost-effectiveness plane.
#' @description This function plots the incremental cost-effectiveness plane.
#' @param df a dataframe.
#' @param e_int character. Name of variable of the dataframe containing total effects of the intervention strategy.
#' @param e_comp character. Name of variable of the dataframe containing total effects of the comparator strategy.
#' @param c_int character. Name of variable of the dataframe containing total costs of the intervention strategy.
#' @param c_comp character. Name of variable of the dataframe containing total costs of the comparator strategy.
#' @param col character. Name of variable of the dataframe to use to colour (in blue) the plotted dots. Default is NULL which results in grey dots.
#' @param n_it (vector of) numeric value(s). Designate which iteration should be coloured in the colour red.
#' @param wtp numeric. Default is NULL. If different than NULL, plots a linear line with intercept 0 and the defined slope.
#' @param currency character. Default is "euro". Determines the currency sign to use in the incremental cost effectiveness plane. Currently included signs: "euro", "dollar", "yen", "none".
#' @return A ggplot2 graph.
#' @examples
#' # Generating plot using the example dataframe, and a willlingness-to-pay threshold of 80,0000 euros.
#' data(df_pa)
#' plot_ice(df = df_pa,
#'          e_int = "t_qaly_d_int",
#'          e_comp = "t_qaly_d_comp",
#'          c_int = "t_costs_d_int",
#'          c_comp = "t_costs_d_comp",
#'          wtp = 8000)
#' @import ggplot2
#' @import scales
#' @import assertthat
#' @import glue
#' @export
plot_ice <- function(df,
                     e_int,
                     e_comp,
                     c_int,
                     c_comp,
                     col = NULL,
                     n_it = NULL,
                     wtp = NULL,
                     currency = "euro") {
  # Checks
  assertthat::assert_that(e_int %in% names(df), msg = glue::glue("{e_int} is not a valid column name of the dataframe."))
  assertthat::assert_that(c_int %in% names(df), msg = glue::glue("{c_int} is not a valid column name of the dataframe."))
  assertthat::assert_that(e_comp %in% names(df), msg = glue::glue("{e_comp} is not a valid column name of the dataframe."))
  assertthat::assert_that(c_comp %in% names(df), msg = glue::glue("{c_comp} is not a valid column name of the dataframe."))
  if(!is.null(col)) {
    assertthat::assert_that(col %in% names(df), msg = glue::glue("{col} is not a valid column name of the dataframe."))
  }
  if(!is.null(n_it)) {
    assertthat::assert_that(is.numeric(n_it), msg = glue::glue("'n_it' is (are) not a numeric value(s)"))
  }
  if(!currency %in% c("none", "euro", "dollar", "yen")) {
    stop("The chosen currency is not valid.")
  }

  # Determine formatting plot
  cur <- switch(currency,
                euro = "\u20ac ",
                dollar = "\u0024 ",
                yen = "\u00a5 ",
                none = "")

  # Calculate increments
  df$inc_costs   <- df[, c_int] - df[, c_comp]
  df$inc_effects <- df[, e_int] - df[, e_comp]

  # Backbone plot
  p_out <- if (is.null(col)) {
    ggplot2::ggplot(data = df, ggplot2::aes_string(x = "inc_effects", y = "inc_costs")) +
      ggplot2::geom_point(shape = 1, colour = "grey")
  } else {
    ggplot2::ggplot(data = df,
                    ggplot2::aes_string(x = "inc_effects", y = "inc_costs", colour = col)) +
      ggplot2::geom_point(shape = 1)
  }

  # Adding formatting to plot
  p_out <- p_out +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::xlab ("Incremental effects") +
    ggplot2::ylab("Incremental costs") +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = cur, suffix = "")) +
    ggplot2::geom_point(data = df[n_it, ], ggplot2::aes_string(x = "inc_effects", y = "inc_costs"), colour = "red", shape = 1) +
    ggplot2::theme_bw()

  if(!is.null(wtp)) {p_out <- p_out + ggplot2::geom_abline(intercept = 0, slope = wtp, lty = 2, colour = "orange")}

  #Export
  p_out
}

#' Plotting cost-effectiveness plane.
#' @description This function plots the cost-effectiveness plane.
#' @inheritParams plot_ice
#' @return A ggplot2 graph.
#' @examples
#' # Plot cost effectiveness plane
#' data("df_pa")
#' plot_ce(df = df_pa,
#'         e_int = "t_qaly_d_int",
#'         e_comp = "t_qaly_d_comp",
#'         c_int = "t_costs_d_int",
#'         c_comp = "t_costs_d_comp"
#'         )
#' @import ggplot2
#' @import scales
#' @import assertthat
#' @import glue
#' @export
plot_ce <- function (df,
                     e_int,
                     e_comp,
                     c_int,
                     c_comp,
                     currency = "euro") {
  # Checks
  assertthat::assert_that(e_int %in% names(df), msg = glue::glue("{e_int} is not a valid column name of the dataframe."))
  assertthat::assert_that(c_int %in% names(df), msg = glue::glue("{c_int} is not a valid column name of the dataframe."))
  assertthat::assert_that(e_comp %in% names(df), msg = glue::glue("{e_comp} is not a valid column name of the dataframe."))
  assertthat::assert_that(c_comp %in% names(df), msg = glue::glue("{c_comp} is not a valid column name of the dataframe."))
  if(!currency %in% c("none", "euro", "dollar", "yen")) {
    stop("The chosen currency is not valid.")
  }

  # Determine formatting plot
  cur <- switch(currency,
                euro = "\u20ac ",
                dollar = "\u0024 ",
                yen = "\u00a5 ",
                none = "")

  # Plot
  p_out <-
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
      ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = cur, suffix = "")) +
      ggplot2::scale_colour_manual(name = "Strategy",
                                   # labels = c("Intervention", "Comparator", "Mean intervention", "Mean comparator"),
                                   # breaks = c(1, 2, 3, 4),
                                   values = c(Intervention = "grey",
                                              Comparator = "orange",
                                              `Mean intervention` = "black",
                                              `Mean comparator` = "blue")
      ) +
      ggplot2::theme_bw()

  # Export
  p_out
}

#' Plotting cost-effectiveness plane.
#' @description This function plots the cost-effectiveness plane for an infinite amount of strategies .
#' @inheritParams plot_ice
#' @param outcomes character. Vector of variable names containing the outcomes to be plotted on the x-axis. The variable names should be structured as follows: 't_qaly_d_' followed by the name of the strategy: e.g. 't_qaly_d_intervention'.
#' @param costs character. Vector of variable names containing the costs to be plotted on the y-axis. The variable names should be structured as follows: 't_costs_d_' followed by the name of the strategy: e.g. 't_costs_d_intervention'.
#' @param ellipse logical. Determines whether plot should plot the dots of each iteration (default, ellipse = FALSE), or whether the mean outcomes and costs and their 95procent confidence ellipses should be plotted (TRUE).
#' @return A ggplot2 graph.
#' @examples
#' # Plot cost effectiveness plane as ellipses
#' data("df_pa")
#' df_pa$t_qaly_d_int2 <- df_pa$t_qaly_d_int * 1.5 # creating additional outcome variable
#' df_pa$t_costs_d_int2 <- df_pa$t_costs_d_int * 1.5 # creating additional costs variable
#' plot_ce_mult(df = df_pa,
#'              outcomes = c("t_qaly_d_int", "t_qaly_d_comp", "t_qaly_d_int2"),
#'              costs = c("t_costs_d_int","t_costs_d_comp", "t_costs_d_int2"),
#'              ellipse = TRUE)
#' @import ggplot2
#' @import scales
#' @import assertthat
#' @import glue
#' @import dplyr
#' @import tidyr
#' @export
plot_ce_mult <- function(df,
                         outcomes,
                         costs,
                         ellipse = FALSE,
                         currency = "euro") {
  # Checks
  assertthat::assert_that(all(outcomes %in% names(df)), msg = glue::glue("At least one variable in vector 'outcomes' is not a valid column name of 'df'."))
  assertthat::assert_that(all(costs %in% names(df)), msg = glue::glue("At least one variable in vector 'costs' is not a valid column name of 'df'."))
  assertthat::assert_that(length(grep("^t_qaly_d_", outcomes)) == length(outcomes),
                          msg = "At least one element of 'outcomes' does not start with 't_qaly_d_'")
  assertthat::assert_that(length(grep("^t_costs_d_", costs)) == length(costs),
                          msg = "At least one element of 'costs' does not start with 't_costs_d_'")
  assertthat::assert_that(length(costs) == length(outcomes), msg = glue::glue("The length of vector of names 'outcomes' is different than length of vercor of names 'costs'."))
  assertthat::assert_that(identical(
    gsub("t_qaly_d_", "", outcomes)[order(gsub("t_qaly_d_", "", outcomes))],
    gsub("t_costs_d_", "", costs)[order(gsub("t_costs_d_",  "", costs))]
  ),
  msg = "The included strategies are different in 'outcomes' and 'costs'. Ensures that the name of the strategies are the same in both argument.")

  if(!currency %in% c("none", "euro", "dollar", "yen")) {
    stop("The chosen currency is not valid.")
  }

  # Determine formatting plot
  outcomes <- outcomes[order(gsub("t_qaly_d_", "", outcomes))]
  costs    <- costs[order(gsub("t_costs_d_",  "", costs))]
  cur <- switch(currency,
                euro = "\u20ac ",
                dollar = "\u0024 ",
                yen = "\u00a5 ",
                none = "")

  # Modify data for plot
  df_plot <- df |>
    dplyr::select(c(outcomes,
                    costs)) |>
    dplyr::rename_all( ~ gsub("t_qaly_d", "QALY", .)) |>
    dplyr::rename_all( ~ gsub("t_costs_d", "Costs", .)) |>
    dplyr::mutate(Iteration = 1:nrow(df)) |>
    tidyr::pivot_longer(!Iteration,
                        names_to = c("Outcome", "Strategy"),
                        names_sep = "_") |>
    tidyr::pivot_wider(
      names_from = Outcome ,
      values_from = value
    )
  df_plot_mean <- df_plot |>
    dplyr::group_by(Strategy) |>
    dplyr::summarise(QALY = mean(QALY),
                     Costs = mean(Costs))

  # Plot
  p_out <- ggplot2::ggplot(data = df_plot, ggplot2::aes(x = QALY, y = Costs, colour = Strategy)) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = cur, suffix = "")) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::xlab ("Total effects") +
    ggplot2::ylab("Total costs") +
    ggplot2::theme_bw()

  if(ellipse == TRUE) {
    p_out <- p_out+
      ggplot2::stat_ellipse() +
      ggplot2::geom_point(data = df_plot_mean, ggplot2::aes(x = QALY, y = Costs, colour = Strategy))
  } else {
    p_out <- p_out+
      ggplot2::geom_point()
  }

  # p_out <- p_out +
  #   ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = cur, suffix = "")) +
  #   ggplot2::geom_hline(yintercept = 0) +
  #   ggplot2::geom_vline(xintercept = 0) +
  #   ggplot2::xlab ("Total effects") +
  #   ggplot2::ylab("Total costs") +
  #   ggplot2::theme_bw()

  # Export
  p_out
}

#' Summary statistics of the incremental cost-effectiveness plane.
#' @description This function computes the probability that the probabilistic outcome is in each of the quadrants.
#' @inheritParams plot_ice
#' @return A dataframe.
#' @examples
#' # Generating statistics of the incremental cost-effectiveness plane using the example data.
#' data(df_pa)
#' summary_ice(df = df_pa,
#'             e_int = "t_qaly_d_int",
#'             e_comp = "t_qaly_d_comp",
#'             c_int = "t_costs_d_int",
#'             c_comp = "t_costs_d_comp"
#'             )
#' @import assertthat
#' @import glue
#' @export
summary_ice <- function(df,
                        e_int,
                        e_comp,
                        c_int,
                        c_comp) {
  # Checks
  assertthat::assert_that(e_int %in% names(df), msg = glue::glue("{e_int} is not a valid column name of the dataframe."))
  assertthat::assert_that(c_int %in% names(df), msg = glue::glue("{c_int} is not a valid column name of the dataframe."))
  assertthat::assert_that(e_comp %in% names(df), msg = glue::glue("{e_comp} is not a valid column name of the dataframe."))
  assertthat::assert_that(c_comp %in% names(df), msg = glue::glue("{c_comp} is not a valid column name of the dataframe."))

  # Calculate incrementals
  df$inc_costs   <- df[, c_int] - df[, c_comp]
  df$inc_effects <- df[, e_int] - df[, e_comp]

  # Table outcomes
  df_out <- data.frame(
    Quadrant = c("NorthEast (more effective, more expensive)", "SouthEast (more effective, less expensive)",
                 "NorthWest (less effective, more expensive)", "SouthWest (less effective, less expensive)"),
    Percentage = c(paste(round(length(which(df[, "inc_effects"] > 0 & df[, "inc_costs"] > 0)) / nrow(df) * 100, 0), "%", sep = ""),
                   paste(round(length(which(df[, "inc_effects"] > 0 & df[, "inc_costs"] < 0)) / nrow(df) * 100, 0), "%", sep = ""),
                   paste(round(length(which(df[, "inc_effects"] < 0 & df[, "inc_costs"] > 0)) / nrow(df) * 100, 0), "%", sep = ""),
                   paste(round(length(which(df[, "inc_effects"] < 0 & df[, "inc_costs"] < 0)) / nrow(df) * 100, 0), "%", sep = "")
                   )
    )

  # Export
  return(df_out)
  }

#' Calculate cost-effectiveness probabilities.
#' @description This function calculates the probabilities that each strategy is the most cost effective.
#' @inheritParams plot_ice
#' @param v_wtp vector of numerical values. Vector of willingness-to-pay threshold for which the probabilities of cost effectiveness have to be defined. Default is 0:100,000 by increments of 1,000.
#' @return A dataframe with three columns:
#' \itemize{
#'    \item WTP_threshold = The willingness-to-pay thresholds at which the probability of cost effectiveness has been calculated for both strategies
#'    \item Prob_int = The probability that the intervention strategy is cost effective at a given willingness-to-pay threshold
#'    \item Prob_comp = The probability that the comparator strategy is cost effective at a given willingness-to-pay threshold
#'    }
#' @examples
#' # Calculate probabilities of cost effectiveness using the example dataframe,
#' # for willlingness-to-pay thresholds of 0 to 50,0000 euros.
#' data("df_pa")
#' calculate_ceac(df = df_pa,
#'                e_int = "t_qaly_d_int",
#'                e_comp = "t_qaly_d_comp",
#'                c_int = "t_costs_d_int",
#'                c_comp = "t_costs_d_comp",
#'                v_wtp = seq(from = 0, to = 50000, by = 1000))
#' @import assertthat
#' @import glue
#' @export
calculate_ceac <- function (df,
                            e_int,
                            e_comp,
                            c_int,
                            c_comp,
                            v_wtp = seq(from = 0, to = 100000, by = 1000)){
  # Checks
  assertthat::assert_that(e_int %in% names(df), msg = glue::glue("{e_int} is not a valid column name of the dataframe."))
  assertthat::assert_that(c_int %in% names(df), msg = glue::glue("{c_int} is not a valid column name of the dataframe."))
  assertthat::assert_that(e_comp %in% names(df), msg = glue::glue("{e_comp} is not a valid column name of the dataframe."))
  assertthat::assert_that(c_comp %in% names(df), msg = glue::glue("{c_comp} is not a valid column name of the dataframe."))
  assertthat::assert_that(is.numeric(v_wtp), msg = ("'v_wtp' does not contain only numeric value(s)"))

  # Initiate matrices
  m_res <- matrix(NA, nrow = length(v_wtp), ncol = 3,
                  dimnames = list(v_wtp, c("WTP_threshold", "Prob_int", "Prob_comp")))
  m_tmp <- matrix(NA, nrow = length(df[, e_int]), ncol = 2, #temp stores NB for each iteration
                  dimnames = list(c(1:length(df[, e_int])),c("res_int", "res_comp")))

  # Loop over willingness to pay threshold to calculate net benefits for both strategies
  for (i in 1:length(v_wtp)){
    m_tmp[,"res_int"]   <- df[, e_int] * v_wtp[i] - df[, c_int]   #compute NB for intervention at wtp threshold
    m_tmp[,"res_comp"]  <- df[, e_comp] * v_wtp[i] - df[, c_comp] #compute NB for comparator at wtp threshold
    num_CE <- length(which(m_tmp[, "res_int"] > m_tmp[, "res_comp"])) #number of iterations in which intervention is cost effective
    p_trt  <- num_CE/length(df[, e_int]) #probability trt has a higher NB than comp
    m_res[i,] <- cbind(v_wtp[i], p_trt, 1 - p_trt) #vector of wtp and probabilities is stored
  }
  rownames(m_res) <- NULL
  df_res <- as.data.frame(m_res)

  # Export
  return(df_res)
}

#' Calculate cost-effectiveness probabilities.
#' @description This function calculates the probabilities that each strategy is the most cost effective, from an infinite amount of strategies.
#' @inheritParams plot_ce_mult
#' @param v_wtp vector of numerical values. Vector of willingness-to-pay threshold for which the probabilities of cost effectiveness have to be defined. Default is 0:100,000 by increments of 1,000.
#' @return A dataframe with three columns:
#' \itemize{
#'    \item WTP_threshold = The willingness-to-pay thresholds at which the probability of cost effectiveness has been calculated for both strategies
#'    \item Prob_int = The probability that the intervention strategy is cost effective at a given willingness-to-pay threshold
#'    \item Prob_comp = The probability that the comparator strategy is cost effective at a given willingness-to-pay threshold
#'    }
#' @examples
#' # Calculate probabilities of cost effectiveness using the example dataframe,
#' # for willlingness-to-pay thresholds of 0 to 50,0000 euros.
#' data("df_pa")
#' calculate_ceac(df = df_pa,
#'                e_int = "t_qaly_d_int",
#'                e_comp = "t_qaly_d_comp",
#'                c_int = "t_costs_d_int",
#'                c_comp = "t_costs_d_comp",
#'                v_wtp = seq(from = 0, to = 50000, by = 1000))
#' @import assertthat
#' @import glue
#' @import dplyr
#' @import tidyr
#' @export
calculate_ceac_mult <- function (df,
                                 outcomes,
                                 costs,
                                 v_wtp = seq(from = 0, to = 100000, by = 1000)){
  # Checks
  assertthat::assert_that(all(outcomes %in% names(df)),
                          msg = glue::glue("At least one variable in vector 'outcomes' is not a valid column name of 'df'."))
  assertthat::assert_that(all(costs %in% names(df)),
                          msg = glue::glue("At least one variable in vector 'costs' is not a valid column name of 'df'."))
  assertthat::assert_that(length(grep("^t_qaly_d_", outcomes)) == length(outcomes),
                          msg = "At least one element of 'outcomes' does not start with 't_qaly_d_'")
  assertthat::assert_that(length(grep("^t_costs_d_", costs)) == length(costs),
                          msg = "At least one element of 'costs' does not start with 't_costs_d_'")
  assertthat::assert_that(length(costs) == length(outcomes), msg = glue::glue("The length of vector of names 'outcomes' is different than length of vercor of names 'costs'."))
  assertthat::assert_that(identical(
    gsub("t_qaly_d_", "", outcomes)[order(gsub("t_qaly_d_", "", outcomes))],
    gsub("t_costs_d_", "", costs)[order(gsub("t_costs_d_",  "", costs))]
    ),
    msg = "The included strategies are different in 'outcomes' and 'costs'. Ensure that the name of the strategies are the same in both argument.")
  assertthat::assert_that(is.numeric(v_wtp), msg = ("'v_wtp' does not contain only numeric value(s)"))

  # Modify data for calculations
  outcomes <- outcomes[order(gsub("t_qaly_d_", "", outcomes))]
  costs    <- costs[order(gsub("t_costs_d_",  "", costs))]
  df_ceac <- df |>
    dplyr::select(c(outcomes,
                    costs)) |>
    dplyr::rename_all( ~ gsub("t_qaly_d", "QALY", .)) |>
    dplyr::rename_all( ~ gsub("t_costs_d", "Costs", .)) |>
    dplyr::mutate(Iteration = 1:nrow(df)) |>
    tidyr::pivot_longer(!Iteration,
                        names_to = c("Outcome", "Strategy"),
                        names_sep = "_") |>
    tidyr::pivot_wider(
      names_from = Outcome ,
      values_from = value
    )

  # Initiate matrices
  m_res <- matrix(NA,
                  nrow = length(v_wtp),
                  ncol = length(outcomes) + 1,
                  dimnames = list(v_wtp,
                                  c("WTP_threshold", unlist(unique(df_ceac[, "Strategy"]), use.names = FALSE)
                                    )
                                  )
                  )
  m_tmp <- matrix(NA,
                  nrow = nrow(df),
                  ncol = length(outcomes), #temp stores NB for each iteration
                  dimnames = list(c(1:nrow(df)),
                                  unlist(unique(df_ceac[, "Strategy"]), use.names = FALSE)
                                  )
                  )

  # Loop over willingness to pay threshold to calculate net benefits for both strategies
  v_n_comp <- 1:length(outcomes)
  for (i in 1:length(v_wtp)){
    m_tmp  <- df[, outcomes] * v_wtp[i] - df[, costs] # compute NB for intervention at wtp threshold
    v_max <- unname(apply(m_tmp, 1, which.max))
    v_n_max <- sapply(v_n_comp, function(x) length(which(v_max == v_n_comp[x])))
    v_p  <- v_n_max/nrow(df) #probability each has highest NB than comp
    m_res[i,] <- c(v_wtp[i], unlist(v_p)) # vector of wtp and probabilities is stored
  }
  rownames(m_res) <- NULL
  df_res <- as.data.frame(m_res)

  # Export
  return(df_res)
}

#' Plotting the cost-effectiveness acceptability curves.
#' @description This function plots cost-effectiveness acceptability curves.
#' @param df a dataframe obtained through the `calculate_ceac()`.
#' @param wtp character. Name of variable of the dataframe containing the willingness-to-pay thresholds at which the probability of cost effectiveness have been defined.
#' @return A ggplot2 graph.
#' @examples
#' # Plot CEAC based on results from calculate_ceac()
#' data("df_pa")
#' df_ceac_p <- calculate_ceac(df = df_pa,
#'                             e_int = "t_qaly_d_int",
#'                             e_comp = "t_qaly_d_comp",
#'                             c_int = "t_costs_d_int",
#'                             c_comp = "t_costs_d_comp")
#' plot_ceac(df = df_ceac_p,
#'           wtp = "WTP_threshold")
#' @import ggplot2
#' @import scales
#' @import assertthat
#' @import glue
#' @import tidyr
#' @export
plot_ceac <- function(df,
                      wtp,
                      currency = "euro") {
  # Check
  assertthat::assert_that(wtp %in% names(df), msg = glue::glue("{wtp} is not a valid column name of the dataframe."))
  if(!currency %in% c("none", "euro", "dollar", "yen")) {
    stop("The chosen currency is not valid.")
  }

  # Currency formatting
  cur <- switch(currency,
                euro = "\u20ac ",
                dollar = "\u0024 ",
                yen = "\u00a5 ",
                none = "")

  # Re-organise dataframe
  df_graph <- df |>
    tidyr::pivot_longer(!wtp,
                        names_to = "Strategy",
                        values_to = "Probability of cost effectiveness")


  # Plot
  p_out <- ggplot2::ggplot(data = df_graph, ggplot2::aes_string(x = wtp, y = "`Probability of cost effectiveness`", colour = "Strategy")) +
    ggplot2::geom_line() +
    ggplot2::xlab ("Willingness-to-pay threshold") +
    ggplot2::scale_x_continuous(labels = scales::dollar_format(prefix = cur, suffix = "")) +
    ggplot2::ylim(c(0, 1)) +
    ggplot2::theme_bw()

  # Export
  p_out
}

#' Calculate NMB and NHB.
#' @description This function calculates the Net Monetary Benefits (NMB) and Net Health Benefits (NHB) for each strategy and the incremental NMB and NHB.
#' @inheritParams plot_ice
#' @param wtp numeric. Willingness-to-pay thresholds to use for NMB and NHB calculations.
#' @return
#' A dataframe containing the original data and the following variables:
#' \itemize{
#'   \item NMB_int = Net monetary benefit of the intervention
#'   \item NMB_comp = Net monetary benefit of the comparator
#'   \item iNMB = Incremental net monetary benefit of the intervention versus the comparator
#'   \item NHB_int = Net health benefit of the intervention
#'   \item NHB_comp = Net health benefit of the comparator
#'   \item iNHB = Net health benefit of the intervention versus the comparator
#' }
#' @examples
#' # Calculate NB's at a willingness-to-pay threshold of 80000 per unit of effects
#' data("df_pa")
#' calculate_nb(df = df_pa,
#'              e_int = "t_qaly_d_int",
#'              e_comp = "t_qaly_d_comp",
#'              c_int = "t_costs_d_int",
#'              c_comp = "t_costs_d_comp",
#'              wtp = 80000)
#' @import assertthat
#' @export
calculate_nb <- function(df,
                         e_int,
                         e_comp,
                         c_int,
                         c_comp,
                         wtp) {
  # Checks
  assertthat::assert_that(e_int %in% names(df), msg = glue::glue("{e_int} is not a valid column name of the dataframe."))
  assertthat::assert_that(c_int %in% names(df), msg = glue::glue("{c_int} is not a valid column name of the dataframe."))
  assertthat::assert_that(e_comp %in% names(df), msg = glue::glue("{e_comp} is not a valid column name of the dataframe."))
  assertthat::assert_that(c_comp %in% names(df), msg = glue::glue("{c_comp} is not a valid column name of the dataframe."))
  assertthat::assert_that(is.numeric(wtp), msg = ("'wtp' is not a numeric value"))

  # Calculate outcomes and store them in a dataframe
  df_out <- data.frame(df,
                       NMB_int = df[, e_int] * wtp - df[, c_int],
                       NMB_comp = df[, e_comp] * wtp - df[, c_comp],
                       iNMB = (df[, e_int] - df[, e_comp]) * wtp - (df[, c_int] - df[, c_comp]),
                       NHB_int = df[, e_int] - df[, c_int] / wtp,
                       NHB_comp = df[, e_comp] - df[, c_comp] / wtp,
                       iNHB = (df[, e_int] - df[, e_comp]) - (df[, c_int] - df[, c_comp]) / wtp
  )

  # Export
  return(df_out)
}

#' Calculate NMB and NHB.
#' @description This function calculates the Net Monetary Benefits (NMB) and Net Health Benefits (NHB) for each strategy and the incremental NMB and NHB.
#' @inheritParams plot_ce_mult
#' @param wtp numeric. Willingness-to-pay thresholds to use for NMB and NHB calculations.
#' @return
#' A dataframe containing the original data and the following variables containing the NMB and NHB for each strategy.
#' The name of these new variables are structured as 'NMB_strategyname' and 'NHB_strategyname'
#' @examples
#' # Calculate NB's at a willingness-to-pay threshold of 80000 per unit of effects
#' data("df_pa")
#' df_pa$t_costs_d_comp2 <- df_pa$t_costs_d_comp * 1.09
#' df_pa$t_qaly_d_comp2 <- df_pa$t_qaly_d_comp * 1.01
#' calculate_nb_mult(df = df_pa,
#'                   outcomes = c("t_qaly_d_comp2", "t_qaly_d_int", "t_qaly_d_comp"),
#'                   costs = c("t_costs_d_int", "t_costs_d_comp2", "t_costs_d_comp"),
#'                   wtp = 50000
#'                   )
#' @import assertthat
#' @export
calculate_nb_mult <- function(df,
                              outcomes,
                              costs,
                              wtp) {
  # Checks
  assertthat::assert_that(all(outcomes %in% names(df)), msg = glue::glue("At least one variable in vector 'outcomes' is not a valid column name of 'df'."))
  assertthat::assert_that(all(costs %in% names(df)), msg = glue::glue("At least one variable in vector 'costs' is not a valid column name of 'df'."))
  assertthat::assert_that(length(grep("^t_qaly_d_", outcomes)) == length(outcomes),
                          msg = "At least one element of 'outcomes' does not start with 't_qaly_d_'")
  assertthat::assert_that(length(grep("^t_costs_d_", costs)) == length(costs),
                          msg = "At least one element of 'costs' does not start with 't_costs_d_'")
  assertthat::assert_that(length(costs) == length(outcomes), msg = glue::glue("The length of vector of names 'outcomes' is different than length of vercor of names 'costs'."))
  assertthat::assert_that(identical(
    gsub("t_qaly_d_", "", outcomes)[order(gsub("t_qaly_d_", "", outcomes))],
    gsub("t_costs_d_", "", costs)[order(gsub("t_costs_d_",  "", costs))]
  ),
  msg = "The included strategies are different in 'outcomes' and 'costs'. Ensure that the name of the strategies are the same in both argument.")

  # Modify data for calculations
  outcomes <- outcomes[order(gsub("t_qaly_d_", "", outcomes))]
  costs    <- costs[order(gsub("t_costs_d_",  "", costs))]
  v_names_strategies <- gsub("t_qaly_d_", "", outcomes)[order(gsub("t_qaly_d_", "", outcomes))]

  # Calculate outcomes and store them in a dataframe
  df_NMB <- df[, outcomes] * wtp - df[, costs]
  df_NHB <- df[, outcomes] - df[, costs] / wtp
  names(df_NMB) <- paste0("NMB", "_", v_names_strategies)
  names(df_NHB) <- paste0("NHB", "_", v_names_strategies)
  df_out <- data.frame(df,
                       df_NMB,
                       df_NHB
                       )

  # Export
  return(df_out)
}

#' Plot (i)NMB or (i)NHB.
#' @description This function plots the Net Monetary Benefits (NMB) and Net Health Benefits (NHB) for each strategy and the incremental NMB and NHB.
#' @param df a dataframe obtained through `calculate_nb()`
#' @param NMB logical. Should the (i)NMBs be plotted? Default is TRUE, if FALSE, (i)NHBs are plotted.
#' @param comparators logical. Should the NMB/NHB for each comparator be plotted? Default is TRUE.
#' @param incremental logical. Should the incremental NMB/NHB be plotted? Default is FALSE
#' @return A ggplot2 graph.
#' @details The use this function, the dataframe `df` should contain the variables `NMB_int`, `NMB_comp`, `iNMB`, `NHB_int`, `NHB_comp`, and `iNHB`. For instance, use the \code{\link{calculate_nb}} function to calculate these outcomes.
#' @examples
#' # Calculate NB's at a willingness-to-pay threshold of 80000 per unit of effects
#' data("df_pa")
#' df_nmb <- calculate_nb(df = df_pa,
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
#' @import ggplot2
#' @export
plot_nb <- function(df,
                    NMB = TRUE,
                    comparators = TRUE,
                    incremental = FALSE) {
  # Initialisation ggplot object
  p <- ggplot2::ggplot() +
    ggplot2::theme_bw()

  # Plot
  if(NMB == TRUE){
    if(comparators == TRUE){
      if(!"NMB_int" %in% names(df)) {stop("There is no variable called 'NMB_int' in the dataframe")}
      if(!"NMB_comp" %in% names(df)) {stop("There is no variable called 'NMB_comp' in the dataframe")}
      p <- p + ggplot2::geom_density(data = df, ggplot2::aes_string(x = "NMB_int", colour = factor("Intervention"))) +
        ggplot2::geom_density(data = df, ggplot2::aes_string(x = "NMB_comp", colour = factor("Comparator"))) +
        ggplot2::xlab("Net monetary benefit")
    }
    if(incremental == TRUE) {
      if(!"iNMB" %in% names(df)) {stop("There is no variable called 'iNMB' in the dataframe")}
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
      if(!"NHB_int" %in% names(df)) {stop("There is no variable called 'NHB_int' in the dataframe")}
      if(!"NHB_comp" %in% names(df)) {stop("There is no variable called 'NHB_comp' in the dataframe")}
      p <-
        p + ggplot2::geom_density(data = df, ggplot2::aes_string(x = "NHB_int", colour = factor("Intervention"))) +
        ggplot2::geom_density(data = df, ggplot2::aes_string(x = "NHB_comp", colour = factor("Comparator"))) +
        ggplot2::xlab("Net health benefit")
    }
    if(incremental == TRUE) {
      if(!"iNHB" %in% names(df)) {stop("There is no variable called 'iNHB' in the dataframe")}
      p <- p + ggplot2::geom_density(data = df, ggplot2::aes_string(x = "iNHB", colour = factor("Incremental")))
    }
    p <- p + ggplot2::scale_colour_manual("Strategy",
                                          values = c(Intervention = "grey",
                                                     Comparator = "orange",
                                                     Incremental = "black"
                                                     )
    )
  }

  # Export
  p
}
