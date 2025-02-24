% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_costeffectiveness.R
\name{plot_ce_mult}
\alias{plot_ce_mult}
\title{Plotting cost-effectiveness plane.}
\usage{
plot_ce_mult(df, outcomes, costs, ellipse = FALSE, currency = "euro")
}
\arguments{
\item{df}{a dataframe.}

\item{outcomes}{character. Vector of variable names containing the outcomes to be plotted on the x-axis. The variable names should be structured as follows: 't_qaly_d_' followed by the name of the strategy: e.g. 't_qaly_d_intervention'.}

\item{costs}{character. Vector of variable names containing the costs to be plotted on the y-axis. The variable names should be structured as follows: 't_costs_d_' followed by the name of the strategy: e.g. 't_costs_d_intervention'.}

\item{ellipse}{logical. Determines whether plot should plot the dots of each iteration (default, ellipse = FALSE), or whether the mean outcomes and costs and their 95procent confidence ellipses should be plotted (TRUE).}

\item{currency}{character. Default is "euro". Determines the currency sign to use in the incremental cost effectiveness plane. Currently included signs: "euro", "dollar", "yen", "none".}
}
\value{
A ggplot2 graph.
# Plot cost effectiveness plane as ellipses
data("df_pa")
df_pa$t_qaly_d_int2 <- df_pa$t_qaly_d_int * 1.5 # creating additional outcome variable
df_pa$t_costs_d_int2 <- df_pa$t_costs_d_int * 1.5 # creating additional cost variable
plot_ce_mult(df = df_pa,
             outcomes = c("t_qaly_d_int", "t_qaly_d_comp", "t_qaly_d_int2"),
             costs = c("t_costs_d_int","t_costs_d_comp", "t_costs_d_int2"),
             ellipse = TRUE)
}
\description{
This function plots the cost-effectiveness plane for an infinite amount of strategies .
}
