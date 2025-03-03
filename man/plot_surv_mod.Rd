% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_inspection.R
\name{plot_surv_mod}
\alias{plot_surv_mod}
\title{Plot parametric survival models}
\usage{
plot_surv_mod(
  df,
  surv_mod_1,
  surv_mod_2,
  v_names_param_mod_1,
  v_names_param_mod_2,
  label_surv_1 = "first survival",
  label_surv_2 = "second survival",
  iteration,
  time = seq(0, 5, 1)
)
}
\arguments{
\item{df}{a dataframe.}

\item{surv_mod_1}{character. Name of the parametric model to use for the first survival model.}

\item{surv_mod_2}{character. Name of the parametric model to use for the second survival model.}

\item{v_names_param_mod_1}{(vector of) character. Name of the columns containing the parameter values for the first survival model.}

\item{v_names_param_mod_2}{(vector of) character. Name of the columns containing the parameter values for the second survival model.}

\item{label_surv_1}{character vector. The label to provide to the first survival curve (relevant for export).}

\item{label_surv_2}{character vector. The label to provide to the second survival curve (relevant for export).}

\item{iteration}{integer. The row number of the iterations for which the parametric survival models have to be plotted.}

\item{time}{a numerical vector. Determine at which time points survival probabilities have to be estimated for both survival models. For each of these time points, it will be checked whether the first survival model results in higher survival probabilities than the second survival model.}
}
\value{
A ggplot object.
}
\description{
This function plots two parametric survival models based on he functional form of the model and their parameters.
}
\details{
The parametric models that can be used are the following: exponential (\code{\link[stats:pexp]{`exp`}}), Weibull (\code{\link[stats:pweibull]{`weibull`}}), gamma (\code{\link[stats:pgamma]{`gamma`}}), loglogistic (\code{\link[stats:plogis]{`logis`}}), and lognormal (\code{\link[stats:plnorm]{`lnorm`}}). All these functions are implemented following their distribution function as documented in the \link[=stats]{stats} package.
}
