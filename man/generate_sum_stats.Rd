% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_inspection.R
\name{generate_sum_stats}
\alias{generate_sum_stats}
\title{Generate summary statistics}
\usage{
generate_sum_stats(df, vars = NULL)
}
\arguments{
\item{df}{a dataframe. This dataframe contains the probabilistic inputs and outputs of the health economic model.}

\item{vars}{a vector of strings. Contains the name of the variables to include in the summary statistics table. Default is NULL meaning all variables will be included.#' @return A dataframe with summary data for the selected variables. The returned summary statistics are:
\itemize{
  \item Mean
  \item Standard deviation
  \item 2.5th percentile
  \item 97.5th percentile
  \item Minimum
  \item Maximum
  \item Median
  \item Skewness
  \item Kurtosis
}}
}
\description{
This function generates summary statistics of input and output values of a probabilistic analysis.
}
\examples{
# Generating summary data of all inputs using the example dataframe
data(df_pa)
df_summary <- generate_sum_stats(df_pa)
}
