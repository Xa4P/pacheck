% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_inspection.R
\name{generate_cor}
\alias{generate_cor}
\title{Generate correlation matrix}
\usage{
generate_cor(df, vars = NULL, figure = FALSE)
}
\arguments{
\item{df}{a dataframe. This dataframe contains the probabilistic inputs and outputs of the health economic model.}

\item{vars}{a vector of strings. Contains the name of the variables to include in the correlation matrix. Default is NULL meaning all variables will be included.}

\item{figure}{logical. Should the correlation matrix be plotted in a figure? Default is FALSE (no figure generated).}
}
\value{
If figure == FALSE: a matrix with summary statistics for the selected inputs and outputs. If figure == TRUE: a tile ggplot2 of the correlation matrix.
}
\description{
This function generates the correlation matrix of input and output values of a probabilistic analysis.
}
\examples{
# Generating summary data of all inputs using the example dataframe
data(df_pa)
generate_cor(df_pa)
}
