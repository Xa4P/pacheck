% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_inspection.R
\name{check_sum_probs}
\alias{check_sum_probs}
\title{Check sum probabilities}
\usage{
check_sum_probs(..., df, digits = NULL, check = "lower", max_view = 100)
}
\arguments{
\item{...}{character vector. This character vector contains the name of the variables of which the sum will be checked.}

\item{df}{a dataframe.}

\item{digits}{numeric. Define the number of digits at which the sum of probabilities should be rounded.}

\item{check}{logical. Define which test to perform."lower" tests whether the sum of the selected variables is lower than or equal to 1 for each iteration. "equal" tests whether the sum of the selected variables is equal to 1 for each iteration. Default is "lower".}

\item{max_view}{numeric. Determines the number of iterations to display which do not fulfill the test Default is 100.}
}
\value{
A text indicating whether the sum of the probabilities is belor and/or eual to one or indicating in which iteration that is not the case.
}
\description{
This function checks whether the sum of user-defined variables representing probabilities is below or equal to 1 for each iteration of the probabilistic inputs.
}
\examples{
# Checking whether the sum of the two probabilities is lower than or equal to 1
check_sum_probs("p_pfspd", "p_pfsd", df = df_pa, check = "lower")

# Checking the sum of the two probabilities equals 1 using a vector to select them,
# Rounding off to two digits, and extending the number of iterations to display to 250.
check_sum_probs(c("p_pfspd", "p_pfsd"), df = df_pa, digits = 2, check = "equal", max_view = 250)
}
