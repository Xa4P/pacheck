% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_metamodel.R
\name{fit_lm_metamodel}
\alias{fit_lm_metamodel}
\title{Fit linear metamodel}
\usage{
fit_lm_metamodel(
  df,
  y_var = NULL,
  x_vars = NULL,
  standardise = FALSE,
  partition = 1,
  seed_num = 1,
  validation = FALSE,
  folds = 5,
  show_intercept = FALSE,
  x_poly_2 = NULL,
  x_poly_3 = NULL,
  x_exp = NULL,
  x_log = NULL,
  x_inter = NULL
)
}
\arguments{
\item{df}{a dataframe.}

\item{y_var}{character. Name of the output variable in the dataframe. This will be the dependent variable of the metamodel.}

\item{x_vars}{character or a vector for characters. Name of the input variable in the dataframe. This will be the independent variable of the metamodel.}

\item{standardise}{logical. Determine whether the parameter of the linear regression should be standardised. Default is FALSE.}

\item{partition}{numeric. Value between 0 and 1 to determine the proportion of the observations to use to fit the metamodel. Default is 1 (fitting the metamodel using all observations).}

\item{seed_num}{numeric. Determine which seed number to use to split the dataframe in fitting and validation sets.}

\item{validation}{logical or character. Determine whether to validate the RF model. Choices are "test_train_split" and "cross-validation". TRUE corresponds to "cross-validation", default is FALSE.}

\item{folds}{numeric. Number of folds for the cross-validation. Default is 5.}

\item{show_intercept}{logical. Determine whether to show the intercept of the perfect prediction line (x = 0, y = 0). Default is FALSE.}

\item{x_poly_2}{character. character or a vector for characters. Name of the input variable in the dataframe. These variables will be exponentiated by factor 2.}

\item{x_poly_3}{character. character or a vector for characters. Name of the input variable in the dataframe. These variables will be exponentiated by factor 3.}

\item{x_exp}{character. character or a vector for characters. Name of the input variable in the dataframe. The exponential of these variables will be included in the metamodel.}

\item{x_log}{character. character or a vector for characters. Name of the input variable in the dataframe. The logarithm of these variables will be included in the metamodel.}

\item{x_inter}{character. character or a vector for characters. Name of the input variables in the dataframe. This vector contains the variables for which the interaction should be considered. The interaction terms of two consecutive variables will be considered in the linear model; hence, the length of this vector should be even.}
}
\value{
A list containing the fit of the model and validation estimates and plots when selected.
}
\description{
This function fits and provides summary statistics of a linear regression model fitted on the input and output values of a probabilistic analysis.
}
\details{
Standardisation of the parameters is obtained by \deqn{(x - u(x)) / sd(x)}
where \eqn{x} is the variable value, \eqn{u(x)} the mean over the variable and \eqn{sd(x)} the standard deviation of \eqn{x}.
For more details, see \href{https://doi.org/10.1177/0272989X13492014}{Jalal et al. 2013}.
}
\examples{
# Fitting linear meta model with two variables using the probabilistic data
data(df_pa)
fit_lm_metamodel(df = df_pa,
                 y_var = "inc_qaly",
                 x_vars = c("p_pfsd", "p_pdd")
                 )

}
