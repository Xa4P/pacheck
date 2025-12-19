# Check parametric survival models

This function tests whether the first of two parametric survival model
is lower than a second parametric survival model.

## Usage

``` r
check_surv_mod(
  df,
  surv_mod_1,
  surv_mod_2,
  v_names_param_mod_1,
  v_names_param_mod_2,
  time = seq(0, 5, 0.1),
  label_surv_1 = "first survival",
  label_surv_2 = "second survival",
  n_view = 10
)
```

## Arguments

- df:

  a dataframe.

- surv_mod_1:

  character. Name of the parametric model to use for the first survival
  model.

- surv_mod_2:

  character. Name of the parametric model to use for the second survival
  model.

- v_names_param_mod_1:

  (vector of) character. Name of the columns containing the parameter
  values for the first survival model.

- v_names_param_mod_2:

  (vector of) character. Name of the columns containing the parameter
  values for the second survival model.

- time:

  a numerical vector. Determine at which time points survival
  probabilities have to be estimated for both survival models. For each
  of these time points, it will be checked whether the first survival
  model results in higher survival probabilities than the second
  survival model.

- label_surv_1:

  character vector. The label to provide to the first survival curve
  (relevant for export).

- label_surv_2:

  character vector. The label to provide to the second survival curve
  (relevant for export).

- n_view:

  integer. Number of iterations to mention in which the curves are
  crossing. Default is 10.

## Value

A list. The first element is a message, the second element contains the
number of the iterations in which the the first curve is higher than the
second curve.

## Details

The parametric models that can be used are the following: exponential
([`exp`](https://rdrr.io/r/stats/Exponential.html)), Weibull
([`weibull`](https://rdrr.io/r/stats/Weibull.html)), gamma
([`gamma`](https://rdrr.io/r/stats/GammaDist.html)), loglogistic
([`logis`](https://rdrr.io/r/stats/Logistic.html)), and lognormal
([`lnorm`](https://rdrr.io/r/stats/Lognormal.html)). All these functions
are implemented following their distribution function as documented in
the [stats](https://rdrr.io/r/stats/stats-package.html) package.
