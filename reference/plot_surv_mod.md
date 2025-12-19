# Plot parametric survival models

This function plots two parametric survival models based on he
functional form of the model and their parameters.

## Usage

``` r
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

- label_surv_1:

  character vector. The label to provide to the first survival curve
  (relevant for export).

- label_surv_2:

  character vector. The label to provide to the second survival curve
  (relevant for export).

- iteration:

  integer. The row number of the iterations for which the parametric
  survival models have to be plotted.

- time:

  a numerical vector. Determine at which time points survival
  probabilities have to be estimated for both survival models. For each
  of these time points, it will be checked whether the first survival
  model results in higher survival probabilities than the second
  survival model.

## Value

A ggplot object.

## Details

The parametric models that can be used are the following: exponential
([`exp`](https://rdrr.io/r/stats/Exponential.html)), Weibull
([`weibull`](https://rdrr.io/r/stats/Weibull.html)), gamma
([`gamma`](https://rdrr.io/r/stats/GammaDist.html)), loglogistic
([`logis`](https://rdrr.io/r/stats/Logistic.html)), and lognormal
([`lnorm`](https://rdrr.io/r/stats/Lognormal.html)). All these functions
are implemented following their distribution function as documented in
the [stats](https://rdrr.io/r/stats/stats-package.html) package.
