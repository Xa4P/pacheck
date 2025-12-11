# Fit distribution to parameter

This function fits statistical distributions to a user-defined
parameter.

## Usage

``` r
fit_dist(df, param, dist = c("norm", "beta", "gamma", "lnorm"))
```

## Arguments

- df:

  a dataframe.

- param:

  character. Name of variable of the dataframe on which to fit the
  distributions.

- dist:

  character or vector of character. Determine which distribution to fit
  on the density plot.

## Value

A list with two objects:

- Statistical_fit: a dataframe containing the statistical fit criteria
  of the fitted distributions.

- Dist_parameters: a dataframe containing the parameter value of the
  fitted distributions.

## Details

The available distributions are: "norm" (normal), "beta", "gamma",
"lnorm" (lognormal). The arguments of the lists are "AIC" which contains
the Akaike Information Criteria for each fitted distribution and
"Dist_parameters" which contains the parameters of the fitted
distributions. The distributions are fitted using the
[`fitdistrplus::fitdist()`](https://lbbe-software.github.io/fitdistrplus/reference/fitdist.html)

## Examples

``` r
# Fitting normal and beta distribution to the "u_pfs" variable of the example dataframe.
data(df_pa)
fit_dist(df = df_pa,
         param = "u_pfs",
         dist = c("norm", "beta"))
#> $Statistical_fit
#>   Distribution    AIC    BIC Kolmorogov-Smirnov
#> 1         norm -24724 -24710              0.026
#> 2         beta -24980 -24966              0.008
#> 
#> $Dist_parameters
#>   Distribution Name_param_1 Value_param_1 Name_param_2 Value_param_2
#> 1         norm         mean          0.75           sd          0.07
#> 2         beta       shape1          27.7       shape2          9.23
#> 
```
