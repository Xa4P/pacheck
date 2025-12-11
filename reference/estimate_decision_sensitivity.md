# Estimate decision sensitivy DSA using linear metamodel

This function performs a logistic regression analysis and determines the
decision sensitivity to parameter value using the logistic regression.
(STILL IN DEVELOPMENT)

## Usage

``` r
estimate_decision_sensitivity(df, y, x, y_binomial = FALSE, limit = 0)
```

## Arguments

- df:

  a dataframe. This dataframe should contain both dependent and
  independent variables.

- y:

  character. Name of the output variable in the dataframe. This will be
  the dependent variable of the logistic regression model.

- x:

  character or a vector for characters. Name of the input variable in
  the dataframe. This(these) will be the independent variable(s) of the
  logistic regression model.

- y_binomial:

  logical. Is \`y\` already a binomial outcome? Default is \`FALSE.\` If
  \`TRUE\`, the \`y\` variable will be used as such, otherwise, the
  \`y\` variable will be converted to a binomial variable using the
  \`limit\` argument.

- limit:

  numeric. Determines the limit when outcomes from \`y\` are categorised
  as 'success' (1) or not (0).

## Value

A dataframe with the parameter values of the fitted logistic regression
and the decision sensitivity associated with each parameter included in
the logistic regression model.

## Details

The method for these analyses is described in \[Merz et al.
1992\](https://doi.org/10.1177

## Examples

``` r
# Determining decision sensitivity using a non-binomial outcome
data(df_pa)
df_pa$inmb <- df_pa$inc_qaly * 100000 - df_pa$inc_costs
estimate_decision_sensitivity(df = df_pa,
                              y = "inmb",
                              x = c("p_pfsd", "p_pdd"),
                              y_binomial = FALSE
                              )
#>        Estimate  Std. Error z value   Pr(>|z|) Low_CI    High_CI   Importance
#> p_pfsd "-43.456" "1.141"    "-38.076" "0"      "-45.693" "-41.219" "63.9 %"  
#> p_pdd  "12.99"   "0.645"    "20.141"  "0"      "11.726"  "14.255"  "36.1 %"  
```
