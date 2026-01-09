# Perform quick checks of inputs and outputs

This function performs multiple checks on user-defined columns.

## Usage

``` r
do_quick_check(
  df,
  v_probs = NULL,
  v_utilities = NULL,
  v_costs = NULL,
  v_hr = NULL,
  v_rr = NULL,
  v_r = NULL,
  v_outcomes = NULL
)
```

## Arguments

- df:

  a dataframe.

- v_probs:

  (a vector of) character. Name of variables containing probabilities.

- v_utilities:

  (a vector of) character. Name of the variables containing utility
  values.

- v_costs:

  (a vector of) character. Name of the variables containing cost
  estimates.

- v_hr:

  (a vector of) character. Name of the variables containing hazard
  ratios.

- v_rr:

  (a vector of) character. Name of the variables containing relative
  risks.

- v_r:

  (a vector of) character. Name of the variables containing rates.

- v_outcomes:

  (a vector of) character. Name of the variables containing outcomes of
  the model.

## Value

A matrix.

## Examples

``` r
# Checking costs and utility values of the example data
# \donttest{
do_quick_check(df = df_pa,
               v_utilities = c("u_pfs", "u_pd"),
               v_costs = c("c_pfs", "c_pd")
               )
#> Test passed with 2000 successes 🥇.
#> Test passed with 2000 successes 🥳.
#> Test passed with 2000 successes 🎉.
#>                                         Test        Result
#> 1             All probabilities are positive NOT PERFORMED
#> 2  All probabilities are lower or equal to 1 NOT PERFORMED
#> 3            All utility values are positive          TRUE
#> 4 All utility values are lower or equal to 1          TRUE
#> 5          All costs parameters are positive          TRUE
#> 6             All hazard ratios are positive NOT PERFORMED
#> 7            All relative risks are positive NOT PERFORMED
#> 8                     All rates are positive NOT PERFORMED
#> 9                  All outcomes are positive NOT PERFORMED
               # }
```
