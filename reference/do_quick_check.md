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
if (FALSE) { # \dontrun{
do_quick_check(df = df_pa,
               v_utilities = c("u_pfs", "u_pd"),
               v_costs = c("c_pfs", "c_pd")
               )
               } # }
```
