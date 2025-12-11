# Perform discounted and undiscounted results check

This function performs multiple checks on user-defined columns.

## Usage

``` r
do_discount_check(df, v_outcomes = NULL, v_outcomes_d = NULL)
```

## Arguments

- df:

  a dataframe.

- v_outcomes:

  (a vector of) character. Name of the variables containing undiscounted
  outcomes of the model.

- v_outcomes_d:

  (a vector of) character. Name of the variables containing discounted
  outcomes of the model.

## Value

A matrix.

## Details

The variables contained in \`v_outcomes\` and \`v_outcomes_d\` should be
in the same order.

## Examples

``` r
# Checking whether discounted QALYs are lower than undiscounted QALYs using the example data
do_discount_check(df = df_pa,
                  v_outcomes = c("t_qaly_comp", "t_qaly_int"),
                  v_outcomes_d = c("t_qaly_d_comp", "t_qaly_d_int")
                  )
#> Test passed with 20000 successes 🥇.
#>                                                           Test Result
#> 1 All discounted outcomes are lower than undiscounted outcomes   TRUE
```
