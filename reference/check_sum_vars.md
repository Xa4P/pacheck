# Check sum variables

This function tests whether the sum of selected variables are equal to
another.

## Usage

``` r
check_sum_vars(..., df, outcome, digits = 3)
```

## Arguments

- ...:

  character vector. This character vector contains the name of the
  variables of which the sum will be checked.

- df:

  a dataframe.

- outcome:

  character string. Name of variable of the dataframe which should equal
  the sum of variables mentioned in \`...\`.

- digits:

  Define the number of digits at which the sum and the \`outcome\`
  variables are rounded. Default is 3 digits.

## Value

A string.

## Examples

``` r
# Checking whether health state and adverse event costs equal the total discounted costs
check_sum_vars("t_costs_pfs_d_int", "t_costs_pd_d_int", "t_costs_ae_int",
               df = head(df_pa),
               outcome = "t_costs_d_int",
               digits = 0)
#> [1] "Sums of t_costs_pfs_d_int, t_costs_pd_d_int, t_costs_ae_int variables equal t_costs_d_int variabe"
```
