# Check mean quality of life

This function checks whether the mean quality of life outcome of each
iteration remain between the maximum and minimum utility values of the
specific iteration.

## Usage

``` r
check_mean_qol(df, t_qaly, t_ly, u_values, max_view = 100)
```

## Arguments

- df:

  a dataframe.

- t_qaly:

  character. Name of the variable containing the total undiscounted
  quality-adjusted life years.

- t_ly:

  character. Name of the variable containing the total undiscounted life
  years.

- u_values:

  (vector of) character. Name(s) of the variable containing the utility
  values.

- max_view:

  numeric. Determines the number of iterations to display which do not
  fulfil the check. Default is 100.

## Value

A matrix.

## Examples

``` r
# Check whether mean quality of life is within min-max utility values
check_mean_qol(df = df_pa,
               t_ly = "t_ly_comp",
               t_qaly = "t_qaly_comp",
               u_values = c("u_pfs", "u_pd")
               )
#>   Mean_QoL_below_min Mean_QoL_above_max
#>   "None"             "None"            
```
