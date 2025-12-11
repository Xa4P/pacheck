# Check binary

This function tests whether the value of variables remain between 0 and
1 (for instance for utility and probability inputs)

## Usage

``` r
check_binary(..., df, max_view = 50)
```

## Arguments

- ...:

  character vector. This character vector contains the name of the
  variables of which the sum will be checked.

- df:

  a dataframe.

- max_view:

  numeric. Determines the number of iterations to display which do not
  fulfill the check. Default is 50.

## Value

A dataframe.

## Examples

``` r
# Checking whether a variable is strictly positive
data(df_pa)
check_binary("u_pfs", df = df_pa)
#>   Input Negative_values Values_above_1
#> 1 u_pfs            None           None
# Checking whether two variables are strictly positive
# Decreasing the number of iterations to display to 20.
check_binary("u_pfs", "p_pfspd", df = df_pa)
#>     Input Negative_values Values_above_1
#> 1   u_pfs            None           None
#> 2 p_pfspd            None           None
```
