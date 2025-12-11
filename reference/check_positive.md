# Check whether variables are strictly positive

This function tests whether variables are strictly positive (for
instance for costs and relative risks inputs)

## Usage

``` r
check_positive(..., df, max_view = 50)
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
check_positive("c_pfs", df = df_pa)
#>   Input Negative_values
#> 1 c_pfs            None

# Checking whether two variables are strictly positive
# Descreasing the number of iterations to display to 20.
check_positive("c_pfs", "c_pd", df = df_pa)
#>   Input Negative_values
#> 1 c_pfs            None
#> 2  c_pd            None
```
