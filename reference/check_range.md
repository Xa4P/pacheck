# Check range

This function tests whether an input or output value falls within a
user-defined range and return the proportion of iteration in which this
is not the case.

## Usage

``` r
check_range(df, param, min_val = NULL, max_val = NULL)
```

## Arguments

- df:

  a dataframe.

- param:

  character string. Name of variable of the dataframe for which to check
  the range.

- min_val:

  numeric. Define the minimum value of the range.

- max_val:

  numeric. Define the maximum value of the range.

## Value

A numeric.

## Details

If only \`min_val\` is specified, the proportion of iteration above this
value will be computed. If only \`max_val\` is specified, the proportion
of iteration below this value will be computed.

## Examples

``` r
# Checking how often the "u_pfs" values falls within 0.55 and 0.72.
data(df_pa)
check_range(df = df_pa,
            param = "u_pfs",
            min_val = 0.55,
            max_val = 0.72
                 )
#> [1] "The proportion of iterations between 0.55 and 0.72 is 31.91%"
```
