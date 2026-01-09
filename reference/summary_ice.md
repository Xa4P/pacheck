# Summary statistics of the incremental cost-effectiveness plane.

This function computes the probability that the probabilistic outcome is
in each of the quadrants.

## Usage

``` r
summary_ice(df, e_int, e_comp, c_int, c_comp)
```

## Arguments

- df:

  a dataframe.

- e_int:

  character. Name of variable of the dataframe containing total effects
  of the intervention strategy.

- e_comp:

  character. Name of variable of the dataframe containing total effects
  of the comparator strategy.

- c_int:

  character. Name of variable of the dataframe containing total costs of
  the intervention strategy.

- c_comp:

  character. Name of variable of the dataframe containing total costs of
  the comparator strategy.

## Value

A dataframe.

## Examples

``` r
# Generating statistics of the incremental cost-effectiveness plane using the example data.
data(df_pa)
summary_ice(df = df_pa,
            e_int = "t_qaly_d_int",
            e_comp = "t_qaly_d_comp",
            c_int = "t_costs_d_int",
            c_comp = "t_costs_d_comp"
            )
#>                                     Quadrant Percentage
#> 1 NorthEast (more effective, more expensive)       100%
#> 2 SouthEast (more effective, less expensive)         0%
#> 3 NorthWest (less effective, more expensive)         0%
#> 4 SouthWest (less effective, less expensive)         0%
```
