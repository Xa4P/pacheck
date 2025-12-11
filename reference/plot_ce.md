# Plotting cost-effectiveness plane for two strategies.

This function plots the cost-effectiveness plane for two strategies.

## Usage

``` r
plot_ce(df, e_int, e_comp, c_int, c_comp, currency = "euro")
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

- currency:

  character. Default is "euro". Determines the currency sign to use in
  the incremental cost effectiveness plane. Currently included signs:
  "euro", "dollar", "yen", "none".

## Value

A ggplot2 graph.

## Examples

``` r
# Plot cost effectiveness plane
data("df_pa")
plot_ce(df = df_pa,
        e_int = "t_qaly_d_int",
        e_comp = "t_qaly_d_comp",
        c_int = "t_costs_d_int",
        c_comp = "t_costs_d_comp"
        )
#> Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
#> ℹ Please use tidy evaluation idioms with `aes()`.
#> ℹ See also `vignette("ggplot2-in-packages")` for more information.
#> ℹ The deprecated feature was likely used in the pacheck package.
#>   Please report the issue to the authors.
#> Warning: All aesthetics have length 1, but the data has 10000 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
#> Warning: All aesthetics have length 1, but the data has 10000 rows.
#> ℹ Please consider using `annotate()` or provide this layer with data containing
#>   a single row.
```
