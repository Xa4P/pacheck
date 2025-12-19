# Plotting cost-effectiveness plane for two strategies.

This function plots the cost-effectiveness plane for two strategies.

## Usage

``` r
plot_ce(df, e_int, e_comp, c_int, c_comp, currency = "euro", axes = T)
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

- axes:

  logical. Default is TRUE, axes are plotted at x = 0 and y = 0. If
  FALSE, no axes are plotted.

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
        c_comp = "t_costs_d_comp",
        currency = "none"
        )
```
