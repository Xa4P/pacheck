# Calculate cost-effectiveness probabilities for two strategies.

This function calculates the probabilities that each strategy is the
cost effective at different willingness to pay thresholds.

## Usage

``` r
calculate_ceac(
  df,
  e_int,
  e_comp,
  c_int,
  c_comp,
  v_wtp = seq(from = 0, to = 1e+05, by = 1000)
)
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

- v_wtp:

  vector of numerical values. Vector of willingness-to-pay threshold for
  which the probabilities of cost effectiveness have to be defined.
  Default is 0:100,000 by increments of 1,000.

## Value

A dataframe with three columns:

- WTP_threshold = The willingness-to-pay thresholds at which the
  probability of cost effectiveness has been calculated for both
  strategies

- Prob_int = The probability that the intervention strategy is cost
  effective at a given willingness-to-pay threshold

- Prob_comp = The probability that the comparator strategy is cost
  effective at a given willingness-to-pay threshold

## Examples

``` r
# Calculate probabilities of cost effectiveness using the example dataframe,
# for willlingness-to-pay thresholds of 0 to 50,0000 euros.
data("df_pa")
calculate_ceac(df = df_pa,
               e_int = "t_qaly_d_int",
               e_comp = "t_qaly_d_comp",
               c_int = "t_costs_d_int",
               c_comp = "t_costs_d_comp",
               v_wtp = seq(from = 0, to = 50000, by = 1000))
#>    WTP_threshold Prob_int Prob_comp
#> 1              0    0.000     1.000
#> 2           1000    0.000     1.000
#> 3           2000    0.000     1.000
#> 4           3000    0.000     1.000
#> 5           4000    0.000     1.000
#> 6           5000    0.000     1.000
#> 7           6000    0.000     1.000
#> 8           7000    0.000     1.000
#> 9           8000    0.000     1.000
#> 10          9000    0.000     1.000
#> 11         10000    0.000     1.000
#> 12         11000    0.000     1.000
#> 13         12000    0.000     1.000
#> 14         13000    0.000     1.000
#> 15         14000    0.000     1.000
#> 16         15000    0.000     1.000
#> 17         16000    0.000     1.000
#> 18         17000    0.000     1.000
#> 19         18000    0.000     1.000
#> 20         19000    0.000     1.000
#> 21         20000    0.000     1.000
#> 22         21000    0.000     1.000
#> 23         22000    0.000     1.000
#> 24         23000    0.000     1.000
#> 25         24000    0.000     1.000
#> 26         25000    0.000     1.000
#> 27         26000    0.000     1.000
#> 28         27000    0.000     1.000
#> 29         28000    0.000     1.000
#> 30         29000    0.000     1.000
#> 31         30000    0.000     1.000
#> 32         31000    0.000     1.000
#> 33         32000    0.000     1.000
#> 34         33000    0.000     1.000
#> 35         34000    0.000     1.000
#> 36         35000    0.000     1.000
#> 37         36000    0.000     1.000
#> 38         37000    0.000     1.000
#> 39         38000    0.000     1.000
#> 40         39000    0.000     1.000
#> 41         40000    0.000     1.000
#> 42         41000    0.002     0.998
#> 43         42000    0.002     0.998
#> 44         43000    0.002     0.998
#> 45         44000    0.002     0.998
#> 46         45000    0.002     0.998
#> 47         46000    0.002     0.998
#> 48         47000    0.003     0.997
#> 49         48000    0.004     0.996
#> 50         49000    0.007     0.993
#> 51         50000    0.007     0.993
```
