# Calculate cost-effectiveness probabilities.

This function calculates the probabilities that each strategy is the
cost effective at different willingness to pay thresholds, for an
infinite amount of strategies.

## Usage

``` r
calculate_ceac_mult(
  df,
  outcomes,
  costs,
  v_wtp = seq(from = 0, to = 1e+05, by = 1000)
)
```

## Arguments

- df:

  a dataframe.

- outcomes:

  character. Vector of variable names containing the outcomes to be
  plotted on the x-axis. The variable names should be structured as
  follows: 't_qaly_d\_' followed by the name of the strategy: e.g.
  't_qaly_d_intervention'.

- costs:

  character. Vector of variable names containing the costs to be plotted
  on the y-axis. The variable names should be structured as follows:
  't_costs_d\_' followed by the name of the strategy: e.g.
  't_costs_d_intervention'.

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
data("df_pa")
df_pa$t_qaly_d_int2 <- df_pa$t_qaly_d_int * 1.5 # creating additional outcome variable
df_pa$t_costs_d_int2 <- df_pa$t_costs_d_int * 1.5 # creating additional cost variable
calculate_ceac_mult(df = df_pa,
             outcomes = c("t_qaly_d_int", "t_qaly_d_comp", "t_qaly_d_int2"),
             costs = c("t_costs_d_int","t_costs_d_comp", "t_costs_d_int2")
             )
#>     WTP_threshold  comp int  int2
#> 1               0 1.000   0 0.000
#> 2            1000 1.000   0 0.000
#> 3            2000 1.000   0 0.000
#> 4            3000 1.000   0 0.000
#> 5            4000 1.000   0 0.000
#> 6            5000 1.000   0 0.000
#> 7            6000 1.000   0 0.000
#> 8            7000 1.000   0 0.000
#> 9            8000 1.000   0 0.000
#> 10           9000 1.000   0 0.000
#> 11          10000 1.000   0 0.000
#> 12          11000 1.000   0 0.000
#> 13          12000 1.000   0 0.000
#> 14          13000 1.000   0 0.000
#> 15          14000 1.000   0 0.000
#> 16          15000 1.000   0 0.000
#> 17          16000 0.995   0 0.005
#> 18          17000 0.987   0 0.013
#> 19          18000 0.968   0 0.032
#> 20          19000 0.912   0 0.088
#> 21          20000 0.828   0 0.172
#> 22          21000 0.719   0 0.281
#> 23          22000 0.590   0 0.410
#> 24          23000 0.444   0 0.556
#> 25          24000 0.328   0 0.672
#> 26          25000 0.215   0 0.785
#> 27          26000 0.140   0 0.860
#> 28          27000 0.092   0 0.908
#> 29          28000 0.062   0 0.938
#> 30          29000 0.041   0 0.959
#> 31          30000 0.027   0 0.973
#> 32          31000 0.019   0 0.981
#> 33          32000 0.012   0 0.988
#> 34          33000 0.009   0 0.991
#> 35          34000 0.005   0 0.995
#> 36          35000 0.002   0 0.998
#> 37          36000 0.002   0 0.998
#> 38          37000 0.002   0 0.998
#> 39          38000 0.000   0 1.000
#> 40          39000 0.000   0 1.000
#> 41          40000 0.000   0 1.000
#> 42          41000 0.000   0 1.000
#> 43          42000 0.000   0 1.000
#> 44          43000 0.000   0 1.000
#> 45          44000 0.000   0 1.000
#> 46          45000 0.000   0 1.000
#> 47          46000 0.000   0 1.000
#> 48          47000 0.000   0 1.000
#> 49          48000 0.000   0 1.000
#> 50          49000 0.000   0 1.000
#> 51          50000 0.000   0 1.000
#> 52          51000 0.000   0 1.000
#> 53          52000 0.000   0 1.000
#> 54          53000 0.000   0 1.000
#> 55          54000 0.000   0 1.000
#> 56          55000 0.000   0 1.000
#> 57          56000 0.000   0 1.000
#> 58          57000 0.000   0 1.000
#> 59          58000 0.000   0 1.000
#> 60          59000 0.000   0 1.000
#> 61          60000 0.000   0 1.000
#> 62          61000 0.000   0 1.000
#> 63          62000 0.000   0 1.000
#> 64          63000 0.000   0 1.000
#> 65          64000 0.000   0 1.000
#> 66          65000 0.000   0 1.000
#> 67          66000 0.000   0 1.000
#> 68          67000 0.000   0 1.000
#> 69          68000 0.000   0 1.000
#> 70          69000 0.000   0 1.000
#> 71          70000 0.000   0 1.000
#> 72          71000 0.000   0 1.000
#> 73          72000 0.000   0 1.000
#> 74          73000 0.000   0 1.000
#> 75          74000 0.000   0 1.000
#> 76          75000 0.000   0 1.000
#> 77          76000 0.000   0 1.000
#> 78          77000 0.000   0 1.000
#> 79          78000 0.000   0 1.000
#> 80          79000 0.000   0 1.000
#> 81          80000 0.000   0 1.000
#> 82          81000 0.000   0 1.000
#> 83          82000 0.000   0 1.000
#> 84          83000 0.000   0 1.000
#> 85          84000 0.000   0 1.000
#> 86          85000 0.000   0 1.000
#> 87          86000 0.000   0 1.000
#> 88          87000 0.000   0 1.000
#> 89          88000 0.000   0 1.000
#> 90          89000 0.000   0 1.000
#> 91          90000 0.000   0 1.000
#> 92          91000 0.000   0 1.000
#> 93          92000 0.000   0 1.000
#> 94          93000 0.000   0 1.000
#> 95          94000 0.000   0 1.000
#> 96          95000 0.000   0 1.000
#> 97          96000 0.000   0 1.000
#> 98          97000 0.000   0 1.000
#> 99          98000 0.000   0 1.000
#> 100         99000 0.000   0 1.000
#> 101        100000 0.000   0 1.000
```
