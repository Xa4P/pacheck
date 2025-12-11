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
#>     WTP_threshold   comp int   int2
#> 1               0 1.0000   0 0.0000
#> 2            1000 1.0000   0 0.0000
#> 3            2000 1.0000   0 0.0000
#> 4            3000 1.0000   0 0.0000
#> 5            4000 1.0000   0 0.0000
#> 6            5000 1.0000   0 0.0000
#> 7            6000 1.0000   0 0.0000
#> 8            7000 1.0000   0 0.0000
#> 9            8000 1.0000   0 0.0000
#> 10           9000 1.0000   0 0.0000
#> 11          10000 1.0000   0 0.0000
#> 12          11000 1.0000   0 0.0000
#> 13          12000 1.0000   0 0.0000
#> 14          13000 0.9999   0 0.0001
#> 15          14000 0.9996   0 0.0004
#> 16          15000 0.9990   0 0.0010
#> 17          16000 0.9956   0 0.0044
#> 18          17000 0.9875   0 0.0125
#> 19          18000 0.9656   0 0.0344
#> 20          19000 0.9184   0 0.0816
#> 21          20000 0.8423   0 0.1577
#> 22          21000 0.7328   0 0.2672
#> 23          22000 0.5993   0 0.4007
#> 24          23000 0.4571   0 0.5429
#> 25          24000 0.3336   0 0.6664
#> 26          25000 0.2239   0 0.7761
#> 27          26000 0.1466   0 0.8534
#> 28          27000 0.0880   0 0.9120
#> 29          28000 0.0521   0 0.9479
#> 30          29000 0.0302   0 0.9698
#> 31          30000 0.0170   0 0.9830
#> 32          31000 0.0103   0 0.9897
#> 33          32000 0.0052   0 0.9948
#> 34          33000 0.0034   0 0.9966
#> 35          34000 0.0020   0 0.9980
#> 36          35000 0.0009   0 0.9991
#> 37          36000 0.0005   0 0.9995
#> 38          37000 0.0003   0 0.9997
#> 39          38000 0.0000   0 1.0000
#> 40          39000 0.0000   0 1.0000
#> 41          40000 0.0000   0 1.0000
#> 42          41000 0.0000   0 1.0000
#> 43          42000 0.0000   0 1.0000
#> 44          43000 0.0000   0 1.0000
#> 45          44000 0.0000   0 1.0000
#> 46          45000 0.0000   0 1.0000
#> 47          46000 0.0000   0 1.0000
#> 48          47000 0.0000   0 1.0000
#> 49          48000 0.0000   0 1.0000
#> 50          49000 0.0000   0 1.0000
#> 51          50000 0.0000   0 1.0000
#> 52          51000 0.0000   0 1.0000
#> 53          52000 0.0000   0 1.0000
#> 54          53000 0.0000   0 1.0000
#> 55          54000 0.0000   0 1.0000
#> 56          55000 0.0000   0 1.0000
#> 57          56000 0.0000   0 1.0000
#> 58          57000 0.0000   0 1.0000
#> 59          58000 0.0000   0 1.0000
#> 60          59000 0.0000   0 1.0000
#> 61          60000 0.0000   0 1.0000
#> 62          61000 0.0000   0 1.0000
#> 63          62000 0.0000   0 1.0000
#> 64          63000 0.0000   0 1.0000
#> 65          64000 0.0000   0 1.0000
#> 66          65000 0.0000   0 1.0000
#> 67          66000 0.0000   0 1.0000
#> 68          67000 0.0000   0 1.0000
#> 69          68000 0.0000   0 1.0000
#> 70          69000 0.0000   0 1.0000
#> 71          70000 0.0000   0 1.0000
#> 72          71000 0.0000   0 1.0000
#> 73          72000 0.0000   0 1.0000
#> 74          73000 0.0000   0 1.0000
#> 75          74000 0.0000   0 1.0000
#> 76          75000 0.0000   0 1.0000
#> 77          76000 0.0000   0 1.0000
#> 78          77000 0.0000   0 1.0000
#> 79          78000 0.0000   0 1.0000
#> 80          79000 0.0000   0 1.0000
#> 81          80000 0.0000   0 1.0000
#> 82          81000 0.0000   0 1.0000
#> 83          82000 0.0000   0 1.0000
#> 84          83000 0.0000   0 1.0000
#> 85          84000 0.0000   0 1.0000
#> 86          85000 0.0000   0 1.0000
#> 87          86000 0.0000   0 1.0000
#> 88          87000 0.0000   0 1.0000
#> 89          88000 0.0000   0 1.0000
#> 90          89000 0.0000   0 1.0000
#> 91          90000 0.0000   0 1.0000
#> 92          91000 0.0000   0 1.0000
#> 93          92000 0.0000   0 1.0000
#> 94          93000 0.0000   0 1.0000
#> 95          94000 0.0000   0 1.0000
#> 96          95000 0.0000   0 1.0000
#> 97          96000 0.0000   0 1.0000
#> 98          97000 0.0000   0 1.0000
#> 99          98000 0.0000   0 1.0000
#> 100         99000 0.0000   0 1.0000
#> 101        100000 0.0000   0 1.0000
```
