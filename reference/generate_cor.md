# Generate correlation matrix

This function generates the correlation matrix of input and output
values of a probabilistic analysis.

## Usage

``` r
generate_cor(df, vars = NULL, figure = FALSE, digits = 3)
```

## Arguments

- df:

  a dataframe. This dataframe contains the probabilistic inputs and
  outputs of the health economic model.

- vars:

  a vector of strings. Contains the name of the variables to include in
  the correlation matrix. Default is NULL meaning all variables will be
  included.

- figure:

  logical. Should the correlation matrix be plotted in a figure? Default
  is FALSE (no figure generated).

- digits:

  integer. Number of decimals to display in correlation matrix. Default
  is 3.

## Value

If figure == FALSE: a matrix with summary statistics for the selected
inputs and outputs. If figure == TRUE: a tile ggplot2 of the correlation
matrix.

## Examples

``` r
# Generating summary data of all inputs using the example dataframe
data(df_pa)
generate_cor(df_pa)
#> Warning: the standard deviation is zero
#>                    p_pfspd p_pfsd  p_pdd p_dd   p_ae     rr  u_pfs   u_pd u_d
#> p_pfspd              1.000 -0.131 -0.073   NA  0.011  0.027 -0.031  0.000  NA
#> p_pfsd              -0.131  1.000  0.010   NA  0.015  0.058  0.020 -0.012  NA
#> p_pdd               -0.073  0.010  1.000   NA -0.017 -0.014 -0.080 -0.044  NA
#> p_dd                    NA     NA     NA    1     NA     NA     NA     NA  NA
#> p_ae                 0.011  0.015 -0.017   NA  1.000  0.045 -0.011 -0.010  NA
#> rr                   0.027  0.058 -0.014   NA  0.045  1.000  0.044 -0.053  NA
#> u_pfs               -0.031  0.020 -0.080   NA -0.011  0.044  1.000  0.052  NA
#> u_pd                 0.000 -0.012 -0.044   NA -0.010 -0.053  0.052  1.000  NA
#> u_d                     NA     NA     NA   NA     NA     NA     NA     NA   1
#> u_ae                -0.003 -0.062  0.013   NA -0.010 -0.042  0.016  0.060  NA
#> c_pfs               -0.015 -0.050 -0.026   NA -0.032  0.000  0.029  0.044  NA
#> c_pd                -0.001  0.017  0.021   NA  0.007 -0.015  0.003 -0.007  NA
#> c_d                     NA     NA     NA   NA     NA     NA     NA     NA  NA
#> c_thx               -0.025  0.045  0.003   NA -0.036  0.021 -0.033 -0.014  NA
#> c_ae                -0.003  0.005  0.013   NA -0.021 -0.010 -0.022  0.083  NA
#> t_qaly_comp         -0.235 -0.640 -0.431   NA -0.017 -0.057  0.333  0.406  NA
#> t_qaly_int          -0.217 -0.714 -0.347   NA -0.027 -0.164  0.341  0.330  NA
#> t_qaly_d_comp       -0.233 -0.644 -0.414   NA -0.018 -0.056  0.352  0.413  NA
#> t_qaly_d_int        -0.213 -0.715 -0.334   NA -0.028 -0.161  0.362  0.336  NA
#> t_costs_comp         0.096 -0.535 -0.541   NA -0.018 -0.023  0.042  0.029  NA
#> t_costs_int         -0.515 -0.704 -0.068   NA -0.032 -0.269  0.014  0.019  NA
#> t_costs_d_comp       0.136 -0.538 -0.487   NA -0.019 -0.021  0.035  0.031  NA
#> t_costs_d_int       -0.519 -0.710 -0.052   NA -0.032 -0.271  0.011  0.020  NA
#> t_ly_comp           -0.136 -0.749 -0.554   NA -0.010 -0.046  0.047  0.022  NA
#> t_ly_int            -0.123 -0.820 -0.441   NA -0.015 -0.140  0.031  0.019  NA
#> t_ly_d_comp         -0.130 -0.767 -0.538   NA -0.010 -0.047  0.044  0.022  NA
#> t_ly_d_int          -0.114 -0.834 -0.427   NA -0.015 -0.138  0.029  0.020  NA
#> t_ly_pfs_d_comp     -0.722 -0.553  0.052   NA -0.022 -0.066  0.025  0.006  NA
#> t_ly_pfs_d_int      -0.605 -0.641  0.045   NA -0.030 -0.297  0.008  0.014  NA
#> t_ly_pd_d_comp       0.505 -0.483 -0.749   NA  0.008  0.000  0.034  0.024  NA
#> t_ly_pd_d_int        0.546 -0.500 -0.700   NA  0.013  0.144  0.034  0.014  NA
#> t_qaly_pfs_d_comp   -0.665 -0.494  0.008   NA -0.025 -0.044  0.414  0.028  NA
#> t_qaly_pfs_d_int    -0.564 -0.576  0.003   NA -0.033 -0.252  0.400  0.036  NA
#> t_qaly_pd_d_comp     0.411 -0.391 -0.621   NA  0.002 -0.033  0.051  0.579  NA
#> t_qaly_pd_d_int      0.457 -0.418 -0.597   NA  0.007  0.087  0.051  0.545  NA
#> t_costs_pfs_d_comp  -0.522 -0.427  0.015   NA -0.049 -0.046  0.040  0.037  NA
#> t_costs_pfs_d_int   -0.605 -0.641  0.043   NA -0.035 -0.295  0.008  0.019  NA
#> t_costs_pd_d_comp    0.433 -0.353 -0.539   NA  0.006  0.002  0.016  0.014  NA
#> t_costs_pd_d_int     0.474 -0.371 -0.517   NA  0.009  0.132  0.016  0.007  NA
#> t_qaly_ae_int        0.006 -0.022 -0.006   NA  0.784  0.025 -0.010  0.019  NA
#> t_costs_ae_int       0.007  0.019 -0.014   NA  0.889  0.034 -0.017  0.021  NA
#> inc_ly               0.016 -0.744  0.311   NA -0.032 -0.505 -0.057 -0.003  NA
#> inc_qaly            -0.014 -0.673  0.196   NA -0.059 -0.550  0.222 -0.180  NA
#> inc_costs           -0.601 -0.650  0.059   NA -0.031 -0.292  0.004  0.014  NA
#>                      u_ae  c_pfs   c_pd c_d  c_thx   c_ae t_qaly_comp
#> p_pfspd            -0.003 -0.015 -0.001  NA -0.025 -0.003      -0.235
#> p_pfsd             -0.062 -0.050  0.017  NA  0.045  0.005      -0.640
#> p_pdd               0.013 -0.026  0.021  NA  0.003  0.013      -0.431
#> p_dd                   NA     NA     NA  NA     NA     NA          NA
#> p_ae               -0.010 -0.032  0.007  NA -0.036 -0.021      -0.017
#> rr                 -0.042  0.000 -0.015  NA  0.021 -0.010      -0.057
#> u_pfs               0.016  0.029  0.003  NA -0.033 -0.022       0.333
#> u_pd                0.060  0.044 -0.007  NA -0.014  0.083       0.406
#> u_d                    NA     NA     NA  NA     NA     NA          NA
#> u_ae                1.000  0.000  0.004  NA  0.006 -0.013       0.072
#> c_pfs               0.000  1.000 -0.083  NA -0.045  0.043       0.077
#> c_pd                0.004 -0.083  1.000  NA  0.025  0.025      -0.021
#> c_d                    NA     NA     NA   1     NA     NA          NA
#> c_thx               0.006 -0.045  0.025  NA  1.000  0.001      -0.031
#> c_ae               -0.013  0.043  0.025  NA  0.001  1.000       0.006
#> t_qaly_comp         0.072  0.077 -0.021  NA -0.031  0.006       1.000
#> t_qaly_int          0.076  0.073 -0.020  NA -0.038  0.003       0.982
#> t_qaly_d_comp       0.072  0.077 -0.021  NA -0.032  0.008       0.999
#> t_qaly_d_int        0.076  0.073 -0.020  NA -0.040  0.004       0.981
#> t_costs_comp        0.036  0.266  0.512  NA -0.016 -0.002       0.621
#> t_costs_int         0.066  0.115  0.085  NA  0.010 -0.006       0.742
#> t_costs_d_comp      0.034  0.319  0.542  NA -0.018  0.005       0.577
#> t_costs_d_int       0.065  0.126  0.087  NA  0.016 -0.003       0.734
#> t_ly_comp           0.051  0.066 -0.025  NA -0.021 -0.026       0.862
#> t_ly_int            0.061  0.064 -0.024  NA -0.029 -0.024       0.851
#> t_ly_d_comp         0.051  0.066 -0.025  NA -0.022 -0.025       0.861
#> t_ly_d_int          0.061  0.064 -0.025  NA -0.030 -0.022       0.849
#> t_ly_pfs_d_comp     0.050  0.050 -0.017  NA -0.012 -0.005       0.651
#> t_ly_pfs_d_int      0.064  0.048 -0.016  NA -0.024 -0.006       0.673
#> t_ly_pd_d_comp      0.020  0.040 -0.017  NA -0.018 -0.028       0.515
#> t_ly_pd_d_int       0.016  0.040 -0.018  NA -0.017 -0.027       0.484
#> t_qaly_pfs_d_comp   0.053  0.058 -0.012  NA -0.025 -0.010       0.730
#> t_qaly_pfs_d_int    0.067  0.057 -0.012  NA -0.036 -0.010       0.754
#> t_qaly_pd_d_comp    0.046  0.048 -0.018  NA -0.019  0.024       0.649
#> t_qaly_pd_d_int     0.042  0.046 -0.018  NA -0.018  0.023       0.626
#> t_costs_pfs_d_comp  0.044  0.690 -0.061  NA -0.036  0.013       0.517
#> t_costs_pfs_d_int   0.064  0.131 -0.021  NA  0.016 -0.003       0.673
#> t_costs_pd_d_comp   0.013 -0.028  0.625  NA  0.000 -0.002       0.347
#> t_costs_pd_d_int    0.008 -0.026  0.592  NA  0.001 -0.002       0.331
#> t_qaly_ae_int       0.565 -0.029  0.023  NA -0.023 -0.027       0.020
#> t_costs_ae_int     -0.016 -0.014  0.026  NA -0.036  0.402      -0.012
#> inc_ly              0.076  0.023 -0.009  NA -0.050  0.001       0.370
#> inc_qaly            0.056  0.020 -0.007  NA -0.053 -0.016       0.397
#> inc_costs           0.063  0.062 -0.033  NA  0.021 -0.004       0.667
#>                    t_qaly_int t_qaly_d_comp t_qaly_d_int t_costs_comp
#> p_pfspd                -0.217        -0.233       -0.213        0.096
#> p_pfsd                 -0.714        -0.644       -0.715       -0.535
#> p_pdd                  -0.347        -0.414       -0.334       -0.541
#> p_dd                       NA            NA           NA           NA
#> p_ae                   -0.027        -0.018       -0.028       -0.018
#> rr                     -0.164        -0.056       -0.161       -0.023
#> u_pfs                   0.341         0.352        0.362        0.042
#> u_pd                    0.330         0.413        0.336        0.029
#> u_d                        NA            NA           NA           NA
#> u_ae                    0.076         0.072        0.076        0.036
#> c_pfs                   0.073         0.077        0.073        0.266
#> c_pd                   -0.020        -0.021       -0.020        0.512
#> c_d                        NA            NA           NA           NA
#> c_thx                  -0.038        -0.032       -0.040       -0.016
#> c_ae                    0.003         0.008        0.004       -0.002
#> t_qaly_comp             0.982         0.999        0.981        0.621
#> t_qaly_int              1.000         0.983        0.999        0.610
#> t_qaly_d_comp           0.983         1.000        0.983        0.610
#> t_qaly_d_int            0.999         0.983        1.000        0.600
#> t_costs_comp            0.610         0.610        0.600        1.000
#> t_costs_int             0.808         0.740        0.801        0.522
#> t_costs_d_comp          0.572         0.569        0.564        0.994
#> t_costs_d_int           0.801         0.732        0.794        0.516
#> t_ly_comp               0.866         0.850        0.854        0.752
#> t_ly_int                0.885         0.843        0.875        0.722
#> t_ly_d_comp             0.869         0.851        0.858        0.749
#> t_ly_d_int              0.884         0.841        0.875        0.719
#> t_ly_pfs_d_comp         0.688         0.650        0.683        0.291
#> t_ly_pfs_d_int          0.746         0.672        0.740        0.340
#> t_ly_pd_d_comp          0.490         0.502        0.480        0.704
#> t_ly_pd_d_int           0.451         0.473        0.443        0.684
#> t_qaly_pfs_d_comp       0.766         0.737        0.770        0.287
#> t_qaly_pfs_d_int        0.823         0.761        0.826        0.331
#> t_qaly_pd_d_comp        0.585         0.642        0.580        0.577
#> t_qaly_pd_d_int         0.557         0.620        0.553        0.577
#> t_costs_pfs_d_comp      0.540         0.517        0.536        0.398
#> t_costs_pfs_d_int       0.744         0.672        0.739        0.360
#> t_costs_pd_d_comp       0.329         0.338        0.323        0.867
#> t_costs_pd_d_int        0.303         0.322        0.298        0.847
#> t_qaly_ae_int           0.013         0.020        0.012        0.013
#> t_costs_ae_int         -0.022        -0.011       -0.022       -0.010
#> inc_ly                  0.524         0.378        0.526        0.221
#> inc_qaly                0.563         0.406        0.567        0.251
#> inc_costs               0.741         0.667        0.736        0.329
#>                    t_costs_int t_costs_d_comp t_costs_d_int t_ly_comp t_ly_int
#> p_pfspd                 -0.515          0.136        -0.519    -0.136   -0.123
#> p_pfsd                  -0.704         -0.538        -0.710    -0.749   -0.820
#> p_pdd                   -0.068         -0.487        -0.052    -0.554   -0.441
#> p_dd                        NA             NA            NA        NA       NA
#> p_ae                    -0.032         -0.019        -0.032    -0.010   -0.015
#> rr                      -0.269         -0.021        -0.271    -0.046   -0.140
#> u_pfs                    0.014          0.035         0.011     0.047    0.031
#> u_pd                     0.019          0.031         0.020     0.022    0.019
#> u_d                         NA             NA            NA        NA       NA
#> u_ae                     0.066          0.034         0.065     0.051    0.061
#> c_pfs                    0.115          0.319         0.126     0.066    0.064
#> c_pd                     0.085          0.542         0.087    -0.025   -0.024
#> c_d                         NA             NA            NA        NA       NA
#> c_thx                    0.010         -0.018         0.016    -0.021   -0.029
#> c_ae                    -0.006          0.005        -0.003    -0.026   -0.024
#> t_qaly_comp              0.742          0.577         0.734     0.862    0.851
#> t_qaly_int               0.808          0.572         0.801     0.866    0.885
#> t_qaly_d_comp            0.740          0.569         0.732     0.850    0.843
#> t_qaly_d_int             0.801          0.564         0.794     0.854    0.875
#> t_costs_comp             0.522          0.994         0.516     0.752    0.722
#> t_costs_int              1.000          0.495         0.999     0.779    0.837
#> t_costs_d_comp           0.495          1.000         0.491     0.702    0.681
#> t_costs_d_int            0.999          0.491         1.000     0.769    0.829
#> t_ly_comp                0.779          0.702         0.769     1.000    0.984
#> t_ly_int                 0.837          0.681         0.829     0.984    1.000
#> t_ly_d_comp              0.785          0.702         0.775     1.000    0.987
#> t_ly_d_int               0.838          0.679         0.830     0.981    1.000
#> t_ly_pfs_d_comp          0.931          0.257         0.931     0.640    0.677
#> t_ly_pfs_d_int           0.978          0.310         0.979     0.681    0.750
#> t_ly_pd_d_comp           0.154          0.674         0.141     0.705    0.654
#> t_ly_pd_d_int            0.104          0.660         0.093     0.676    0.621
#> t_qaly_pfs_d_comp        0.851          0.252         0.850     0.603    0.630
#> t_qaly_pfs_d_int         0.899          0.300         0.898     0.643    0.699
#> t_qaly_pd_d_comp         0.128          0.554         0.119     0.573    0.531
#> t_qaly_pd_d_int          0.092          0.558         0.083     0.566    0.519
#> t_costs_pfs_d_comp       0.739          0.409         0.747     0.501    0.525
#> t_costs_pfs_d_int        0.981          0.335         0.983     0.680    0.748
#> t_costs_pd_d_comp        0.137          0.867         0.128     0.493    0.456
#> t_costs_pd_d_int         0.091          0.850         0.083     0.480    0.437
#> t_qaly_ae_int            0.009          0.011         0.009     0.020    0.020
#> t_costs_ae_int          -0.030         -0.008        -0.029    -0.017   -0.022
#> inc_ly                   0.679          0.238         0.684     0.411    0.566
#> inc_qaly                 0.667          0.258         0.668     0.437    0.574
#> inc_costs                0.976          0.300         0.978     0.675    0.746
#>                    t_ly_d_comp t_ly_d_int t_ly_pfs_d_comp t_ly_pfs_d_int
#> p_pfspd                 -0.130     -0.114          -0.722         -0.605
#> p_pfsd                  -0.767     -0.834          -0.553         -0.641
#> p_pdd                   -0.538     -0.427           0.052          0.045
#> p_dd                        NA         NA              NA             NA
#> p_ae                    -0.010     -0.015          -0.022         -0.030
#> rr                      -0.047     -0.138          -0.066         -0.297
#> u_pfs                    0.044      0.029           0.025          0.008
#> u_pd                     0.022      0.020           0.006          0.014
#> u_d                         NA         NA              NA             NA
#> u_ae                     0.051      0.061           0.050          0.064
#> c_pfs                    0.066      0.064           0.050          0.048
#> c_pd                    -0.025     -0.025          -0.017         -0.016
#> c_d                         NA         NA              NA             NA
#> c_thx                   -0.022     -0.030          -0.012         -0.024
#> c_ae                    -0.025     -0.022          -0.005         -0.006
#> t_qaly_comp              0.861      0.849           0.651          0.673
#> t_qaly_int               0.869      0.884           0.688          0.746
#> t_qaly_d_comp            0.851      0.841           0.650          0.672
#> t_qaly_d_int             0.858      0.875           0.683          0.740
#> t_costs_comp             0.749      0.719           0.291          0.340
#> t_costs_int              0.785      0.838           0.931          0.978
#> t_costs_d_comp           0.702      0.679           0.257          0.310
#> t_costs_d_int            0.775      0.830           0.931          0.979
#> t_ly_comp                1.000      0.981           0.640          0.681
#> t_ly_int                 0.987      1.000           0.677          0.750
#> t_ly_d_comp              1.000      0.985           0.645          0.688
#> t_ly_d_int               0.985      1.000           0.678          0.751
#> t_ly_pfs_d_comp          0.645      0.678           1.000          0.961
#> t_ly_pfs_d_int           0.688      0.751           0.961          1.000
#> t_ly_pd_d_comp           0.701      0.651          -0.093         -0.001
#> t_ly_pd_d_int            0.673      0.620          -0.117         -0.053
#> t_qaly_pfs_d_comp        0.606      0.629           0.916          0.874
#> t_qaly_pfs_d_int         0.648      0.699           0.887          0.916
#> t_qaly_pd_d_comp         0.570      0.529          -0.078          0.002
#> t_qaly_pd_d_int          0.564      0.519          -0.100         -0.040
#> t_costs_pfs_d_comp       0.504      0.526           0.742          0.712
#> t_costs_pfs_d_int        0.687      0.749           0.957          0.995
#> t_costs_pd_d_comp        0.490      0.454          -0.124         -0.051
#> t_costs_pd_d_int         0.479      0.437          -0.148         -0.099
#> t_qaly_ae_int            0.020      0.021           0.010          0.008
#> t_costs_ae_int          -0.017     -0.021          -0.021         -0.029
#> inc_ly                   0.430      0.578           0.500          0.686
#> inc_qaly                 0.452      0.582           0.482          0.669
#> inc_costs                0.683      0.748           0.959          0.998
#>                    t_ly_pd_d_comp t_ly_pd_d_int t_qaly_pfs_d_comp
#> p_pfspd                     0.505         0.546            -0.665
#> p_pfsd                     -0.483        -0.500            -0.494
#> p_pdd                      -0.749        -0.700             0.008
#> p_dd                           NA            NA                NA
#> p_ae                        0.008         0.013            -0.025
#> rr                          0.000         0.144            -0.044
#> u_pfs                       0.034         0.034             0.414
#> u_pd                        0.024         0.014             0.028
#> u_d                            NA            NA                NA
#> u_ae                        0.020         0.016             0.053
#> c_pfs                       0.040         0.040             0.058
#> c_pd                       -0.017        -0.018            -0.012
#> c_d                            NA            NA                NA
#> c_thx                      -0.018        -0.017            -0.025
#> c_ae                       -0.028        -0.027            -0.010
#> t_qaly_comp                 0.515         0.484             0.730
#> t_qaly_int                  0.490         0.451             0.766
#> t_qaly_d_comp               0.502         0.473             0.737
#> t_qaly_d_int                0.480         0.443             0.770
#> t_costs_comp                0.704         0.684             0.287
#> t_costs_int                 0.154         0.104             0.851
#> t_costs_d_comp              0.674         0.660             0.252
#> t_costs_d_int               0.141         0.093             0.850
#> t_ly_comp                   0.705         0.676             0.603
#> t_ly_int                    0.654         0.621             0.630
#> t_ly_d_comp                 0.701         0.673             0.606
#> t_ly_d_int                  0.651         0.620             0.629
#> t_ly_pfs_d_comp            -0.093        -0.117             0.916
#> t_ly_pfs_d_int             -0.001        -0.053             0.874
#> t_ly_pd_d_comp              1.000         0.986            -0.065
#> t_ly_pd_d_int               0.986         1.000            -0.087
#> t_qaly_pfs_d_comp          -0.065        -0.087             1.000
#> t_qaly_pfs_d_int            0.017        -0.031             0.968
#> t_qaly_pd_d_comp            0.815         0.798            -0.045
#> t_qaly_pd_d_int             0.828         0.833            -0.065
#> t_costs_pfs_d_comp         -0.035        -0.052             0.690
#> t_costs_pfs_d_int           0.001        -0.050             0.871
#> t_costs_pd_d_comp           0.755         0.748            -0.101
#> t_costs_pd_d_int            0.762         0.778            -0.123
#> t_qaly_ae_int               0.018         0.022             0.006
#> t_costs_ae_int             -0.002         0.002            -0.025
#> inc_ly                      0.093         0.058             0.429
#> inc_qaly                    0.139         0.086             0.526
#> inc_costs                  -0.006        -0.055             0.871
#>                    t_qaly_pfs_d_int t_qaly_pd_d_comp t_qaly_pd_d_int
#> p_pfspd                      -0.564            0.411           0.457
#> p_pfsd                       -0.576           -0.391          -0.418
#> p_pdd                         0.003           -0.621          -0.597
#> p_dd                             NA               NA              NA
#> p_ae                         -0.033            0.002           0.007
#> rr                           -0.252           -0.033           0.087
#> u_pfs                         0.400            0.051           0.051
#> u_pd                          0.036            0.579           0.545
#> u_d                              NA               NA              NA
#> u_ae                          0.067            0.046           0.042
#> c_pfs                         0.057            0.048           0.046
#> c_pd                         -0.012           -0.018          -0.018
#> c_d                              NA               NA              NA
#> c_thx                        -0.036           -0.019          -0.018
#> c_ae                         -0.010            0.024           0.023
#> t_qaly_comp                   0.754            0.649           0.626
#> t_qaly_int                    0.823            0.585           0.557
#> t_qaly_d_comp                 0.761            0.642           0.620
#> t_qaly_d_int                  0.826            0.580           0.553
#> t_costs_comp                  0.331            0.577           0.577
#> t_costs_int                   0.899            0.128           0.092
#> t_costs_d_comp                0.300            0.554           0.558
#> t_costs_d_int                 0.898            0.119           0.083
#> t_ly_comp                     0.643            0.573           0.566
#> t_ly_int                      0.699            0.531           0.519
#> t_ly_d_comp                   0.648            0.570           0.564
#> t_ly_d_int                    0.699            0.529           0.519
#> t_ly_pfs_d_comp               0.887           -0.078          -0.100
#> t_ly_pfs_d_int                0.916            0.002          -0.040
#> t_ly_pd_d_comp                0.017            0.815           0.828
#> t_ly_pd_d_int                -0.031            0.798           0.833
#> t_qaly_pfs_d_comp             0.968           -0.045          -0.065
#> t_qaly_pfs_d_int              1.000            0.027          -0.012
#> t_qaly_pd_d_comp              0.027            1.000           0.990
#> t_qaly_pd_d_int              -0.012            0.990           1.000
#> t_costs_pfs_d_comp            0.667           -0.018          -0.035
#> t_costs_pfs_d_int             0.911            0.006          -0.036
#> t_costs_pd_d_comp            -0.037            0.614           0.627
#> t_costs_pd_d_int             -0.080            0.615           0.648
#> t_qaly_ae_int                 0.003            0.023           0.027
#> t_costs_ae_int               -0.033            0.012           0.016
#> inc_ly                        0.599            0.072           0.047
#> inc_qaly                      0.700            0.004          -0.029
#> inc_costs                     0.912           -0.001          -0.042
#>                    t_costs_pfs_d_comp t_costs_pfs_d_int t_costs_pd_d_comp
#> p_pfspd                        -0.522            -0.605             0.433
#> p_pfsd                         -0.427            -0.641            -0.353
#> p_pdd                           0.015             0.043            -0.539
#> p_dd                               NA                NA                NA
#> p_ae                           -0.049            -0.035             0.006
#> rr                             -0.046            -0.295             0.002
#> u_pfs                           0.040             0.008             0.016
#> u_pd                            0.037             0.019             0.014
#> u_d                                NA                NA                NA
#> u_ae                            0.044             0.064             0.013
#> c_pfs                           0.690             0.131            -0.028
#> c_pd                           -0.061            -0.021             0.625
#> c_d                                NA                NA                NA
#> c_thx                          -0.036             0.016             0.000
#> c_ae                            0.013            -0.003            -0.002
#> t_qaly_comp                     0.517             0.673             0.347
#> t_qaly_int                      0.540             0.744             0.329
#> t_qaly_d_comp                   0.517             0.672             0.338
#> t_qaly_d_int                    0.536             0.739             0.323
#> t_costs_comp                    0.398             0.360             0.867
#> t_costs_int                     0.739             0.981             0.137
#> t_costs_d_comp                  0.409             0.335             0.867
#> t_costs_d_int                   0.747             0.983             0.128
#> t_ly_comp                       0.501             0.680             0.493
#> t_ly_int                        0.525             0.748             0.456
#> t_ly_d_comp                     0.504             0.687             0.490
#> t_ly_d_int                      0.526             0.749             0.454
#> t_ly_pfs_d_comp                 0.742             0.957            -0.124
#> t_ly_pfs_d_int                  0.712             0.995            -0.051
#> t_ly_pd_d_comp                 -0.035             0.001             0.755
#> t_ly_pd_d_int                  -0.052            -0.050             0.748
#> t_qaly_pfs_d_comp               0.690             0.871            -0.101
#> t_qaly_pfs_d_int                0.667             0.911            -0.037
#> t_qaly_pd_d_comp               -0.018             0.006             0.614
#> t_qaly_pd_d_int                -0.035            -0.036             0.627
#> t_costs_pfs_d_comp              1.000             0.767            -0.100
#> t_costs_pfs_d_int               0.767             1.000            -0.053
#> t_costs_pd_d_comp              -0.100            -0.053             1.000
#> t_costs_pd_d_int               -0.115            -0.100             0.989
#> t_qaly_ae_int                  -0.018             0.004             0.022
#> t_costs_ae_int                 -0.039            -0.033             0.012
#> inc_ly                          0.367             0.679             0.059
#> inc_qaly                        0.353             0.661             0.089
#> inc_costs                       0.721             0.997            -0.066
#>                    t_costs_pd_d_int t_qaly_ae_int t_costs_ae_int inc_ly
#> p_pfspd                       0.474         0.006          0.007  0.016
#> p_pfsd                       -0.371        -0.022          0.019 -0.744
#> p_pdd                        -0.517        -0.006         -0.014  0.311
#> p_dd                             NA            NA             NA     NA
#> p_ae                          0.009         0.784          0.889 -0.032
#> rr                            0.132         0.025          0.034 -0.505
#> u_pfs                         0.016        -0.010         -0.017 -0.057
#> u_pd                          0.007         0.019          0.021 -0.003
#> u_d                              NA            NA             NA     NA
#> u_ae                          0.008         0.565         -0.016  0.076
#> c_pfs                        -0.026        -0.029         -0.014  0.023
#> c_pd                          0.592         0.023          0.026 -0.009
#> c_d                              NA            NA             NA     NA
#> c_thx                         0.001        -0.023         -0.036 -0.050
#> c_ae                         -0.002        -0.027          0.402  0.001
#> t_qaly_comp                   0.331         0.020         -0.012  0.370
#> t_qaly_int                    0.303         0.013         -0.022  0.524
#> t_qaly_d_comp                 0.322         0.020         -0.011  0.378
#> t_qaly_d_int                  0.298         0.012         -0.022  0.526
#> t_costs_comp                  0.847         0.013         -0.010  0.221
#> t_costs_int                   0.091         0.009         -0.030  0.679
#> t_costs_d_comp                0.850         0.011         -0.008  0.238
#> t_costs_d_int                 0.083         0.009         -0.029  0.684
#> t_ly_comp                     0.480         0.020         -0.017  0.411
#> t_ly_int                      0.437         0.020         -0.022  0.566
#> t_ly_d_comp                   0.479         0.020         -0.017  0.430
#> t_ly_d_int                    0.437         0.021         -0.021  0.578
#> t_ly_pfs_d_comp              -0.148         0.010         -0.021  0.500
#> t_ly_pfs_d_int               -0.099         0.008         -0.029  0.686
#> t_ly_pd_d_comp                0.762         0.018         -0.002  0.093
#> t_ly_pd_d_int                 0.778         0.022          0.002  0.058
#> t_qaly_pfs_d_comp            -0.123         0.006         -0.025  0.429
#> t_qaly_pfs_d_int             -0.080         0.003         -0.033  0.599
#> t_qaly_pd_d_comp              0.615         0.023          0.012  0.072
#> t_qaly_pd_d_int               0.648         0.027          0.016  0.047
#> t_costs_pfs_d_comp           -0.115        -0.018         -0.039  0.367
#> t_costs_pfs_d_int            -0.100         0.004         -0.033  0.679
#> t_costs_pd_d_comp             0.989         0.022          0.012  0.059
#> t_costs_pd_d_int              1.000         0.022          0.013  0.020
#> t_qaly_ae_int                 0.022         1.000          0.689  0.011
#> t_costs_ae_int                0.013         0.689          1.000 -0.032
#> inc_ly                        0.020         0.011         -0.032  1.000
#> inc_qaly                      0.036        -0.029         -0.062  0.921
#> inc_costs                    -0.111         0.007         -0.029  0.693
#>                    inc_qaly inc_costs
#> p_pfspd              -0.014    -0.601
#> p_pfsd               -0.673    -0.650
#> p_pdd                 0.196     0.059
#> p_dd                     NA        NA
#> p_ae                 -0.059    -0.031
#> rr                   -0.550    -0.292
#> u_pfs                 0.222     0.004
#> u_pd                 -0.180     0.014
#> u_d                      NA        NA
#> u_ae                  0.056     0.063
#> c_pfs                 0.020     0.062
#> c_pd                 -0.007    -0.033
#> c_d                      NA        NA
#> c_thx                -0.053     0.021
#> c_ae                 -0.016    -0.004
#> t_qaly_comp           0.397     0.667
#> t_qaly_int            0.563     0.741
#> t_qaly_d_comp         0.406     0.667
#> t_qaly_d_int          0.567     0.736
#> t_costs_comp          0.251     0.329
#> t_costs_int           0.667     0.976
#> t_costs_d_comp        0.258     0.300
#> t_costs_d_int         0.668     0.978
#> t_ly_comp             0.437     0.675
#> t_ly_int              0.574     0.746
#> t_ly_d_comp           0.452     0.683
#> t_ly_d_int            0.582     0.748
#> t_ly_pfs_d_comp       0.482     0.959
#> t_ly_pfs_d_int        0.669     0.998
#> t_ly_pd_d_comp        0.139    -0.006
#> t_ly_pd_d_int         0.086    -0.055
#> t_qaly_pfs_d_comp     0.526     0.871
#> t_qaly_pfs_d_int      0.700     0.912
#> t_qaly_pd_d_comp      0.004    -0.001
#> t_qaly_pd_d_int      -0.029    -0.042
#> t_costs_pfs_d_comp    0.353     0.721
#> t_costs_pfs_d_int     0.661     0.997
#> t_costs_pd_d_comp     0.089    -0.066
#> t_costs_pd_d_int      0.036    -0.111
#> t_qaly_ae_int        -0.029     0.007
#> t_costs_ae_int       -0.062    -0.029
#> inc_ly                0.921     0.693
#> inc_qaly              1.000     0.671
#> inc_costs             0.671     1.000
```
