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
#> p_pfspd              1.000 -0.139 -0.010   NA -0.030  0.006 -0.003 -0.008  NA
#> p_pfsd              -0.139  1.000 -0.011   NA -0.004  0.007  0.010 -0.010  NA
#> p_pdd               -0.010 -0.011  1.000   NA  0.002 -0.006 -0.017 -0.014  NA
#> p_dd                    NA     NA     NA    1     NA     NA     NA     NA  NA
#> p_ae                -0.030 -0.004  0.002   NA  1.000  0.008 -0.011  0.004  NA
#> rr                   0.006  0.007 -0.006   NA  0.008  1.000 -0.004 -0.006  NA
#> u_pfs               -0.003  0.010 -0.017   NA -0.011 -0.004  1.000  0.021  NA
#> u_pd                -0.008 -0.010 -0.014   NA  0.004 -0.006  0.021  1.000  NA
#> u_d                     NA     NA     NA   NA     NA     NA     NA     NA   1
#> u_ae                 0.004 -0.008  0.006   NA -0.021 -0.023  0.014  0.008  NA
#> c_pfs               -0.007  0.009  0.008   NA  0.012  0.000  0.003 -0.013  NA
#> c_pd                -0.006 -0.002  0.004   NA -0.003 -0.013 -0.004  0.004  NA
#> c_d                     NA     NA     NA   NA     NA     NA     NA     NA  NA
#> c_thx               -0.010  0.003 -0.007   NA -0.006  0.003 -0.001  0.003  NA
#> c_ae                -0.005 -0.021  0.008   NA -0.002 -0.015 -0.014  0.019  NA
#> t_qaly_comp         -0.249 -0.645 -0.409   NA  0.009 -0.010  0.285  0.410  NA
#> t_qaly_int          -0.223 -0.719 -0.323   NA  0.005 -0.122  0.306  0.333  NA
#> t_qaly_d_comp       -0.246 -0.650 -0.390   NA  0.009 -0.011  0.305  0.417  NA
#> t_qaly_d_int        -0.217 -0.721 -0.308   NA  0.004 -0.120  0.328  0.339  NA
#> t_costs_comp         0.072 -0.525 -0.527   NA  0.000 -0.008  0.004  0.014  NA
#> t_costs_int         -0.509 -0.703 -0.087   NA  0.022 -0.221  0.000  0.018  NA
#> t_costs_d_comp       0.116 -0.525 -0.467   NA  0.000 -0.008  0.002  0.012  NA
#> t_costs_d_int       -0.512 -0.710 -0.072   NA  0.022 -0.222 -0.001  0.017  NA
#> t_ly_comp           -0.162 -0.744 -0.549   NA  0.009 -0.007  0.006  0.021  NA
#> t_ly_int            -0.141 -0.816 -0.435   NA  0.009 -0.097  0.004  0.020  NA
#> t_ly_d_comp         -0.154 -0.762 -0.532   NA  0.009 -0.007  0.006  0.021  NA
#> t_ly_d_int          -0.132 -0.830 -0.420   NA  0.009 -0.095  0.003  0.020  NA
#> t_ly_pfs_d_comp     -0.710 -0.562  0.016   NA  0.028 -0.011  0.000  0.017  NA
#> t_ly_pfs_d_int      -0.596 -0.644  0.018   NA  0.023 -0.248 -0.001  0.017  NA
#> t_ly_pd_d_comp       0.478 -0.483 -0.732   NA -0.015  0.001  0.008  0.012  NA
#> t_ly_pd_d_int        0.519 -0.509 -0.676   NA -0.014  0.153  0.006  0.010  NA
#> t_qaly_pfs_d_comp   -0.655 -0.512  0.007   NA  0.021 -0.013  0.383  0.025  NA
#> t_qaly_pfs_d_int    -0.549 -0.587  0.008   NA  0.017 -0.229  0.385  0.025  NA
#> t_qaly_pd_d_comp     0.376 -0.387 -0.588   NA -0.011 -0.002  0.020  0.593  NA
#> t_qaly_pd_d_int      0.418 -0.418 -0.557   NA -0.011  0.122  0.018  0.563  NA
#> t_costs_pfs_d_comp  -0.518 -0.399  0.017   NA  0.027 -0.007  0.002  0.003  NA
#> t_costs_pfs_d_int   -0.597 -0.643  0.018   NA  0.024 -0.247 -0.001  0.016  NA
#> t_costs_pd_d_comp    0.410 -0.361 -0.524   NA -0.015 -0.005  0.001  0.011  NA
#> t_costs_pd_d_int     0.451 -0.387 -0.497   NA -0.016  0.132 -0.001  0.010  NA
#> t_qaly_ae_int       -0.021 -0.008  0.006   NA  0.764 -0.005  0.000  0.005  NA
#> t_costs_ae_int      -0.028 -0.011  0.003   NA  0.895 -0.001 -0.015  0.012  NA
#> inc_ly               0.042 -0.741  0.333   NA  0.003 -0.474 -0.011  0.005  NA
#> inc_qaly             0.023 -0.658  0.219   NA -0.021 -0.548  0.257 -0.192  NA
#> inc_costs           -0.590 -0.654  0.032   NA  0.025 -0.242 -0.002  0.016  NA
#>                      u_ae  c_pfs   c_pd c_d  c_thx   c_ae t_qaly_comp
#> p_pfspd             0.004 -0.007 -0.006  NA -0.010 -0.005      -0.249
#> p_pfsd             -0.008  0.009 -0.002  NA  0.003 -0.021      -0.645
#> p_pdd               0.006  0.008  0.004  NA -0.007  0.008      -0.409
#> p_dd                   NA     NA     NA  NA     NA     NA          NA
#> p_ae               -0.021  0.012 -0.003  NA -0.006 -0.002       0.009
#> rr                 -0.023  0.000 -0.013  NA  0.003 -0.015      -0.010
#> u_pfs               0.014  0.003 -0.004  NA -0.001 -0.014       0.285
#> u_pd                0.008 -0.013  0.004  NA  0.003  0.019       0.410
#> u_d                    NA     NA     NA  NA     NA     NA          NA
#> u_ae                1.000 -0.010  0.008  NA  0.009  0.000       0.006
#> c_pfs              -0.010  1.000 -0.004  NA -0.005 -0.008      -0.010
#> c_pd                0.008 -0.004  1.000  NA  0.005  0.004      -0.002
#> c_d                    NA     NA     NA   1     NA     NA          NA
#> c_thx               0.009 -0.005  0.005  NA  1.000  0.002       0.007
#> c_ae                0.000 -0.008  0.004  NA  0.002  1.000       0.014
#> t_qaly_comp         0.006 -0.010 -0.002  NA  0.007  0.014       1.000
#> t_qaly_int          0.007 -0.011 -0.002  NA  0.006  0.016       0.982
#> t_qaly_d_comp       0.007 -0.010 -0.002  NA  0.007  0.014       0.999
#> t_qaly_d_int        0.007 -0.011 -0.001  NA  0.005  0.016       0.980
#> t_costs_comp        0.003  0.264  0.549  NA  0.006  0.006       0.598
#> t_costs_int         0.009  0.068  0.111  NA  0.044  0.020       0.751
#> t_costs_d_comp      0.003  0.324  0.580  NA  0.005  0.006       0.550
#> t_costs_d_int       0.009  0.081  0.113  NA  0.049  0.021       0.743
#> t_ly_comp          -0.001 -0.010 -0.004  NA  0.008  0.010       0.861
#> t_ly_int            0.003 -0.011 -0.003  NA  0.006  0.014       0.854
#> t_ly_d_comp         0.000 -0.010 -0.004  NA  0.008  0.011       0.861
#> t_ly_d_int          0.003 -0.011 -0.003  NA  0.006  0.015       0.852
#> t_ly_pfs_d_comp     0.004 -0.001  0.002  NA  0.008  0.015       0.672
#> t_ly_pfs_d_int      0.009 -0.004  0.004  NA  0.006  0.020       0.690
#> t_ly_pd_d_comp     -0.004 -0.013 -0.007  NA  0.002  0.000       0.510
#> t_ly_pd_d_int      -0.006 -0.013 -0.009  NA  0.001 -0.001       0.488
#> t_qaly_pfs_d_comp   0.008  0.001  0.000  NA  0.007  0.009       0.734
#> t_qaly_pfs_d_int    0.013 -0.002  0.001  NA  0.005  0.014       0.749
#> t_qaly_pd_d_comp    0.001 -0.016 -0.003  NA  0.003  0.011       0.656
#> t_qaly_pd_d_int    -0.002 -0.016 -0.004  NA  0.002  0.009       0.637
#> t_costs_pfs_d_comp -0.001  0.682  0.000  NA  0.002  0.005       0.475
#> t_costs_pfs_d_int   0.009  0.084  0.004  NA  0.049  0.020       0.685
#> t_costs_pd_d_comp   0.004 -0.014  0.639  NA  0.004  0.004       0.347
#> t_costs_pd_d_int    0.001 -0.014  0.606  NA  0.003  0.002       0.338
#> t_qaly_ae_int       0.576  0.005  0.004  NA  0.002 -0.001       0.011
#> t_costs_ae_int     -0.019  0.009 -0.002  NA -0.005  0.409       0.014
#> inc_ly              0.019 -0.010  0.002  NA -0.006  0.027       0.378
#> inc_qaly            0.005 -0.009  0.001  NA -0.006  0.019       0.373
#> inc_costs           0.009  0.012 -0.014  NA  0.053  0.021       0.685
#>                    t_qaly_int t_qaly_d_comp t_qaly_d_int t_costs_comp
#> p_pfspd                -0.223        -0.246       -0.217        0.072
#> p_pfsd                 -0.719        -0.650       -0.721       -0.525
#> p_pdd                  -0.323        -0.390       -0.308       -0.527
#> p_dd                       NA            NA           NA           NA
#> p_ae                    0.005         0.009        0.004        0.000
#> rr                     -0.122        -0.011       -0.120       -0.008
#> u_pfs                   0.306         0.305        0.328        0.004
#> u_pd                    0.333         0.417        0.339        0.014
#> u_d                        NA            NA           NA           NA
#> u_ae                    0.007         0.007        0.007        0.003
#> c_pfs                  -0.011        -0.010       -0.011        0.264
#> c_pd                   -0.002        -0.002       -0.001        0.549
#> c_d                        NA            NA           NA           NA
#> c_thx                   0.006         0.007        0.005        0.006
#> c_ae                    0.016         0.014        0.016        0.006
#> t_qaly_comp             0.982         0.999        0.980        0.598
#> t_qaly_int              1.000         0.983        0.999        0.590
#> t_qaly_d_comp           0.983         1.000        0.982        0.588
#> t_qaly_d_int            0.999         0.982        1.000        0.580
#> t_costs_comp            0.590         0.588        0.580        1.000
#> t_costs_int             0.813         0.748        0.806        0.533
#> t_costs_d_comp          0.547         0.541        0.540        0.994
#> t_costs_d_int           0.806         0.741        0.800        0.528
#> t_ly_comp               0.863         0.850        0.851        0.732
#> t_ly_int                0.886         0.846        0.876        0.706
#> t_ly_d_comp             0.866         0.851        0.855        0.730
#> t_ly_d_int              0.886         0.844        0.876        0.703
#> t_ly_pfs_d_comp         0.703         0.671        0.697        0.309
#> t_ly_pfs_d_int          0.758         0.689        0.753        0.353
#> t_ly_pd_d_comp          0.488         0.497        0.478        0.685
#> t_ly_pd_d_int           0.457         0.476        0.448        0.666
#> t_qaly_pfs_d_comp       0.770         0.740        0.773        0.286
#> t_qaly_pfs_d_int        0.821         0.757        0.825        0.326
#> t_qaly_pd_d_comp        0.591         0.649        0.586        0.551
#> t_qaly_pd_d_int         0.567         0.631        0.563        0.549
#> t_costs_pfs_d_comp      0.496         0.474        0.492        0.411
#> t_costs_pfs_d_int       0.753         0.685        0.748        0.376
#> t_costs_pd_d_comp       0.333         0.338        0.327        0.871
#> t_costs_pd_d_int        0.314         0.330        0.309        0.851
#> t_qaly_ae_int           0.008         0.011        0.007        0.003
#> t_costs_ae_int          0.011         0.014        0.010        0.003
#> inc_ly                  0.534         0.388        0.538        0.220
#> inc_qaly                0.543         0.384        0.550        0.239
#> inc_costs               0.755         0.685        0.750        0.343
#>                    t_costs_int t_costs_d_comp t_costs_d_int t_ly_comp t_ly_int
#> p_pfspd                 -0.509          0.116        -0.512    -0.162   -0.141
#> p_pfsd                  -0.703         -0.525        -0.710    -0.744   -0.816
#> p_pdd                   -0.087         -0.467        -0.072    -0.549   -0.435
#> p_dd                        NA             NA            NA        NA       NA
#> p_ae                     0.022          0.000         0.022     0.009    0.009
#> rr                      -0.221         -0.008        -0.222    -0.007   -0.097
#> u_pfs                    0.000          0.002        -0.001     0.006    0.004
#> u_pd                     0.018          0.012         0.017     0.021    0.020
#> u_d                         NA             NA            NA        NA       NA
#> u_ae                     0.009          0.003         0.009    -0.001    0.003
#> c_pfs                    0.068          0.324         0.081    -0.010   -0.011
#> c_pd                     0.111          0.580         0.113    -0.004   -0.003
#> c_d                         NA             NA            NA        NA       NA
#> c_thx                    0.044          0.005         0.049     0.008    0.006
#> c_ae                     0.020          0.006         0.021     0.010    0.014
#> t_qaly_comp              0.751          0.550         0.743     0.861    0.854
#> t_qaly_int               0.813          0.547         0.806     0.863    0.886
#> t_qaly_d_comp            0.748          0.541         0.741     0.850    0.846
#> t_qaly_d_int             0.806          0.540         0.800     0.851    0.876
#> t_costs_comp             0.533          0.994         0.528     0.732    0.706
#> t_costs_int              1.000          0.502         0.999     0.789    0.845
#> t_costs_d_comp           0.502          1.000         0.500     0.677    0.659
#> t_costs_d_int            0.999          0.500         1.000     0.780    0.837
#> t_ly_comp                0.789          0.677         0.780     1.000    0.985
#> t_ly_int                 0.845          0.659         0.837     0.985    1.000
#> t_ly_d_comp              0.795          0.677         0.787     1.000    0.988
#> t_ly_d_int               0.846          0.658         0.839     0.982    1.000
#> t_ly_pfs_d_comp          0.932          0.270         0.931     0.667    0.700
#> t_ly_pfs_d_int           0.979          0.319         0.978     0.700    0.765
#> t_ly_pd_d_comp           0.171          0.651         0.159     0.702    0.654
#> t_ly_pd_d_int            0.127          0.638         0.117     0.677    0.626
#> t_qaly_pfs_d_comp        0.857          0.249         0.856     0.616    0.645
#> t_qaly_pfs_d_int         0.899          0.294         0.899     0.646    0.705
#> t_qaly_pd_d_comp         0.143          0.523         0.133     0.567    0.528
#> t_qaly_pd_d_int          0.110          0.525         0.102     0.560    0.518
#> t_costs_pfs_d_comp       0.717          0.425         0.726     0.470    0.494
#> t_costs_pfs_d_int        0.981          0.347         0.984     0.696    0.760
#> t_costs_pd_d_comp        0.162          0.870         0.156     0.489    0.457
#> t_costs_pd_d_int         0.122          0.853         0.115     0.482    0.444
#> t_qaly_ae_int            0.023          0.003         0.024     0.008    0.010
#> t_costs_ae_int           0.027          0.002         0.028     0.012    0.014
#> inc_ly                   0.667          0.238         0.672     0.401    0.555
#> inc_qaly                 0.638          0.248         0.641     0.407    0.546
#> inc_costs                0.977          0.311         0.978     0.695    0.762
#>                    t_ly_d_comp t_ly_d_int t_ly_pfs_d_comp t_ly_pfs_d_int
#> p_pfspd                 -0.154     -0.132          -0.710         -0.596
#> p_pfsd                  -0.762     -0.830          -0.562         -0.644
#> p_pdd                   -0.532     -0.420           0.016          0.018
#> p_dd                        NA         NA              NA             NA
#> p_ae                     0.009      0.009           0.028          0.023
#> rr                      -0.007     -0.095          -0.011         -0.248
#> u_pfs                    0.006      0.003           0.000         -0.001
#> u_pd                     0.021      0.020           0.017          0.017
#> u_d                         NA         NA              NA             NA
#> u_ae                     0.000      0.003           0.004          0.009
#> c_pfs                   -0.010     -0.011          -0.001         -0.004
#> c_pd                    -0.004     -0.003           0.002          0.004
#> c_d                         NA         NA              NA             NA
#> c_thx                    0.008      0.006           0.008          0.006
#> c_ae                     0.011      0.015           0.015          0.020
#> t_qaly_comp              0.861      0.852           0.672          0.690
#> t_qaly_int               0.866      0.886           0.703          0.758
#> t_qaly_d_comp            0.851      0.844           0.671          0.689
#> t_qaly_d_int             0.855      0.876           0.697          0.753
#> t_costs_comp             0.730      0.703           0.309          0.353
#> t_costs_int              0.795      0.846           0.932          0.979
#> t_costs_d_comp           0.677      0.658           0.270          0.319
#> t_costs_d_int            0.787      0.839           0.931          0.978
#> t_ly_comp                1.000      0.982           0.667          0.700
#> t_ly_int                 0.988      1.000           0.700          0.765
#> t_ly_d_comp              1.000      0.986           0.671          0.707
#> t_ly_d_int               0.986      1.000           0.700          0.767
#> t_ly_pfs_d_comp          0.671      0.700           1.000          0.960
#> t_ly_pfs_d_int           0.707      0.767           0.960          1.000
#> t_ly_pd_d_comp           0.698      0.651          -0.062          0.024
#> t_ly_pd_d_int            0.675      0.625          -0.078         -0.022
#> t_qaly_pfs_d_comp        0.620      0.645           0.920          0.883
#> t_qaly_pfs_d_int         0.652      0.706           0.882          0.919
#> t_qaly_pd_d_comp         0.563      0.525          -0.043          0.025
#> t_qaly_pd_d_int          0.559      0.517          -0.057         -0.012
#> t_costs_pfs_d_comp       0.474      0.494           0.717          0.686
#> t_costs_pfs_d_int        0.703      0.762           0.955          0.995
#> t_costs_pd_d_comp        0.487      0.456          -0.093         -0.023
#> t_costs_pd_d_int         0.482      0.445          -0.109         -0.065
#> t_qaly_ae_int            0.008      0.010           0.024          0.024
#> t_costs_ae_int           0.013      0.014           0.030          0.028
#> inc_ly                   0.420      0.568           0.486          0.673
#> inc_qaly                 0.423      0.555           0.444          0.639
#> inc_costs                0.702      0.764           0.958          0.998
#>                    t_ly_pd_d_comp t_ly_pd_d_int t_qaly_pfs_d_comp
#> p_pfspd                     0.478         0.519            -0.655
#> p_pfsd                     -0.483        -0.509            -0.512
#> p_pdd                      -0.732        -0.676             0.007
#> p_dd                           NA            NA                NA
#> p_ae                       -0.015        -0.014             0.021
#> rr                          0.001         0.153            -0.013
#> u_pfs                       0.008         0.006             0.383
#> u_pd                        0.012         0.010             0.025
#> u_d                            NA            NA                NA
#> u_ae                       -0.004        -0.006             0.008
#> c_pfs                      -0.013        -0.013             0.001
#> c_pd                       -0.007        -0.009             0.000
#> c_d                            NA            NA                NA
#> c_thx                       0.002         0.001             0.007
#> c_ae                        0.000        -0.001             0.009
#> t_qaly_comp                 0.510         0.488             0.734
#> t_qaly_int                  0.488         0.457             0.770
#> t_qaly_d_comp               0.497         0.476             0.740
#> t_qaly_d_int                0.478         0.448             0.773
#> t_costs_comp                0.685         0.666             0.286
#> t_costs_int                 0.171         0.127             0.857
#> t_costs_d_comp              0.651         0.638             0.249
#> t_costs_d_int               0.159         0.117             0.856
#> t_ly_comp                   0.702         0.677             0.616
#> t_ly_int                    0.654         0.626             0.645
#> t_ly_d_comp                 0.698         0.675             0.620
#> t_ly_d_int                  0.651         0.625             0.645
#> t_ly_pfs_d_comp            -0.062        -0.078             0.920
#> t_ly_pfs_d_int              0.024        -0.022             0.883
#> t_ly_pd_d_comp              1.000         0.985            -0.054
#> t_ly_pd_d_int               0.985         1.000            -0.070
#> t_qaly_pfs_d_comp          -0.054        -0.070             1.000
#> t_qaly_pfs_d_int            0.026        -0.018             0.966
#> t_qaly_pd_d_comp            0.800         0.787            -0.031
#> t_qaly_pd_d_int             0.807         0.819            -0.045
#> t_costs_pfs_d_comp         -0.054        -0.065             0.660
#> t_costs_pfs_d_int           0.023        -0.023             0.878
#> t_costs_pd_d_comp           0.746         0.738            -0.085
#> t_costs_pd_d_int            0.754         0.772            -0.101
#> t_qaly_ae_int              -0.013        -0.014             0.022
#> t_costs_ae_int             -0.012        -0.013             0.022
#> inc_ly                      0.097         0.066             0.442
#> inc_qaly                    0.140         0.087             0.511
#> inc_costs                   0.020        -0.024             0.881
#>                    t_qaly_pfs_d_int t_qaly_pd_d_comp t_qaly_pd_d_int
#> p_pfspd                      -0.549            0.376           0.418
#> p_pfsd                       -0.587           -0.387          -0.418
#> p_pdd                         0.008           -0.588          -0.557
#> p_dd                             NA               NA              NA
#> p_ae                          0.017           -0.011          -0.011
#> rr                           -0.229           -0.002           0.122
#> u_pfs                         0.385            0.020           0.018
#> u_pd                          0.025            0.593           0.563
#> u_d                              NA               NA              NA
#> u_ae                          0.013            0.001          -0.002
#> c_pfs                        -0.002           -0.016          -0.016
#> c_pd                          0.001           -0.003          -0.004
#> c_d                              NA               NA              NA
#> c_thx                         0.005            0.003           0.002
#> c_ae                          0.014            0.011           0.009
#> t_qaly_comp                   0.749            0.656           0.637
#> t_qaly_int                    0.821            0.591           0.567
#> t_qaly_d_comp                 0.757            0.649           0.631
#> t_qaly_d_int                  0.825            0.586           0.563
#> t_costs_comp                  0.326            0.551           0.549
#> t_costs_int                   0.899            0.143           0.110
#> t_costs_d_comp                0.294            0.523           0.525
#> t_costs_d_int                 0.899            0.133           0.102
#> t_ly_comp                     0.646            0.567           0.560
#> t_ly_int                      0.705            0.528           0.518
#> t_ly_d_comp                   0.652            0.563           0.559
#> t_ly_d_int                    0.706            0.525           0.517
#> t_ly_pfs_d_comp               0.882           -0.043          -0.057
#> t_ly_pfs_d_int                0.919            0.025          -0.012
#> t_ly_pd_d_comp                0.026            0.800           0.807
#> t_ly_pd_d_int                -0.018            0.787           0.819
#> t_qaly_pfs_d_comp             0.966           -0.031          -0.045
#> t_qaly_pfs_d_int              1.000            0.032          -0.004
#> t_qaly_pd_d_comp              0.032            1.000           0.989
#> t_qaly_pd_d_int              -0.004            0.989           1.000
#> t_costs_pfs_d_comp            0.631           -0.042          -0.052
#> t_costs_pfs_d_int             0.914            0.024          -0.013
#> t_costs_pd_d_comp            -0.020            0.599           0.607
#> t_costs_pd_d_int             -0.060            0.605           0.634
#> t_qaly_ae_int                 0.022           -0.009          -0.010
#> t_costs_ae_int                0.021           -0.005          -0.006
#> inc_ly                        0.613            0.077           0.054
#> inc_qaly                      0.692           -0.006          -0.039
#> inc_costs                     0.917            0.022          -0.014
#>                    t_costs_pfs_d_comp t_costs_pfs_d_int t_costs_pd_d_comp
#> p_pfspd                        -0.518            -0.597             0.410
#> p_pfsd                         -0.399            -0.643            -0.361
#> p_pdd                           0.017             0.018            -0.524
#> p_dd                               NA                NA                NA
#> p_ae                            0.027             0.024            -0.015
#> rr                             -0.007            -0.247            -0.005
#> u_pfs                           0.002            -0.001             0.001
#> u_pd                            0.003             0.016             0.011
#> u_d                                NA                NA                NA
#> u_ae                           -0.001             0.009             0.004
#> c_pfs                           0.682             0.084            -0.014
#> c_pd                            0.000             0.004             0.639
#> c_d                                NA                NA                NA
#> c_thx                           0.002             0.049             0.004
#> c_ae                            0.005             0.020             0.004
#> t_qaly_comp                     0.475             0.685             0.347
#> t_qaly_int                      0.496             0.753             0.333
#> t_qaly_d_comp                   0.474             0.685             0.338
#> t_qaly_d_int                    0.492             0.748             0.327
#> t_costs_comp                    0.411             0.376             0.871
#> t_costs_int                     0.717             0.981             0.162
#> t_costs_d_comp                  0.425             0.347             0.870
#> t_costs_d_int                   0.726             0.984             0.156
#> t_ly_comp                       0.470             0.696             0.489
#> t_ly_int                        0.494             0.760             0.457
#> t_ly_d_comp                     0.474             0.703             0.487
#> t_ly_d_int                      0.494             0.762             0.456
#> t_ly_pfs_d_comp                 0.717             0.955            -0.093
#> t_ly_pfs_d_int                  0.686             0.995            -0.023
#> t_ly_pd_d_comp                 -0.054             0.023             0.746
#> t_ly_pd_d_int                  -0.065            -0.023             0.738
#> t_qaly_pfs_d_comp               0.660             0.878            -0.085
#> t_qaly_pfs_d_int                0.631             0.914            -0.020
#> t_qaly_pd_d_comp               -0.042             0.024             0.599
#> t_qaly_pd_d_int                -0.052            -0.013             0.607
#> t_costs_pfs_d_comp              1.000             0.745            -0.076
#> t_costs_pfs_d_int               0.745             1.000            -0.023
#> t_costs_pd_d_comp              -0.076            -0.023             1.000
#> t_costs_pd_d_int               -0.088            -0.066             0.988
#> t_qaly_ae_int                   0.022             0.024            -0.008
#> t_costs_ae_int                  0.027             0.029            -0.012
#> inc_ly                          0.341             0.668             0.076
#> inc_qaly                        0.312             0.635             0.104
#> inc_costs                       0.696             0.997            -0.037
#>                    t_costs_pd_d_int t_qaly_ae_int t_costs_ae_int inc_ly
#> p_pfspd                       0.451        -0.021         -0.028  0.042
#> p_pfsd                       -0.387        -0.008         -0.011 -0.741
#> p_pdd                        -0.497         0.006          0.003  0.333
#> p_dd                             NA            NA             NA     NA
#> p_ae                         -0.016         0.764          0.895  0.003
#> rr                            0.132        -0.005         -0.001 -0.474
#> u_pfs                        -0.001         0.000         -0.015 -0.011
#> u_pd                          0.010         0.005          0.012  0.005
#> u_d                              NA            NA             NA     NA
#> u_ae                          0.001         0.576         -0.019  0.019
#> c_pfs                        -0.014         0.005          0.009 -0.010
#> c_pd                          0.606         0.004         -0.002  0.002
#> c_d                              NA            NA             NA     NA
#> c_thx                         0.003         0.002         -0.005 -0.006
#> c_ae                          0.002        -0.001          0.409  0.027
#> t_qaly_comp                   0.338         0.011          0.014  0.378
#> t_qaly_int                    0.314         0.008          0.011  0.534
#> t_qaly_d_comp                 0.330         0.011          0.014  0.388
#> t_qaly_d_int                  0.309         0.007          0.010  0.538
#> t_costs_comp                  0.851         0.003          0.003  0.220
#> t_costs_int                   0.122         0.023          0.027  0.667
#> t_costs_d_comp                0.853         0.003          0.002  0.238
#> t_costs_d_int                 0.115         0.024          0.028  0.672
#> t_ly_comp                     0.482         0.008          0.012  0.401
#> t_ly_int                      0.444         0.010          0.014  0.555
#> t_ly_d_comp                   0.482         0.008          0.013  0.420
#> t_ly_d_int                    0.445         0.010          0.014  0.568
#> t_ly_pfs_d_comp              -0.109         0.024          0.030  0.486
#> t_ly_pfs_d_int               -0.065         0.024          0.028  0.673
#> t_ly_pd_d_comp                0.754        -0.013         -0.012  0.097
#> t_ly_pd_d_int                 0.772        -0.014         -0.013  0.066
#> t_qaly_pfs_d_comp            -0.101         0.022          0.022  0.442
#> t_qaly_pfs_d_int             -0.060         0.022          0.021  0.613
#> t_qaly_pd_d_comp              0.605        -0.009         -0.005  0.077
#> t_qaly_pd_d_int               0.634        -0.010         -0.006  0.054
#> t_costs_pfs_d_comp           -0.088         0.022          0.027  0.341
#> t_costs_pfs_d_int            -0.066         0.024          0.029  0.668
#> t_costs_pd_d_comp             0.988        -0.008         -0.012  0.076
#> t_costs_pd_d_int              1.000        -0.010         -0.013  0.041
#> t_qaly_ae_int                -0.010         1.000          0.683  0.014
#> t_costs_ae_int               -0.013         0.683          1.000  0.014
#> inc_ly                        0.041         0.014          0.014  1.000
#> inc_qaly                      0.051        -0.014         -0.011  0.921
#> inc_costs                    -0.077         0.025          0.030  0.681
#>                    inc_qaly inc_costs
#> p_pfspd               0.023    -0.590
#> p_pfsd               -0.658    -0.654
#> p_pdd                 0.219     0.032
#> p_dd                     NA        NA
#> p_ae                 -0.021     0.025
#> rr                   -0.548    -0.242
#> u_pfs                 0.257    -0.002
#> u_pd                 -0.192     0.016
#> u_d                      NA        NA
#> u_ae                  0.005     0.009
#> c_pfs                -0.009     0.012
#> c_pd                  0.001    -0.014
#> c_d                      NA        NA
#> c_thx                -0.006     0.053
#> c_ae                  0.019     0.021
#> t_qaly_comp           0.373     0.685
#> t_qaly_int            0.543     0.755
#> t_qaly_d_comp         0.384     0.685
#> t_qaly_d_int          0.550     0.750
#> t_costs_comp          0.239     0.343
#> t_costs_int           0.638     0.977
#> t_costs_d_comp        0.248     0.311
#> t_costs_d_int         0.641     0.978
#> t_ly_comp             0.407     0.695
#> t_ly_int              0.546     0.762
#> t_ly_d_comp           0.423     0.702
#> t_ly_d_int            0.555     0.764
#> t_ly_pfs_d_comp       0.444     0.958
#> t_ly_pfs_d_int        0.639     0.998
#> t_ly_pd_d_comp        0.140     0.020
#> t_ly_pd_d_int         0.087    -0.024
#> t_qaly_pfs_d_comp     0.511     0.881
#> t_qaly_pfs_d_int      0.692     0.917
#> t_qaly_pd_d_comp     -0.006     0.022
#> t_qaly_pd_d_int      -0.039    -0.014
#> t_costs_pfs_d_comp    0.312     0.696
#> t_costs_pfs_d_int     0.635     0.997
#> t_costs_pd_d_comp     0.104    -0.037
#> t_costs_pd_d_int      0.051    -0.077
#> t_qaly_ae_int        -0.014     0.025
#> t_costs_ae_int       -0.011     0.030
#> inc_ly                0.921     0.681
#> inc_qaly              1.000     0.644
#> inc_costs             0.644     1.000
```
