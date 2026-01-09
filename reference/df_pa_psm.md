# Dataframe for testing

A dataframe containing 10,000 iterations of a probabilistic analysis of
a partitioned survival model. To access the original dataframe used in
the scientific publication of PACBOARD (Pouwels et al. 2024), follow the
link below.

## Usage

``` r
df_pa_psm
```

## Format

A dataframe with 10,000 rows, each row being the inputs and
(intermediate) outputs of a single probabilistic iteration, and 46
variables:

- p_ae:

  Probability of occurence of an adverse event in the intervention
  stategy

- r_exp_pfs_comp:

  Rate of the exponential survival model used to estimate PFS of the
  comparator

- rr_thx_pfs:

  Relative risk of the occurrence of progression of the intervention
  versus the comparator, used to estimate PFS of the intervention

- r_exp_pfs_int:

  Rate of the exponential survival model used to estimate PFS of the
  intervention

- shape_weib_os:

  Shape of the Weibull survival model used to estimate OS of the
  comparator and intervention

- scale_weib_os_comp:

  Scale of the Weibull survival model used to estimate OS of the
  comparator

- rr_thx_os:

  Relative risk of the occurrence of death of the intervention versus
  the comparator, used to estimate PFS of the intervention

- scale_weib_os_int:

  Scale of the Weibull survival model used to estimate OS of the
  intervention

- u_pfs:

  Utility value (per cycle) associated with PFS health state

- u_pd:

  Utility value (per cycle) associated with PD health state

- u_d:

  Utility value (per cycle) associated with D health state

- u_ae:

  Utility decrement associated with the occurrence of an adverse event

- c_pfs:

  Costs (per cycle) associated with PFS health state

- c_pd:

  Costs (per cycle) associated with PD health state

- c_d:

  Costs (per cycle) associated with D health state

- c_thx:

  Costs (per cycle) associated with receiving treatment, in the PFS
  health state

- c_ae:

  Costs associated with experiencing an adverse event

- t_qaly_comp:

  Total undiscounted QALY obtained with the comparator, i.e. no
  treatment administered

- t_qaly_int:

  Total undiscounted QALY obtained with the intervention, i.e. treatment
  administered

- t_qaly_d_comp:

  Total discounted QALY obtained with the comparator, i.e. no treatment
  administered

- t_qaly_d_int:

  Total discounted QALY obtained with the intervention, i.e. treatment
  administered

- t_costs_comp:

  Total undiscounted costs obtained with the comparator, i.e. no
  treatment administered

- t_costs_int:

  Total undiscounted costs obtained with the intervention, i.e.
  treatment administered

- t_costs_d_comp:

  Total discounted costs obtained with the comparator, i.e. no treatment
  administered

- t_costs_d_int:

  Total discounted costs obtained with the intervention, i.e. treatment
  administered

- t_ly_comp:

  Total undiscounted LY obtained with the comparator, i.e. no treatment
  administered

- t_ly_int:

  Total undiscounted LY obtained with the intervention, i.e. treatment
  administered

- t_ly_d_comp:

  Total discounted LY obtained with the comparator, i.e. no treatment
  administered

- t_ly_d_int:

  Total discounted LY obtained with the intervention, i.e. treatment
  administered

- t_ly_pfs_d_comp:

  Total discounted life years accrued in PFS health state, comparator
  strategy

- t_ly_pfs_d_int:

  Total discounted life years accrued in PFS health state, intervention
  strategy

- t_ly_pd_d_comp:

  Total discounted life years accrued in PD health state, comparator
  strategy

- t_ly_pd_d_int:

  Total discounted life years accrued in PD health state, intervention
  strategy

- t_qaly_pfs_d_comp:

  Total discounted quality-adjusted life years accrued in PFS health
  state, comparator strategy

- t_qaly_pfs_d_int:

  Total discounted quality-adjusted life years accrued in PFS health
  state, intervention strategy

- t_qaly_pd_d_comp:

  Total discounted quality-adjusted life years accrued in PD health
  state, comparator strategy

- t_qaly_pd_d_int:

  Total discounted quality-adjusted life years accrued in PD health
  state, intervention strategy

- t_costs_pfs_d_comp:

  Total discounted costs accrued in PFS health state, comparator
  strategy

- t_costs_pfs_d_int:

  Total discounted costs accrued in PFS health state, intervention
  strategy

- t_costs_pd_d_comp:

  Total discounted costs accrued in PD health state, comparator strategy

- t_costs_pd_d_int:

  Total discounted costs accrued in PD health state, intervention
  strategy

- t_qaly_ae_int:

  Quality-adjusted life year decrement associated with the occurence of
  adverse events, intervention strategy

- t_costs_ae_int:

  Costs associated with the occurence of adverse events, intervention
  strategy

- inc_ly:

  Incremental QALYs obtained with the intervention versus the comparator

- inc_qaly:

  Incremental QALYs obtained with the intervention versus the comparator

- inc_costs:

  Incremental costs obtained with the intervention versus the comparator

## Source

Pouwels XGLV, Kroeze K, van der Linden N, Kip MMA, Koffijberg H.
Validating Health Economic Models With the Probabilistic Analysis Check
dashBOARD. Value Health. 2024 Aug;27(8):1073-1084. doi:
10.1016/j.jval.2024.04.008.

Link to the original data ("df_pa_psm") used in the PACBOARD
publication: <https://github.com/Xa4P/pacheck/tree/master/data-raw>
