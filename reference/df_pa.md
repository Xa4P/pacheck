# Dataframe for testing

A dataframe containing 10,000 iterations of a probabilistic analysis of
a health state transition model.

## Usage

``` r
df_pa
```

## Format

A dataframe with 10,000 rows, each row being the inputs and
(intermediate) outputs of a single probabilistic iteration, and 44
variables:

- p_pfspd:

  Probability to transit from the progression-free survival (PFS) to
  progressed disease (PD) health state

- p_pfsd:

  Probability to transit from the PFS to Death (D) health state

- p_pdd:

  Probability to transit from the PD to D health state

- p_dd:

  Probability to transit from the D to D health state

- p_ae:

  Probability of occurence of an adverse event in the intervention
  stategy

- rr:

  Relative effectiveness of the treatment (\_int)

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
