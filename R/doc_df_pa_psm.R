#' Dataframe for testing
#'
#' A dataframe containing 10,000 iterations of a probabilistic analysis of a partitioned survival model.
#'
#' @format A dataframe with 10,000 rows, each row being the inputs and (intermediate) outputs of a single probabilistic iteration, and 46 variables:
#' \describe{
#'   \item{p_ae}{Probability of occurence of an adverse event in the intervention stategy}
#'   \item{r_exp_pfs_comp}{Rate of the exponential survival model used to estimate PFS of the comparator}
#'   \item{rr_thx_pfs}{Relative risk of the occurrence of progression of the intervention versus the comparator, used to estimate PFS of the intervention}
#'   \item{r_exp_pfs_int}{Rate of the exponential survival model used to estimate PFS of the intervention}
#'   \item{shape_weib_os}{Shape of the Weibull survival model used to estimate OS of the comparator and intervention}
#'   \item{scale_weib_os_comp}{Scale of the Weibull survival model used to estimate OS of the comparator}
#'   \item{rr_thx_os}{Relative risk of the occurrence of death of the intervention versus the comparator, used to estimate PFS of the intervention}
#'   \item{scale_weib_os_int}{Scale of the Weibull survival model used to estimate OS of the intervention}
#'   \item{u_pfs}{Utility value (per cycle) associated with PFS health state}
#'   \item{u_pd}{Utility value (per cycle) associated with PD health state}
#'   \item{u_d}{Utility value (per cycle) associated with D health state}
#'   \item{u_ae}{Utility decrement associated with the occurrence of an adverse event}
#'   \item{c_pfs}{Costs (per cycle) associated with PFS health state}
#'   \item{c_pd}{Costs (per cycle) associated with PD health state}
#'   \item{c_d}{Costs (per cycle) associated with D health state}
#'   \item{c_thx}{Costs (per cycle) associated with receiving treatment, in the PFS health state}
#'   \item{c_ae}{Costs associated with experiencing an adverse event}
#'   \item{t_qaly_comp}{Total undiscounted QALY obtained with the comparator, i.e. no treatment administered}
#'   \item{t_qaly_int}{Total undiscounted QALY obtained with the intervention, i.e. treatment administered}
#'   \item{t_qaly_d_comp}{Total discounted QALY obtained with the comparator, i.e. no treatment administered}
#'   \item{t_qaly_d_int}{Total discounted QALY obtained with the intervention, i.e. treatment administered}
#'   \item{t_costs_comp}{Total undiscounted costs obtained with the comparator, i.e. no treatment administered}
#'   \item{t_costs_int}{Total undiscounted costs obtained with the intervention, i.e. treatment administered}
#'   \item{t_costs_d_comp}{Total discounted costs obtained with the comparator, i.e. no treatment administered}
#'   \item{t_costs_d_int}{Total discounted costs obtained with the intervention, i.e. treatment administered}
#'   \item{t_ly_comp}{Total undiscounted LY obtained with the comparator, i.e. no treatment administered}
#'   \item{t_ly_int}{Total undiscounted LY obtained with the intervention, i.e. treatment administered}
#'   \item{t_ly_d_comp}{Total discounted LY obtained with the comparator, i.e. no treatment administered}
#'   \item{t_ly_d_int}{Total discounted LY obtained with the intervention, i.e. treatment administered}
#'   \item{t_ly_pfs_d_comp}{Total discounted life years accrued in PFS health state, comparator strategy}
#'   \item{t_ly_pfs_d_int}{Total discounted life years accrued in PFS health state, intervention strategy}
#'   \item{t_ly_pd_d_comp}{Total discounted life years accrued in PD health state, comparator strategy}
#'   \item{t_ly_pd_d_int}{Total discounted life years accrued in PD health state, intervention strategy}
#'   \item{t_qaly_pfs_d_comp}{Total discounted quality-adjusted life years accrued in PFS health state, comparator strategy}
#'   \item{t_qaly_pfs_d_int}{Total discounted quality-adjusted life years accrued in PFS health state, intervention strategy}
#'   \item{t_qaly_pd_d_comp}{Total discounted quality-adjusted life years accrued in PD health state, comparator strategy}
#'   \item{t_qaly_pd_d_int}{Total discounted quality-adjusted life years accrued in PD health state, intervention strategy}
#'   \item{t_costs_pfs_d_comp}{Total discounted costs accrued in PFS health state, comparator strategy}
#'   \item{t_costs_pfs_d_int}{Total discounted costs accrued in PFS health state, intervention strategy}
#'   \item{t_costs_pd_d_comp}{Total discounted costs accrued in PD health state, comparator strategy}
#'   \item{t_costs_pd_d_int}{Total discounted costs accrued in PD health state, intervention strategy}
#'   \item{t_qaly_ae_int}{Quality-adjusted life year decrement associated with the occurence of adverse events, intervention strategy}
#'   \item{t_costs_ae_int}{Costs associated with the occurence of adverse events, intervention strategy}
#'   \item{inc_ly}{Incremental QALYs obtained with the intervention versus the comparator}
#'   \item{inc_qaly}{Incremental QALYs obtained with the intervention versus the comparator}
#'   \item{inc_costs}{Incremental costs obtained with the intervention versus the comparator}
#' }
"df_pa_psm"
