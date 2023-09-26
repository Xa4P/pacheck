#' Dataframe for testing
#' A dataframe containing 10,000 iterations of a probabilistic analysis.
#' @format A dataframe with 10,000 rows, each row being the inputs and (intermediate) outputs of a single probabilistic iteration, and 35 variables:
#' \describe{
#'   \item{p_pfspd}{Probability to transit from the progression-free survival (PFS) to progressed disease (PD) health state}
#'   \item{p_pfsd}{Probability to transit from the PFS to Death (D) health state}
#'   \item{p_pdd}{Probability to transit from the PD to D health state}
#'   \item{p_dd}{Probability to transit from the D to D health state}
#'   \item{rr}{Relative effectiveness of the treatment (_int)}
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
"df_pa"
