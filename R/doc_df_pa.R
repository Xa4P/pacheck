#' Dataframe for testing
#'
#' A dataframe containing 10,000 iterations of a probabilistic analysis.
#'
#'
#' @format A dataframe with 10,000 rows, each row being an input or output of a single probabilistic iteration, and 18 variables:
#' \describe{
#'   \item{p_pfspd}{Probability to transit from the progression-free survival (PFS) to progressed disease (PD) health state}
#'   \item{p_pfsd}{Probability to transit from the PFS to Death (D) health state}
#'   \item{p_pdd}{Probability to transit from the PD to D health state}
#'   \item{p_dd}{Probability to transit from the D to D health state}
#'   \item{rr}{Relative effectiveness of the treatment (_int)}
#'   \item{u_pfs}{Utility value (per cycle) associated with PFS health state}
#'   \item{u_pd}{Utility value (per cycle) associated with PD health state}
#'   \item{u_d}{Utility value (per cycle) associated with D health state}
#'   \item{c_pfs}{Costs (per cycle) associated with PFS health state}
#'   \item{c_pd}{Costs (per cycle) associated with PD health state}
#'   \item{c_d}{Costs (per cycle) associated with D health state}
#'   \item{c_thx}{Costs (per cycle) associated receiving treatment, in the PFS health state}
#'   \item{QALY_comp}{Total QALY obtained with the comparator, i.e. no treatment administered}
#'   \item{QALY_int}{Total QALY obtained with the intervention, i.e. treatment administered}
#'   \item{Costs_comp}{Total costs obtained with the comparator, i.e. no treatment administered}
#'   \item{Costs_int}{Total costs obtained with the intervention, i.e. treatment administered}
#'   \item{Inc_QALY}{Incremental QALYs obtained with the intervention versus the comparator}
#'   \item{Inc_Costs}{Incremental costs obtained with the intervention versus the comparator}
#' }
"df_pa"
