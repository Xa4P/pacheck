% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doc_df_pa.R
\docType{data}
\name{df_pa}
\alias{df_pa}
\title{Dataframe for testing}
\format{
A dataframe with 10,000 rows, each row being the inputs and (intermediate) outputs of a single probabilistic iteration, and 35 variables:
\describe{
  \item{p_pfspd}{Probability to transit from the progression-free survival (PFS) to progressed disease (PD) health state}
  \item{p_pfsd}{Probability to transit from the PFS to Death (D) health state}
  \item{p_pdd}{Probability to transit from the PD to D health state}
  \item{p_dd}{Probability to transit from the D to D health state}
  \item{rr}{Relative effectiveness of the treatment (_int)}
  \item{u_pfs}{Utility value (per cycle) associated with PFS health state}
  \item{u_pd}{Utility value (per cycle) associated with PD health state}
  \item{u_d}{Utility value (per cycle) associated with D health state}
  \item{c_pfs}{Costs (per cycle) associated with PFS health state}
  \item{c_pd}{Costs (per cycle) associated with PD health state}
  \item{c_d}{Costs (per cycle) associated with D health state}
  \item{c_thx}{Costs (per cycle) associated receiving treatment, in the PFS health state}
  \item{QALY_comp}{Total QALY obtained with the comparator, i.e. no treatment administered}
  \item{QALY_int}{Total QALY obtained with the intervention, i.e. treatment administered}
  \item{Costs_comp}{Total costs obtained with the comparator, i.e. no treatment administered}
  \item{Costs_int}{Total costs obtained with the intervention, i.e. treatment administered}
  \item{LY_pfs_comp}{Life years accrued in PFS health state, comparator strategy}
  \item{LY_pfs_int}{Life years accrued in PFS health state, intervention strategy}
  \item{LY_pd_comp}{Life years accrued in PD health state, comparator strategy}
  \item{LY_pd_int}{Life years accrued in PD health state, intervention strategy}
  \item{QALY_pfs_comp}{Quality-adjusted life years accrued in PFS health state, comparator strategy}
  \item{QALY_pfs_int}{Quality-adjusted life years accrued in PFS health state, intervention strategy}
  \item{QALY_pd_comp}{Quality-adjusted life years accrued in PD health state, comparator strategy}
  \item{QALY_pd_int}{Quality-adjusted life years accrued in PD health state, intervention strategy}
  \item{Costs_pfs_comp}{Costs accrued in PFS health state, comparator strategy}
  \item{Costs_pfs_int}{Costs accrued in PFS health state, intervention strategy}
  \item{Costs_pd_comp}{Costs accrued in PD health state, comparator strategy}
  \item{Costs_pd_int}{Costs accrued in PD health state, intervention strategy}
  \item{QALY_ae_int}{Quality-adjusted life year impact of the occurence of adverse events, intervention strategy}
  \item{Costs_ae_int}{Costs associated with the occurence of adverse events, intervention strategy}
  \item{Inc_QALY}{Incremental QALYs obtained with the intervention versus the comparator}
  \item{Inc_Costs}{Incremental costs obtained with the intervention versus the comparator}
}
}
\usage{
df_pa
}
\description{
A dataframe containing 10,000 iterations of a probabilistic analysis.
}
\keyword{datasets}