#-------------------------------------#
#### VALIDATION FUNCTIONS PACBOARD ####
#-------------------------------------#

#--------------#
#### Set up ####
#--------------#
rm(list = ls())
library(pacheck)
data(df_pa)

#------------------------------------#
##### Introduce errors in PA data ####
#------------------------------------#
df_pa_error <- df_pa

df_pa_error[c(1, 10, 600, 503, 8888), "u_pfs"]   <- -1  # negative utility values
df_pa_error[c(2, 20, 1200, 1006, 4444), "u_pd"]  <- 2   # utility values above 1

df_pa_error[c(3, 30, 1800, 1509, 2222), "p_pfspd"]   <- -1  # negative probabilities
df_pa_error[c(4, 40, 2400, 3560, 5555), "p_pdd"]     <- 2   # probabilities above 1

df_pa_error[c(10, 100, 6000, 5030, 888), "c_pd"] <- -1  # negative costs

df_pa_error[c(62, 543, 3684, 1253, 9999), "rr"]  <- -10  # negative relative risks

df_pa_error[c(620, 5430, 368, 1235, 999), "t_costs_pd_d_comp"]  <- 0   # discounted results > undiscounted results

df_pa_error[c(6200, 545, 3688, 1236, 9991), "t_qaly_d_comp"] <- -10 # negative total outcomes

#----------------------------------------------------#
##### Check whether errors are identified PA data ####
#----------------------------------------------------#
# Quick check function
res_quick_check <- do_quick_check(df_pa_error,
                                   v_probs = c("p_pfspd", "p_pfsd", "p_pdd"),
                                   v_utilities = c("u_pfs", "u_pd", "u_ae"),
                                   v_costs = c("c_pfs", "c_pd", "c_ae", "c_thx"),
                                   v_hr = NULL,
                                   v_rr = "rr",
                                   v_r = NULL,
                                   v_outcomes = c("t_qaly_d_comp", "t_qaly_d_int", "t_costs_d_comp", "t_costs_d_int")
                                  )

check_all_false <- testthat::test_that("Results are as expected",
                                       {for (i in c(1, 2, 3, 4, 5, 7, 9)){
                                         testthat::expect_equal(res_quick_check[i, 2], "FALSE")
                                         }
                                         }) # correct, all issues identified!

# Check sum probabilities
df_pa_high_p <- df_pa
df_pa_high_p[c(1, 100, 1000, 10000), "p_pfspd"] <- 1
res_sum_probs_no_error_sum <- check_sum_probs(c("p_pfspd", "p_pfsd"), df = df_pa, digits = NULL, check = "lower", max_view = 100) # ok!
res_sum_probs_error_equal <- check_sum_probs(c("p_pfspd", "p_pfsd"), df = df_pa, digits = 3, check = "equal", max_view = 100) # ok because pfspfs not taken into account!
res_sum_probs_error_sum <- check_sum_probs(c("p_pfspd", "p_pfsd"), df = df_pa_high_p, digits = NULL, check = "lower", max_view = 100) # ok!


#---------------------------------------------------#
#### Comparison metamodel vs original prediction ####
#---------------------------------------------------#
