#-------------------------------------#
#### VALIDATION FUNCTIONS PACBOARD ####
#-------------------------------------#

#--------------#
#### Set up ####
#--------------#
rm(list = ls())
options(scipen = 999)
library(pacheck)
source(paste(getwd(), "/R/fct_hemodel.R", sep = ""))
data(df_pa)
n_sim <- nrow(df_pa)
df_pa_orig <- df_pa
wtp <- 120000

# calculate (i)NMBs and i(NHB)s
df_pa_complete <- calculate_nb(df_pa_orig,
                               e_int = "t_qaly_d_int",
                               c_int = "t_costs_d_int",
                               e_comp = "t_qaly_d_comp",
                               c_comp = "t_costs_d_comp",
                               wtp = wtp)
# rescale inputs
df_pa_complete_rescaled <- df_pa_complete
v_names_inputs <- names(df_pa_complete)[-grep("t_", names(df_pa_complete))]
v_names_inputs <- v_names_inputs[-grep("inc_", v_names_inputs)]
v_names_inputs <- v_names_inputs[-grep("NMB", v_names_inputs)]
v_names_inputs <- v_names_inputs[-grep("NHB", v_names_inputs)]


df_pa_complete_rescaled[, v_names_inputs] <- apply(df_pa_complete_rescaled[, v_names_inputs], 2, function(x) {(x - mean(x)) / sd(x)})
#-------------------------#
##### INTRODUCE ERRORS ####
#-------------------------#
df_pa_error <- df_pa_complete

df_pa_error[c(1, 10, 600, 503, 8888), "u_pfs"]   <- -1  # negative utility values
df_pa_error[c(2, 20, 1200, 1006, 4444), "u_pd"]  <- 2   # utility values above 1

df_pa_error[c(3, 30, 1800, 1509, 2222), "p_pfspd"]   <- -1  # negative probabilities
df_pa_error[c(4, 40, 2400, 3560, 5555), "p_pdd"]     <- 2   # probabilities above 1

df_pa_error[c(10, 100, 6000, 5030, 888), "c_pd"] <- -1  # negative costs

df_pa_error[c(62, 543, 3684, 1253, 9999), "rr"]  <- -10  # negative relative risks

df_pa_error[c(620, 5430, 368, 1235, 999), "t_costs_pd_d_comp"]  <- 0   # discounted results > undiscounted results

df_pa_error[c(6200, 545, 3688, 1236, 9991), "t_qaly_d_comp"] <- -10 # negative total outcomes

#------------------------------#
##### CHECK FUNCTIONALITIES ####
#------------------------------#
# Quick check function ----
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

# Check sum probabilities ----
df_pa_high_p <- df_pa
df_pa_high_p[c(1, 100, 1000, 10000), "p_pfspd"] <- 1
res_sum_probs_no_error_sum <- check_sum_probs(c("p_pfspd", "p_pfsd"), df = df_pa, digits = NULL, check = "lower", max_view = 100) # ok!
res_sum_probs_error_equal <- check_sum_probs(c("p_pfspd", "p_pfsd"), df = df_pa, digits = 3, check = "equal", max_view = 100) # ok because pfspfs not taken into account!
res_sum_probs_error_sum <- check_sum_probs(c("p_pfspd", "p_pfsd"), df = df_pa_high_p, digits = NULL, check = "lower", max_view = 100) # ok!

# Check positive values ----
res_check_pos_no_error <- check_positive(c("c_pfs", "c_pd", "u_pfs", "u_pd", "c_ae", "u_ae", "t_qaly_d_comp", "t_qaly_d_int", "t_costs_d_comp", "t_costs_d_int"), df = df_pa) # ok!
res_check_pos_error    <- check_positive(c("c_pfs", "c_pd", "u_pfs", "u_pd", "c_ae", "u_ae", "t_qaly_d_comp", "t_qaly_d_int", "t_costs_d_comp", "t_costs_d_int"), df = df_pa_error) # ok!

# Check range ----
res_check_range_binary_no_error <- check_range(df = df_pa,
                                               outcome = "u_pfs",
                                               min_val = 0,
                                               max_val = 1) # ok!
res_check_range_below_no_error <- check_range(df = df_pa,
                                               outcome = "u_pfs",
                                               #min_val = 0,
                                               max_val = 1) # ok!
res_check_range_above_no_error <- check_range(df = df_pa,
                                              outcome = "u_pfs",
                                              min_val = 0) # ok!

res_check_range_binary_error <- check_range(df = df_pa_error,
                                               outcome = "u_pfs",
                                               min_val = 0,
                                               max_val = 1) # ok!
res_check_range_below_error <- check_range(df = df_pa_error,
                                              outcome = "u_pfs",
                                              #min_val = 0,
                                              max_val = 1) # ok!
res_check_range_above_error <- check_range(df = df_pa_error,
                                              outcome = "u_pfs",
                                              min_val = 0) # ok!

# Check binary ----
res_check_bin_no_error <- check_binary(c("u_pfs", "u_pd"), df = df_pa)
res_check_bin_error <- check_binary(c("u_pfs", "u_pd"), df = df_pa_error)

# Check mean QALY ----
df_pa$t_qaly_int_check <- df_pa$t_qaly_pfs_d_int + df_pa$t_qaly_pd_d_int # only consider QALYs won in health states fpr this check
df_pa$t_qaly_int_check_error_below <- df_pa$t_qaly_int_check
df_pa[c(3, 333, 3333), "t_qaly_int_check_error_below"] <-  df_pa[c(3, 333, 3333), "t_qaly_int_check_error_below"] - 3
df_pa$t_qaly_int_check_error_above <- df_pa$t_qaly_int_check
df_pa[c(3, 333, 3333), "t_qaly_int_check_error_above"] <-  df_pa[c(3, 333, 3333), "t_qaly_int_check_error_above"] + 3

res_check_mean_qol_no_error <- check_mean_qol(df = df_pa,
                                              t_qaly = "t_qaly_int_check",
                                              t_ly = "t_ly_d_int",
                                              u_values = c("u_pfs", "u_pd"))
res_check_mean_qol_below_error <- check_mean_qol(df = df_pa,
                                                 t_qaly = "t_qaly_int_check_error_below",
                                                 t_ly = "t_ly_d_int",
                                                 u_values = c("u_pfs", "u_pd"))
res_check_mean_qol_above_error <- check_mean_qol(df = df_pa,
                                                 t_qaly = "t_qaly_int_check_error_above",
                                                 t_ly = "t_ly_d_int",
                                                 u_values = c("u_pfs", "u_pd"))
#res_check_mean_qol_no_error # ok
#res_check_mean_qol_below_error # ok
#res_check_mean_qol_above_error # ok

#----------------------------------------#
#### COMPARISON METAMODEL VS ORIGINAL ####
#----------------------------------------#
# Fit metamodel ----
v_inputs_short <- c("p_pfspd",
                    "p_pfsd",
                    "p_pdd",
                    #"p_dd",
                    "p_ae",
                    "rr",
                    "u_pfs",
                    "u_pd",
                    #"u_d",
                    "u_ae",
                    "c_pfs",
                    #"c_pd",
                    #"c_d",
                    "c_thx"#,
                    #"c_ae"
)

# Full factorial
lm_valid <- fit_lm_metamodel(df = df_pa_complete,
                             y = "iNMB",
                             x = v_names_inputs,
                             partition = 0.75
                             )
summary(lm_valid)
lm_valid_2 <- fit_lm_metamodel(df = df_pa,
                             y = "iNMB",
                             x = c("p_pfspd",
                                   "p_pfsd",
                                   "p_pdd",
                                   #"p_dd",
                                   "p_ae",
                                   "rr",
                                   "u_pfs",
                                   "u_pd",
                                   #"u_d",
                                   "u_ae",
                                   "c_pfs",
                                   #"c_pd",
                                   #"c_d",
                                   "c_thx"#,
                                   #"c_ae"
                             ))
#summary(lm_valid)$adj.r.squared
#summary(lm_valid_2)$adj.r.squared
## does not really matter!

# One-way deterministic analyses ----
## Original model
df_res_dowsa <- perform_dowsa(df = df_pa_orig,
                               vars = v_inputs_all
                              )
tornado_dowsa <- plot_tornado(df = df_res_dowsa,
                                   df_basecase = df_pa,
                                   outcome = "iNMB")

## Metamodel - full factorial
df_res_dowsa_meta <- dsa_lm_metamodel(df = df_pa,
                                      lm_metamodel = lm_valid)
tornado_dowsa_meta <- plot_tornado(df = df_res_dowsa_meta,
                                   df_basecase = df_pa,
                                   outcome = "iNMB")

## Metamodel - parsimonious


# Other predictions ----
## Original model
### No difference in effectiveness (rr = 1)
df_pred_no_diff_e <- df_pa_orig
df_pred_no_diff_e$rr <- 1

## Perform PA
### Initialise matrix outcomes
m_res_pa <- matrix(0,
                   ncol = 26,
                   nrow = n_sim,
                   dimnames = list(c(1:n_sim),
                                   c("t_qaly_comp",
                                     "t_qaly_int",
                                     "t_qaly_d_comp",
                                     "t_qaly_d_int",
                                     "t_costs_comp",
                                     "t_costs_int",
                                     "t_costs_d_comp",
                                     "t_costs_d_int",
                                     "t_ly_comp",
                                     "t_ly_int",
                                     "t_ly_d_comp",
                                     "t_ly_d_int",
                                     "t_ly_pfs_d_comp",
                                     "t_ly_pfs_d_int",
                                     "t_ly_pd_d_comp",
                                     "t_ly_pd_d_int",
                                     "t_qaly_pfs_d_comp",
                                     "t_qaly_pfs_d_int",
                                     "t_qaly_pd_d_comp",
                                     "t_qaly_pd_d_int",
                                     "t_costs_pfs_d_comp",
                                     "t_costs_pfs_d_int",
                                     "t_costs_pd_d_comp",
                                     "t_costs_pd_d_int",
                                     "t_qaly_ae_int",
                                     "t_costs_ae_int")))
for(i in 1:n_sim) {

  l_params_temp <- as.list(df_pred_no_diff_e[i, ])

  m_res_pa[i, ] <- perform_simulation(l_params = l_params_temp,
                                      verbose = FALSE)
}

df_pa_no_eff_diff <- as.data.frame(m_res_pa)
#df_pa_no_eff_diff <- cbind(df_pred_no_diff_e, df_pa_no_eff_diff)
df_pa_no_eff_diff$inc_ly <- df_pa_no_eff_diff$t_ly_d_int - df_pa_no_eff_diff$t_ly_d_comp
df_pa_no_eff_diff$inc_qaly <- df_pa_no_eff_diff$t_qaly_d_int - df_pa_no_eff_diff$t_qaly_d_comp
df_pa_no_eff_diff$inc_costs <- df_pa_no_eff_diff$t_costs_d_int - df_pa_no_eff_diff$t_costs_d_comp
df_pa_no_eff_diff$iNMB <- df_pa_no_eff_diff$inc_qaly * wtp - df_pa_no_eff_diff$inc_costs

### Costs treatment is 0 (c_thx = 0)
df_pred_no_c_thx <- df_pa_orig
df_pred_no_c_thx$c_thx <- rep(0, nrow(df_pred_no_c_thx))

## Perform PA
### Initialise matrix outcomes
m_res_pa <- matrix(0,
                   ncol = 26,
                   nrow = n_sim,
                   dimnames = list(c(1:n_sim),
                                   c("t_qaly_comp",
                                     "t_qaly_int",
                                     "t_qaly_d_comp",
                                     "t_qaly_d_int",
                                     "t_costs_comp",
                                     "t_costs_int",
                                     "t_costs_d_comp",
                                     "t_costs_d_int",
                                     "t_ly_comp",
                                     "t_ly_int",
                                     "t_ly_d_comp",
                                     "t_ly_d_int",
                                     "t_ly_pfs_d_comp",
                                     "t_ly_pfs_d_int",
                                     "t_ly_pd_d_comp",
                                     "t_ly_pd_d_int",
                                     "t_qaly_pfs_d_comp",
                                     "t_qaly_pfs_d_int",
                                     "t_qaly_pd_d_comp",
                                     "t_qaly_pd_d_int",
                                     "t_costs_pfs_d_comp",
                                     "t_costs_pfs_d_int",
                                     "t_costs_pd_d_comp",
                                     "t_costs_pd_d_int",
                                     "t_qaly_ae_int",
                                     "t_costs_ae_int")))
for(i in 1:n_sim) {

  l_params_temp <- as.list(df_pred_no_c_thx[i, ])

  m_res_pa[i, ] <- perform_simulation(l_params = l_params_temp,
                                      verbose = FALSE)
}

df_pa_no_c_thx <- as.data.frame(m_res_pa)
#df_pred_no_c_thx <- cbind(df_pred_no_diff_e, df_pred_no_c_thx)
df_pa_no_c_thx$inc_ly <- df_pa_no_c_thx$t_ly_d_int - df_pa_no_c_thx$t_ly_d_comp
df_pa_no_c_thx$inc_qaly <- df_pa_no_c_thx$t_qaly_d_int - df_pa_no_c_thx$t_qaly_d_comp
df_pa_no_c_thx$inc_costs <- df_pa_no_c_thx$t_costs_d_int - df_pa_no_c_thx$t_costs_d_comp
df_pa_no_c_thx$iNMB <- df_pa_no_c_thx$inc_qaly * wtp - df_pa_no_c_thx$inc_costs

## Metamodel
### No difference in effectiveness (rr = 1)
res_meta_no_eff_diff <- predict.lm(lm_valid, newdata = df_pred_no_diff_e[, v_inputs_all])
res_meta_no_eff_diff_2 <- predict.lm(lm_valid_2, newdata = df_pred_no_diff_e[, v_inputs_short])

### Costs treatment is 0 (c_thx = 0)
res_meta_no_c_thx <- predict.lm(lm_valid, newdata = df_pred_no_c_thx[, v_inputs_all])
res_meta_no_c_thx_2 <- predict.lm(lm_valid_2, newdata = df_pred_no_c_thx[, v_inputs_short])

## Comparison of results
### No difference in effectiveness
summary(df_pa_no_eff_diff$iNMB)
summary(res_meta_no_eff_diff)
summary(res_meta_no_eff_diff_2)

df_1 <- data.frame(res_meta_no_eff_diff)
names(df_1) <- "iNMB"

check_range(df = df_1,
            outcome = "iNMB",
            min_val = unname(round(quantile(df_pa_no_eff_diff$iNMB, c(0.025)))),
            max_val = unname(round(quantile(df_pa_no_eff_diff$iNMB, c(0.975))))
            )

sqrt(mean((df_pa_no_eff_diff$iNMB - df_1$iNMB) ^ 2)) #RMSE
mean(abs(df_pa_no_eff_diff$iNMB - df_1$iNMB)) #mean absolute error
mean(abs(df_pa_no_eff_diff$iNMB - df_1$iNMB) / abs(df_pa_no_eff_diff$iNMB)) # mean relative diff
qqplot(x = df_pa_no_eff_diff$iNMB,
       y = df_1$iNMB)

summary(abs(df_pa_no_eff_diff$iNMB - df_1$iNMB) / abs(df_pa_no_eff_diff$iNMB))

hist(df_pa_no_eff_diff$iNMB)
hist(res_meta_no_eff_diff)
hist(res_meta_no_eff_diff_2)y
plot(density(df_pa_no_eff_diff$iNMB),
     ylim = c(0, 0.0001)
)
lines(density(df_1$iNMB), col = "red")

## No treatment costs
summary(df_pa_no_c_thx$iNMB)
summary(res_meta_no_c_thx)
summary(res_meta_no_c_thx_2)

df_2 <- data.frame(res_meta_no_c_thx)
names(df_2) <- "iNMB"

check_range(df = df_2,
            outcome = "iNMB",
            min_val = unname(round(quantile(df_pa_no_c_thx$iNMB, c(0.025)))),
            max_val = unname(round(quantile(df_pa_no_c_thx$iNMB, c(0.975))))
)

sqrt(mean((df_pa_no_c_thx$iNMB - df_2$iNMB) ^ 2)) # RMSE
mean(abs(df_pa_no_c_thx$iNMB - df_2$iNMB)) # mean absolute error
mean(abs(df_pa_no_c_thx$iNMB - df_2$iNMB) / abs(df_pa_no_c_thx$iNMB)) #relative error
qqplot(x = df_pa_no_c_thx$iNMB,
       y = df_2$iNMB)

hist(df_pa_no_c_thx$iNMB)
hist(df_2$iNMB)
hist(res_meta_no_c_thx_2)
plot(density(df_pa_no_c_thx$iNMB),
     ylim = c(0, 0.00003)
     )
lines(density(df_2$iNMB), col = "red")

#--------------#
#### EXPORT ####
#--------------#
# Data sets for PACBOARD ----
write.csv(df_pa_complete, file = paste(getwd(), "/data/data_with_nb.csv", sep = ""))
write.csv(df_pa_complete_rescaled, file = paste(getwd(), "/data/data_with_nb_rescaled.csv", sep = ""))
write.csv(df_pa_error, file = paste(getwd(), "/data/data_with_nb_error.csv", sep = ""))

# Tornado's ----
png(paste(getwd(), "/figs/tornado_original.png", sep = ""))
print(tornado_dowsa)
dev.off()

png(paste(getwd(), "/figs/tornado_original_meta.png", sep = ""))
print(tornado_dowsa_meta)
dev.off()
