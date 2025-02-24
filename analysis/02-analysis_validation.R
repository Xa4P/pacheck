#-------------------------------------#
#### VALIDATION FUNCTIONS PACBOARD ####
#-------------------------------------#

#--------------#
#### Set up ####
#--------------#
rm(list = ls())
options(scipen = 999)
devtools::load_all()

data(df_pa)
data(df_pa_psm)
n_sim <- nrow(df_pa)
set.seed(500)
selection <- sample(1:nrow(df_pa), size = round(nrow(df_pa) * 0.75), replace = FALSE)
df_pa_orig <- df_pa[selection, ]
df_pa_validation <- df_pa[-selection, ]
wtp <- 80000

# calculate (i)NMBs and i(NHB)s
df_pa_complete <- calculate_nb(df_pa_orig,
                               e_int = "t_qaly_d_int",
                               c_int = "t_costs_d_int",
                               e_comp = "t_qaly_d_comp",
                               c_comp = "t_costs_d_comp",
                               wtp = wtp)


df_pa_validation_complete <- calculate_nb(df_pa_validation,
                                          e_int = "t_qaly_d_int",
                                          c_int = "t_costs_d_int",
                                          e_comp = "t_qaly_d_comp",
                                          c_comp = "t_costs_d_comp",
                                          wtp = wtp)

v_names_inputs <- names(df_pa_complete)[-grep("t_", names(df_pa_complete))]
v_names_inputs <- v_names_inputs[-grep("inc_", v_names_inputs)]
v_names_inputs <- v_names_inputs[-grep("NMB", v_names_inputs)]
v_names_inputs <- v_names_inputs[-grep("NHB", v_names_inputs)]

#-------------------------#
##### INTRODUCE ERRORS ####
#-------------------------#
# In HSTM model
df_pa_error <- df_pa

df_pa_error[c(1, 10, 600, 503, 8888), "u_pfs"]   <- -1  # negative utility values
df_pa_error[c(2, 20, 1200, 1006, 4444), "u_pd"]  <- 2   # utility values above 1

df_pa_error[c(3, 30, 1800, 1509, 2222), "p_pfspd"]   <- -1  # negative probabilities
df_pa_error[c(4, 40, 2400, 3560, 5555), "p_pdd"]     <- 2   # probabilities above 1

df_pa_error[c(10, 100, 6000, 5030, 888), "c_pd"] <- -1  # negative costs

df_pa_error[c(62, 543, 3684, 1253, 9999), "rr"]  <- -10  # negative relative risks

df_pa_error[c(620, 5430, 368, 1235, 999), "t_costs_pd_d_comp"]  <- 0   # discounted results > undiscounted results

df_pa_error[c(6200, 545, 3688, 1236, 9991), "t_qaly_d_comp"] <- -10 # negative total outcomes

df_pa_error[c(25, 256, 2567, 8462), "t_qaly_d_int"] <- df_pa_error[c(25, 256, 2567, 8462), "t_qaly_int"] + 1 # discounted outcomes higher than undiscounted outcomes

df_pa_error <- calculate_nb(df_pa_error,
                            e_int = "t_qaly_d_int",
                            c_int = "t_costs_d_int",
                            e_comp = "t_qaly_d_comp",
                            c_comp = "t_costs_d_comp",
                            wtp = wtp)

# In partitioned survival model
df_pa_psm_error <- df_pa_psm
df_pa_psm_error[c(1, 120, 156, 777), "r_exp_pfs_comp"] <- 0.01

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
                    "p_ae",
                    "rr",
                    "u_pfs",
                    "u_pd",
                    "u_ae",
                    "c_pfs",
                    "c_thx",
                    "c_pd",
                    "c_ae"
                    )

# All parameters
lm_valid <- fit_lm_metamodel(df = df_pa_complete,
                             y_var = "iNMB",
                             x_vars = v_inputs_short
                             )
# Calculate mean absolute error in validation set
v_predictions_validation_full <- predict.lm(lm_valid$fit,
                                       newdata = df_pa_validation)
summary(lm_valid$fit) # all predictors are statistically significant at 0.05, except c_pd and c_ae, removing them does not increase mea absolute error, hence they all remain in

# Remove c_ae and re-fit
v_inputs_short_1 <- c("p_pfspd",
                    "p_pfsd",
                    "p_pdd",
                    "p_ae",
                    "rr",
                    "u_pfs",
                    "u_pd",
                    "u_ae",
                    "c_pfs",
                    "c_thx",
                    "c_pd"
                    )
lm_valid_1 <- fit_lm_metamodel(df = df_pa_complete,
                             y_var = "iNMB",
                             x_vars = v_inputs_short_1
)
summary(lm_valid_1$fit)

# Remove c_pd and re-fit
v_inputs_short_2 <- c("p_pfspd",
                      "p_pfsd",
                      "p_pdd",
                      "p_ae",
                      "rr",
                      "u_pfs",
                      "u_pd",
                      "u_ae",
                      "c_pfs",
                      "c_thx"
                      )
lm_valid_2 <- fit_lm_metamodel(df = df_pa_complete,
                               y_var = "iNMB",
                               x_vars = v_inputs_short_2
)
summary(lm_valid_2$fit)

# Predict on validation set
v_predictions_validation <- predict.lm(lm_valid_2$fit,
                                       newdata = df_pa_validation)

# Calculate mean iNMBs - validation set
mean_inmb_validation_set <- mean(df_pa_validation_complete[, "iNMB"])
mean_inmb_prediction     <- mean(v_predictions_validation)
sd_inmb_validation_set   <- sd(df_pa_validation_complete[, "iNMB"])
sd_inmb_prediction       <- sd(v_predictions_validation)

m_res_validation <- matrix(round(c(mean_inmb_validation_set,
                                   mean_inmb_prediction,
                                   sd_inmb_validation_set,
                                   sd_inmb_prediction)
                                 ),
                           byrow = FALSE,
                           nrow = 2,
                           ncol = 2,
                           dimnames = list(
                             c("Validation set", "Metamodel predictions"),
                             c("Mean iNMB", "SD iNMB")
                             )
                           )

# Calculate mean absolute error
mae_validation <- mean(abs(v_predictions_validation - df_pa_validation_complete[, "iNMB"]))
mre_validation <- mean(abs((v_predictions_validation - df_pa_validation_complete[, "iNMB"])) / abs(df_pa_validation_complete[, "iNMB"]))
r_squared_validation <-  cor(v_predictions_validation, df_pa_validation_complete[, "iNMB"]) ^ 2

## matrix to export
outcomes_fit <- c("MAE", "MRE", "Rsquared")
outcomes_fit_stat <- c("min", "Q1", "median", "mean", "Q3", "max")

m_fit <- matrix(NA,
                nrow = length(outcomes_fit),
                ncol = length(outcomes_fit_stat),
                dimnames = list(
                  outcomes_fit,
                  outcomes_fit_stat
                )
                )

m_fit["MAE", ] <- unlist(summary(abs(v_predictions_validation - df_pa_validation_complete[, "iNMB"])))
m_fit["Rsquared", "mean"] <- r_squared_validation
m_fit <- round(m_fit, 3)
m_fit["MRE", ] <- paste(round(unlist(summary(abs(v_predictions_validation - df_pa_validation_complete[, "iNMB"]) / abs(df_pa_validation_complete[, "iNMB"]))) * 100), "%")

p_mre_above_1 <- length(which(abs((v_predictions_validation - df_pa_validation_complete[, "iNMB"]) / df_pa_validation_complete[, "iNMB"]) > 1)) / nrow(df_pa_validation_complete)

# Probabilities cost-effectiveness both metamodel and original validation data ----
p_ce_original  <- length(which(df_pa_validation_complete[, "iNMB"] > 0)) / length(df_pa_validation_complete[, "iNMB"])
p_ce_metamodel <-length(which(v_predictions_validation > 0)) / length(v_predictions_validation)
df_probs <- data.frame(
  Data = c("Original validation set", "Metamodel prediction"),
  Probability_costeffectiveness = c(p_ce_original, p_ce_metamodel)
)
#--------------#
#### EXPORT ####
#--------------#
# Data sets for PACBOARD ----
write.csv(df_pa_complete, file = paste(getwd(), "/data/data_with_nb.csv", sep = ""))
write.csv(df_pa_complete_rescaled, file = paste(getwd(), "/data/data_with_nb_rescaled.csv", sep = ""))
write.csv(df_pa_error, file = paste(getwd(), "/data/data_with_nb_error.csv", sep = ""))
write.csv(df_pa_psm_error, file = paste(getwd(), "/data/data_psm_error.csv", sep = ""))

# Tables ----
# ## Mean outcomes training set and metamodel
# readr::write_excel_csv2(data.frame(cbind(
#   Stat = rownames(m_res_training),
#   m_res_training)
# ), file = paste(getwd(), "/output/iNMB_metamodel_training_set.csv", sep = ""))

## Mean outcomes validation set and metamodel
readr::write_excel_csv2(data.frame(cbind(
  Stat = rownames(m_res_validation),
  m_res_validation)
), file = paste(getwd(), "/output/iNMB_metamodel_validation_set.csv", sep = ""))
readr::write_excel_csv2(df_probs,
                        file = paste(getwd(), "/output/P_costeffectiveness_metamodel_validation_set.csv", sep = ""))

## Fit statistics validation set
readr::write_excel_csv2(data.frame(cbind(
  Stat = rownames(m_fit),
  m_fit)
  ), file = paste(getwd(), "/output/fit_metamodel_validation_set.csv", sep = ""))

# Predicted versus observed & QQ-plot ----
png(paste(getwd(), "/figs/Prediction_versus_observation_check.png", sep = ""),
    units = "cm",
    width = 21,
    height = 21 / 16 * 9,
    res = 300)
par(mfrow = c(1, 2))
plot(x = df_pa_validation_complete[, "iNMB"],
     y = v_predictions_validation,
     xlab = "Observed",
     ylab = "Predicted",
     main = "A")
abline(coef = c(0,1),
       col = "red")

# QQplot
qqplot(x = df_pa_validation_complete[, "iNMB"],
       y = v_predictions_validation,
       xlab = "Observed",
       ylab = "Predicted",
       main = "B")
abline(coef = c(0,1),
       col = "red")
dev.off()

tiff(paste(getwd(), "/figs/Prediction_versus_observation_check.tif", sep = ""),
    units = "cm",
    width = 21,
    height = 21 / 16 * 9,
    res = 300)
par(mfrow = c(1, 2))
plot(x = df_pa_validation_complete[, "iNMB"],
     y = v_predictions_validation,
     xlab = "Observed",
     ylab = "Predicted",
     main = "A")
abline(coef = c(0,1),
       col = "red")

# QQplot
qqplot(x = df_pa_validation_complete[, "iNMB"],
       y = v_predictions_validation,
       xlab = "Observed",
       ylab = "Predicted",
       main = "B")
abline(coef = c(0,1),
       col = "red")
dev.off()
