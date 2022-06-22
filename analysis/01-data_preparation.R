#########################################
#### 01. DATA PREPARATION: TOY MODEL ####
#########################################

# Set up
rm(list = ls())
library(ggplot2)
source(paste(getwd(), "/R/fct_hemodel.R", sep = ""))

# Model inputs
n_sim <- 10000 # number PA iterations
wtp <- 20000 # willingness to pay threshold, for ICE graph
v_wtp <- seq(from = 0, to = 10000, by = 1000) # vector of WTP's, for CEAC
n_sim_validation <- 5000

## Deterministic
#l_inputs <- generate_det_inputs()

## Probabilistic
df_inputs_pa <- generate_pa_inputs(n_sim)
df_inputs_pa_validation <- generate_pa_inputs(n_sim = n_sim_validation,
                                              seed_num = 500)
df_inputs_pa_psm <- generate_pa_inputs_psm(n_sim)

# Run deterministic model
#perform_simulation(l_params = l_inputs) #WORKS!

# Run probabilistic model
# Perform PA
## Initialise matrix outcomes
m_res_pa <- m_res_pa_psm <- matrix(0,
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
                                                     "t_costs_ae_int")
                                                   )
                                   )
## Probabilistic analysis
for(i in 1:n_sim) {

  ### HSTM
  l_params_temp <- as.list(df_inputs_pa[i, ])
  m_res_pa[i, ] <- perform_simulation(l_params = l_params_temp)

  ### PSM
  l_params_temp     <- as.list(df_inputs_pa_psm[i, ])
  m_res_pa_psm[i, ] <- perform_simulation_psm(l_params = l_params_temp)

}

## Calculate incrementals --> needed?
df_pa <- as.data.frame(m_res_pa)
df_pa <- cbind(df_inputs_pa, df_pa)
df_pa$inc_ly <- df_pa$t_ly_d_int - df_pa$t_ly_d_comp
df_pa$inc_qaly <- df_pa$t_qaly_d_int - df_pa$t_qaly_d_comp
df_pa$inc_costs <- df_pa$t_costs_d_int - df_pa$t_costs_d_comp

df_pa_psm <- as.data.frame(m_res_pa_psm)
df_pa_psm <- cbind(df_inputs_pa_psm, df_pa_psm)
df_pa_psm$inc_ly <- df_pa_psm$t_ly_d_int - df_pa_psm$t_ly_d_comp
df_pa_psm$inc_qaly <- df_pa_psm$t_qaly_d_int - df_pa_psm$t_qaly_d_comp
df_pa_psm$inc_costs <- df_pa_psm$t_costs_d_int - df_pa_psm$t_costs_d_comp

# Run probabilistic model - validation
# Perform PA
## Initialise matrix outcomes
m_res_pa_validation <- matrix(0,
                   ncol = 26,
                   nrow = n_sim_validation,
                   dimnames = list(c(1:n_sim_validation),
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
## Probabilistic analysis
for(i in 1:n_sim_validation) {

  l_params_temp <- as.list(df_inputs_pa_validation[i, ])

  m_res_pa_validation[i, ] <- perform_simulation(l_params = l_params_temp,
                                                 verbose = FALSE)
}

## Calculate incrementals
df_pa_validation <- as.data.frame(m_res_pa_validation)
df_pa_validation <- cbind(df_inputs_pa_validation, df_pa_validation)
df_pa_validation$inc_ly <- df_pa_validation$t_ly_d_int - df_pa_validation$t_ly_d_comp
df_pa_validation$inc_qaly <- df_pa_validation$t_qaly_d_int - df_pa_validation$t_qaly_d_comp
df_pa_validation$inc_costs <- df_pa_validation$t_costs_d_int - df_pa_validation$t_costs_d_comp

save(df_pa_validation, file = paste(getwd(),"/data/df_pa_validation.rda", sep = ""))
