# Install & Set up ----
# devtools::install_github("InnovationValueInitiative/IVI-RA")
rm(list = ls())
require(iviRA)
set.seed(123)

# Estimate model inputs ----
n_pats <- 1000
pop <- iviRA::sample_pop(n = n_pats, type = "heterog")
l_inputs <- iviRA::get_input_data(pop = pop)
l_iviRA_pa_params <- iviRA::sample_pars(n = 1000, input_data = l_inputs)
v_txseq1 <- c("adamtx", "etnmtx", "ifxmtx")
txseq2 <- c("cdmards")
mod.structs <- iviRA::select_model_structures(tx_ihaq = "acr-eular-haq",
                                              tx_iswitch = "acr-eular-switch",
                                              cdmards_haq_model = "linear",
                                              ttd_cause = "all",
                                              ttd_dist = "exponential",
                                              utility_model = "wailoo")

# Run model ----
l_iviRA_out_summ_1 <- iviRA::sim_iviRA(
  tx_seqs = v_txseq1,
  input_data = l_inputs,
  pars = l_iviRA_pa_params,
  model_structures = mod.structs,
  output = "summary"
)
l_iviRA_out_summ_2 <- iviRA::sim_iviRA(
  tx_seqs = txseq2,
  input_data = l_inputs,
  pars = l_iviRA_pa_params,
  model_structures = mod.structs,
  output = "summary"
)

# Save datasets ----
df_iviRA_pa <- data.frame(
  p_discount = l_iviRA_pa_params$tx.cost$discount,
  p_rebound = l_iviRA_pa_params$rebound,
  or_mort = exp(l_iviRA_pa_params$mort.logor),
  r_haq_prog = l_iviRA_pa_params$haq.lprog.tx,
  n_hosp_days = l_iviRA_pa_params$hosp.cost$hosp.days,
  c_hosp_pday = l_iviRA_pa_params$hosp.cost$cost.pday,
  c_si   = l_iviRA_pa_params$si.cost,
  c_prod_loss = l_iviRA_pa_params$prod.loss,
  u_model_wailoo = l_iviRA_pa_params$utility.wailoo,
  t_qaly_thx1 = l_iviRA_out_summ_1$means$qalys,
  t_qaly_d_thx1 = l_iviRA_out_summ_1$means$dqalys,
  t_cost_d_thx1 = l_iviRA_out_summ_1$means$dtot_cost,
  t_qaly_thx2 = l_iviRA_out_summ_2$means$qalys,
  t_qaly_d_thx2 = l_iviRA_out_summ_2$means$dqalys,
  t_cost_d_thx2 = l_iviRA_out_summ_2$means$dtot_cost
)
df_iviRA_pa$inc_qaly <- df_iviRA_pa$t_qaly_d_thx1 - df_iviRA_pa$t_qaly_d_thx2
df_iviRA_pa$inc_cost <- df_iviRA_pa$t_cost_d_thx1 - df_iviRA_pa$t_cost_d_thx2
# save(df_iviRA_pa, file = "data/df_iviRA_pa.rda")
