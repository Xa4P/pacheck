# Install & Set up ----
# devtools::install_github("InnovationValueInitiative/IVI-RA")
rm(list = ls())
require(iviRA)
set.seed(123)

# Estimate model inputs ----
n_pats <- 100
pop <- iviRA::sample_pop(n = n_pats, type = "heterog")
l_inputs <- iviRA::get_input_data(pop = pop)
l_pa_param <- iviRA::sample_pars(n = 1000, input_data = l_inputs)
v_txseq1 <- c("adamtx", "etnmtx", "ifxmtx")
txseq2 <- c("cdmards")
mod.structs <- iviRA::select_model_structures(tx_ihaq = "acr-eular-haq",
                                              tx_iswitch = "acr-eular-switch",
                                              cdmards_haq_model = "lcgm",
                                              ttd_cause = "all",
                                              ttd_dist = "exponential",
                                              utility_model = "wailoo")

# Run model ----
sim_out1 <- iviRA::sim_iviRA(
  tx_seqs = v_txseq1,
  input_data = l_inputs,
  pars = l_pa_param,
  model_structures = mod.structs
)
sim_out1_summary <- iviRA::sim_iviRA(
  tx_seqs = v_txseq1,
  input_data = l_inputs,
  pars = l_pa_param,
  model_structures = mod.structs,
  output = "summary"
)
sim_out2_summary <- iviRA::sim_iviRA(
  tx_seqs = txseq2,
  input_data = l_inputs,
  pars = l_pa_param,
  model_structures = mod.structs,
  output = "summary"
)

# Save datasets ----
save(l_pa_param, file = "data/l_iviRA_pa_params.rda")
save(sim_out1, file = "data/df_iviRA_out_all.rda")
save(sim_out1_summary, file = "data/l_iviRA_out_summ_1.rda")
save(sim_out2_summary, file = "data/l_iviRA_out_summ_2.rda")
