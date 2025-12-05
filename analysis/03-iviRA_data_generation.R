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
# save(l_iviRA_pa_params, file = "data/l_iviRA_pa_params.rda")
# save(l_iviRA_out_summ_1, file = "data/l_iviRA_out_summ_1.rda")
# save(l_iviRA_out_summ_2, file = "data/l_iviRA_out_summ_2.rda")
