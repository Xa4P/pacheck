#### REDUCE SIZE EXAMPLE DATASETS ####
# Set up ----
rm(list = ls())
set.seed(1)
load("data-raw/df_pa.rda")
load("data-raw/df_pa_psm.rda")

# Select iterations ----
n_it <- nrow(df_pa)
v_iterations_hstm <- sample(1:n_it, 1000)
v_iterations_psm  <- sample(1:n_it, 1000)

# Reduce datasets size ----
df_pa <- df_pa[v_iterations_hstm, ]
df_pa_psm <- df_pa_psm[v_iterations_psm, ]

# Save ----
save(df_pa, file = "data/df_pa.rda")
save(df_pa_psm, file = "data/df_pa_psm.rda")
