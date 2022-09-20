#' Generate deterministic model inputs.
#'
#' @description This function generates the deterministic model inputs for the example health economic model developed to test the functionalities of the package.
#'
#' @return A list.
#'
#' @examples
#' # Generating deterministic model inputs and storing them in an object.
#' l_inputs_det <- generate_det_inputs()
#'
#' @export
#'
generate_det_inputs <- function() {

   l_output = list(

     # Rates & probabilities
     p_pfspd = 0.15,
     p_pfsd = 0.1,
     p_pdd = 0.2,
     p_dd = 1,
     p_ae = 0.05,

     # Treatment effectiveness
     rr = 0.75,

     # Utility values
     u_pfs = 0.8,
     u_pd = 0.6,
     u_d = 0,
     u_ae = 0.15,

     # Costs
     c_pfs = 1000,
     c_pd = 2000,
     c_d = 0,
     c_ae = 500,

     c_thx = 10000
    )
  return(l_output)
}

#' Generate probabilistic model inputs.
#'
#' @description This function generates the probabilistic model inputs for the example health economic model developed to test the functionalities of the package.
#'
#' @param n_sim integer. Number of probabilistic value to draw for each model input. Default is 10,000.
#' @param sd_var numeric. Determines the standard error of the mean to use for the normal distributions when the standard error not known. Default is 0.2 (20\%).
#' @param seed_num integer. The seed number to use when drawing the probabilistic values. Default is 452.
#'
#' @return A list.
#'
#' @examples
#' # Generating deterministic model inputs and storing them in an object.
#' df_inputs_prob <- generate_pa_inputs()
#'
#' @import gtools
#' @export
#'
generate_pa_inputs <- function(n_sim = 10000,
                               sd_var = 0.2,
                               seed_num = 452) {

  # Set seed
  set.seed(seed_num)

  # Function to calculate the parameters of beta distributions.
  estimate_params_beta <- function(mu, sd){
    alpha <- (((mu * (1 - mu)) / sd ^ 2) - 1) * mu
    beta  <- (((mu * (1 - mu)) / sd ^ 2) - 1) * (1 - mu)
    return(params = list(alpha = alpha,
                         beta = beta)
    )
    }

  # Function to estimate parameters of a GAMMA distribution based on mean and sd, using the methods of moments
  estimate_params_gamma <- function(mu, sd)  {
    shape <- mu ^ 2 / sd ^ 2
    rate  <- mu / sd ^ 2
    return(params = list(shape = shape,
                         rate = rate)
    )
    }

  # Determine probabilistic model input values
  df_tp_pfs <- gtools::rdirichlet(n_sim, c(75, 15, 10))
  names(df_tp_pfs) <- c("p_pfspfs", "p_pfspd",  "p_pfsd")

  df_output = data.frame(

    ## Rates & probabilities
    p_pfspd = df_tp_pfs[, 2],
    p_pfsd = df_tp_pfs[, 3],
    p_pdd = rbeta(n_sim, 20, 80),
    p_dd = rep(1, n_sim),
    p_ae = rbeta(n_sim, 5, 95),

    ## Treatment effectiveness
    rr = exp(rnorm(n_sim, log(0.75), (log(0.88) - log(0.62))/ (2 * 1.96))),

    ## Utility values
    u_pfs = rbeta(n_sim, estimate_params_beta(0.75, 0.07)[[1]], estimate_params_beta(0.75, 0.07)[[2]]),
    u_pd = rbeta(n_sim, estimate_params_beta(0.55, 0.1)[[1]], estimate_params_beta(0.55, 0.1)[[2]]),
    u_d = rep(0, n_sim),
    u_ae = rbeta(n_sim, estimate_params_beta(0.15, 0.05)[[1]], estimate_params_beta(0.15, 0.05)[[2]]),

    ## Costs
    c_pfs = rnorm(n_sim, 1000, 1000 * sd_var),
    c_pd = rnorm(n_sim, 2000, 2000 * sd_var),
    c_d = rep(0, n_sim),
    c_thx = rnorm(n_sim, 10000, 100),
    c_ae = rgamma(n_sim, estimate_params_gamma(500, 500 * sd_var)[[1]], estimate_params_gamma(500, 500 * sd_var)[[2]])
  )
  return(df_output)
}

#' Perform the health economic simulation.
#'
#' @description This function performs the simulation of the health economic model developed to test the functionalities of the package.
#'
#' @param l_params list. List of inputs of the health economic model
#'
#' @return A vector. This vector contains the (un)discouted intermediate and final outcomes of the health economic model.
#'
#' @examples
#' # Perform the simulation using the deterministic model inputs
#' l_inputs_det <- generate_det_inputs()
#' v_results_det <- perform_simulation(l_inputs_det)
#'
#' @export
perform_simulation <- function(l_params) {

  with(as.list(l_params), {

    # Setting parameters
    n_cycles <- 30 # number of cycles
    r_d_effects <- 0.015 # annual discount rate, health effects
    r_d_costs <- 0.04 # annual discount rate, costs
    v_names_hs <- c("PFS", "PD", "D") # vector of names of health states
    n_hs <- length(v_names_hs) # number of health states
    n_ind <- 10000 # number of individuals to simulate
    v_start_hs <- c(n_ind, 0, 0) # vector of starting position in the model

    ## Define discount weights per cycle (years in this case)
    v_dw_e <- 1 / (1 + r_d_effects) ^ c(1:n_cycles)
    v_dw_c <- 1 / (1 + r_d_costs) ^ c(1:n_cycles)

    m_tp_comp <- matrix(0,
                   ncol = n_hs,
                   nrow = n_hs,
                   dimnames = list(v_names_hs,
                                   v_names_hs))

    m_tp_comp["PFS", "PFS"]  <- 1 - p_pfspd - p_pfsd
    m_tp_comp["PFS", "PD"]   <- p_pfspd
    m_tp_comp["PFS", "D"]    <- p_pfsd
    m_tp_comp["PD", "PD"]    <- 1 - p_pdd
    m_tp_comp["PD", "D"]     <- p_pdd
    m_tp_comp["D", "D"]      <- p_dd

    m_tp_int <- m_tp_comp

    m_tp_int["PFS", "PD"]    <- 1 - exp(log(1 - p_pfspd) * rr)
    m_tp_int["PFS", "PFS"]   <- 1 - m_tp_int["PFS", "PD"] - m_tp_int["PFS", "D"]

    m_hs_comp <- m_hs_int <- matrix(0,
                                    nrow = n_cycles + 1,
                                    ncol = length(v_names_hs),
                                    dimnames = list(c(0:n_cycles),
                                                    v_names_hs))

    ## Define the starting positions of the cohort
    m_hs_comp[1, ] <- m_hs_int[1, ] <- v_start_hs

    # Perform the matrix multiplication to determine the state membership over the cycles
    for(cycle in 1:n_cycles){
      m_hs_comp[cycle + 1, ] <- m_hs_comp[cycle, ] %*% m_tp_comp
      m_hs_int[cycle + 1, ]  <- m_hs_int[cycle, ] %*% m_tp_int
    }

    # Calculate undiscounted output
    ## Life years
    v_ly_comp <-  v_ly_int <- c("PFS" = 1,
                                "PD" = 1,
                                "D" = 0) # vector of rewards
    v_t_ly_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_ly_comp
    v_t_ly_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_ly_int

    ## QALY's
    v_qaly_comp <- v_qaly_int <- c("PFS" = u_pfs,
                                   "PD" = u_pd,
                                   "D" = u_d)
    v_t_qaly_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_qaly_comp
    v_t_qaly_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_qaly_int

    ## Costs
    v_c_comp <- c("PFS" = c_pfs,
                  "PD" = c_pd,
                  "D" = c_d)

    v_c_int <- c("PFS" = c_pfs + c_thx,
                 "PD" = c_pd,
                 "D" = c_d)

    v_t_c_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_c_comp
    v_t_c_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_c_int

    # Calculate discounted output

    ## Life years
    n_t_ly_comp_d <- t(v_t_ly_comp) %*% v_dw_e
    n_t_ly_int_d  <- t(v_t_ly_int) %*% v_dw_e
    t_ly_pfs_comp <- sum(t(m_hs_comp[2:nrow(m_hs_comp), 1]) %*% v_dw_e)
    t_ly_pd_comp  <- sum(t(m_hs_comp[2:nrow(m_hs_comp), 2]) %*% v_dw_e)
    t_ly_pfs_int  <- sum(t(m_hs_int[2:nrow(m_hs_int), 1]) %*% v_dw_e)
    t_ly_pd_int   <- sum(t(m_hs_int[2:nrow(m_hs_int), 2]) %*% v_dw_e)

    ## QALYs
    t_qaly_ae_int <- n_ind * p_ae * u_ae # total qaly loss adverse events

    n_t_qaly_comp_d <- t(v_t_qaly_comp) %*% v_dw_e
    n_t_qaly_int_d  <- t(v_t_qaly_int) %*% v_dw_e - t_qaly_ae_int
    t_qaly_pfs_comp <- sum(t(m_hs_comp[2:nrow(m_hs_comp), 1] * u_pfs) %*% v_dw_e)
    t_qaly_pd_comp  <- sum(t(m_hs_comp[2:nrow(m_hs_comp), 2] * u_pd) %*% v_dw_e)
    t_qaly_pfs_int  <- sum(t(m_hs_int[2:nrow(m_hs_int), 1] * u_pfs) %*% v_dw_e)
    t_qaly_pd_int   <- sum(t(m_hs_int[2:nrow(m_hs_int), 2] * u_pd) %*% v_dw_e)

    ## Costs
    t_c_ae_int <- n_ind * p_ae * c_ae # total costs adverse events

    n_t_c_comp_d <- t(v_t_c_comp) %*% v_dw_c
    n_t_c_int_d  <- (t(v_t_c_int) %*% v_dw_c) + t_c_ae_int
    t_c_pfs_comp <- sum(t(m_hs_comp[2:nrow(m_hs_comp), 1] * c_pfs) %*% v_dw_c)
    t_c_pd_comp  <- sum(t(m_hs_comp[2:nrow(m_hs_comp), 2] * c_pd) %*% v_dw_c)
    t_c_pfs_int  <- sum(t(m_hs_int[2:nrow(m_hs_int), 1] * (c_pfs + c_thx)) %*% v_dw_c)
    t_c_pd_int   <- sum(t(m_hs_int[2:nrow(m_hs_int), 2] * c_pd) %*% v_dw_c)

    # Mean total and intermediate (un)discounted outputs
    v_res <- c(t_qaly_comp = sum(v_t_qaly_comp) / n_ind,
               t_qaly_int = (sum(v_t_qaly_int) - t_qaly_ae_int)  / n_ind,
               t_qaly_d_comp = n_t_qaly_comp_d / n_ind,
               t_qaly_d_int = n_t_qaly_int_d / n_ind,
               t_costs_comp = sum(v_t_c_comp) / n_ind,
               t_costs_int = (sum(v_t_c_int) + t_c_ae_int) / n_ind,
               t_costs_d_comp = n_t_c_comp_d / n_ind,
               t_costs_d_int = n_t_c_int_d / n_ind,
               t_ly_comp = sum(v_t_ly_comp) / n_ind,
               t_ly_int = sum(v_t_ly_int)  / n_ind,
               t_ly_d_comp = n_t_ly_comp_d / n_ind,
               t_ly_d_int = n_t_ly_int_d / n_ind,
               t_ly_pfs_d_comp = t_ly_pfs_comp / n_ind,
               t_ly_pfs_d_int = t_ly_pfs_int / n_ind,
               t_ly_pd_d_comp = t_ly_pd_comp / n_ind,
               t_ly_pd_d_int = t_ly_pd_int / n_ind,
               t_qaly_pfs_d_comp = t_qaly_pfs_comp / n_ind,
               t_qaly_pfs_d_int = t_qaly_pfs_int / n_ind,
               t_qaly_pd_d_comp = t_qaly_pd_comp / n_ind,
               t_qaly_pd_d_int = t_qaly_pd_int / n_ind,
               t_costs_pfs_d_comp = t_c_pfs_comp / n_ind,
               t_costs_pfs_d_int = t_c_pfs_int / n_ind,
               t_costs_pd_d_comp = t_c_pd_comp / n_ind,
               t_costs_pd_d_int = t_c_pd_int / n_ind,
               t_qaly_ae_int = t_qaly_ae_int / n_ind,
               t_costs_ae_int = t_c_ae_int / n_ind
    )

    return(v_res)

  }
  )
}

#' Perform deterministic one-way sensitivity analyses using probabilistic inputs and outputs.
#'
#' @description This function performs the deterministic one-way sensitivity analyses (DOWSA) using probabilistic inputs and outputs for the health economic model developed to test the package. The outcome of the DOWSA is the incremental net monetary benefit.
#'
#' @param df a dataframe. This dataframe contains the probabilistic inputs and outputs of the health economic model.
#' @param vars a vector of strings. Contains the name of the variables for which to perform thedeterministic one-way sensitivity analysis.
#' @param wtp numeric. The willingness to pay per QALY in euros. Default is 120,000 euros per QALY.
#'
#' @return A dataframe. The ouctome of the deterministic one-way sensitivity analyses is the iNMB by default.
#'
#' @examples
#' # Perform the deterministic one-way sensitivity analyses for a selection of parameters
#' data(df_pa)
#' df_res_dowsa <- perform_dowsa(df = df_pa,
#'                               vars = c("rr", "c_pfs", "p_pd", "u_pfs", "u_pd"))
#'
#' @export
perform_dowsa <- function(df,
                          vars,
                          wtp = 120000) {

  # Determine lower and upper bound using the probabilistic outcomes
  df_dsa <- data.frame(
    rbind(apply(df[, vars], 2, mean),
          apply(df[, vars], 2, function(x) quantile(x, 0.025)),
          apply(df[, vars], 2, function(x) quantile(x, 0.975))
    )
  )

  # Initiate matrices to store results
  m_low <- matrix(NA,
                  ncol = 2,
                  nrow = ncol(df_dsa),
                  dimnames = list(names(df_dsa),
                                  c("Parameter", "Lower_Bound")))
  m_upp <- matrix(NA,
                  ncol = 2,
                  nrow = ncol(df_dsa),
                  dimnames = list(names(df_dsa),
                                  c("Parameter", "Upper_Bound")))

  # Perform DOWSA and fill matrices
  ## Lower bound of parameters
  for (j in vars){
    df_temp_dsa <- df
    df_temp_dsa[, j] <- df_dsa[2, which(names(df_dsa) == j)]
    m_res_pa <- matrix(NA_real_,
                       nrow = nrow(df),
                       ncol = length(perform_simulation(l_params = as.list(df_temp_dsa[1, ])))
                       )

  for (i in 1:nrow(df)) {
    l_params_temp <- as.list(df_temp_dsa[i, ])
    m_res_pa[i, ] <- perform_simulation(l_params = l_params_temp)
    }
    colnames(m_res_pa) <- names(perform_simulation(l_params = as.list(df_temp_dsa[1, ])))
    v_iNMB <- (m_res_pa[, "t_qaly_d_int"] - m_res_pa[, "t_qaly_d_comp"]) * wtp - (m_res_pa[, "t_costs_d_int"] - m_res_pa[, "t_costs_d_comp"])
    m_low[j, ] <- c(names(df)[which(names(df) == j)], mean(v_iNMB))
  }

  ## Upper bound of parameters
  for (j in vars){
    df_temp_dsa <- df
    df_temp_dsa[, j] <- df_dsa[3, which(names(df_dsa) == j)]
    m_res_pa <- matrix(NA_real_,
                       nrow = nrow(df),
                       ncol = length(perform_simulation(l_params = as.list(df_temp_dsa[1, ]))))

    for (i in 1:nrow(df)) {
      l_params_temp <- as.list(df_temp_dsa[i, ])
      m_res_pa[i, ] <- perform_simulation(l_params = l_params_temp)
      }
    colnames(m_res_pa) <- names(perform_simulation(l_params = as.list(df_temp_dsa[1, ])))
    v_iNMB <- (m_res_pa[, "t_qaly_d_int"] - m_res_pa[, "t_qaly_d_comp"]) * wtp - (m_res_pa[, "t_costs_d_int"] - m_res_pa[, "t_costs_d_comp"])
    m_upp[j, ] <- c(names(df)[which(names(df) == j)], mean(v_iNMB))
  }

  # Combine matrices and export
  rownames(m_low) <- rownames(m_upp) <- NULL
  df_low <- as.data.frame(m_low)
  df_upp <- as.data.frame(m_upp)

  df_out <- merge(df_low, df_upp)
  df_out[, 2:ncol(df_out)] <- apply(df_out[, 2:ncol(df_out)], 2, function(x) as.numeric(as.character(x)))

  return(df_out)

}

#' Generate probabilistic model inputs for partitioned survival model.
#'
#' @description This function generates the probabilistic model inputs for the example health economic model developed to test the functionalities of the package.
#'
#' @param n_sim integer. Number of probabilistic value to draw for each model input. Default is 10,000.
#' @param sd_var numeric. Determines the standard error of the mean to use for the normal distributions when the standard error not known. Default is 0.2 (20\%).
#' @param seed_num integer. The seed number to use when drawing the probabilistic values. Default is 452.
#'
#' @return A list.
#'
#' @examples
#' # Generating deterministic model inputs and storing them in an object.
#' df_inputs_prob <- generate_pa_inputs()
#'
#' @import gtools
#' @import flexsurv
#' @import boot
#' @import simsurv
#' @export
#'
generate_pa_inputs_psm <- function(n_sim = 10000,
                                   sd_var = 0.2,
                                   seed_num = 452) {

  # Set seed
  set.seed(seed_num)

  # Function to calculate the parameters of beta distributions.
  estimate_params_beta <- function(mu, sd){
    alpha <- (((mu * (1 - mu)) / sd ^ 2) - 1) * mu
    beta  <- (((mu * (1 - mu)) / sd ^ 2) - 1) * (1 - mu)
    return(params = list(alpha = alpha,
                         beta = beta)
    )
  }

  # Function to estimate parameters of a GAMMA distribution based on mean and sd, using the methods of moments
  estimate_params_gamma <- function(mu, sd)  {
    shape <- mu ^ 2 / sd ^ 2
    rate  <- mu / sd ^ 2
    return(params = list(shape = shape,
                         rate = rate)
    )
  }

  # Simulate PFS and OS data
  df_sim_surv_time_pfs <- simsurv::simsurv(dist = "exponential",
                                           x = data.frame(id = 1:1000,
                                                          trt = c(rep(0, 500),
                                                                  rep(1, 500)
                                                          )
                                           ),
                                           lambdas = 0.3,
                                           betas = c(trt = -0.5),
                                           maxt = 5
  )
  df_sim_surv_time_os <- simsurv::simsurv(dist = "weibull",
                                          x = data.frame(id = 1:1000,
                                                         trt = c(rep(0, 500),
                                                                 rep(1, 500)
                                                         )
                                          ),
                                          lambdas = 0.009,
                                          gammas = 1.95,
                                          betas = c(trt = - 0.25),
                                          maxt = 5
  )
  df_sim_surv_pfs <- merge(data.frame(id = 1:1000,
                                      trt = factor(
                                        c(rep(0, 500),
                                          rep(1, 500)
                                        )
                                      )
  ),
  df_sim_surv_time_pfs
  )
  df_sim_surv_os <- merge(data.frame(id = 1:1000,
                                     trt = factor(
                                       c(rep(0, 500),
                                         rep(1, 500)
                                       )
                                     )
  ),
  df_sim_surv_time_os
  )

  # Function to fit exponential model to bootstrap sample
  fct_surv_params_exp <- function(df, i){
    df_boot <- df[i,]
    l_exp <- flexsurv::flexsurvreg(Surv(eventtime, status) ~ trt,
                                   data = df_boot,
                                   dist = "exp")
    v_out <- c(l_exp$res[1, 1], exp(l_exp$res[2, 1]))
    v_out
  }

  # Function to fit weibull model to bootstrap sample
  fct_surv_params_weib <- function(df, i){
    df_boot <- df[i,]
    l_weib <- flexsurv::flexsurvreg(Surv(eventtime, status) ~ trt,
                                    data = df_boot,
                                    dist = "weibull")
    v_out <- c(l_weib$res[1, 1], l_weib$res[2, 1], exp(l_weib$res[3, 1]))
    v_out
  }

  l_boot_pfs  <- boot::boot(df_sim_surv_pfs, R = n_sim, statistic = fct_surv_params_exp, strata = df_sim_surv_pfs[, "trt"])
  l_boot_os <- boot::boot(df_sim_surv_os, R = n_sim, statistic = fct_surv_params_weib, strata = df_sim_surv_os[, "trt"])

  df_output = data.frame(

    ## Rates & probabilities

    p_ae = rbeta(n_sim, 5, 95),

    ## Surv model parameters
    ### PFS
    r_exp_pfs_comp = l_boot_pfs$t[, 1],
    rr_thx_pfs     = l_boot_pfs$t[, 2],
    r_exp_pfs_trt  = l_boot_pfs$t[, 1] * l_boot_pfs$t[, 2],
    ### OS
    shape_weib_os      = l_boot_os$t[, 1],
    scale_weib_os_comp = l_boot_os$t[, 2],
    rr_thx_os          = l_boot_os$t[, 3],
    scale_weib_os_int  = l_boot_os$t[, 2] * l_boot_os$t[, 3],

    ## Utility values
    u_pfs = rbeta(n_sim, estimate_params_beta(0.75, 0.07)[[1]], estimate_params_beta(0.75, 0.07)[[2]]),
    u_pd = rbeta(n_sim, estimate_params_beta(0.55, 0.1)[[1]], estimate_params_beta(0.55, 0.1)[[2]]),
    u_d = rep(0, n_sim),
    u_ae = rbeta(n_sim, estimate_params_beta(0.15, 0.05)[[1]], estimate_params_beta(0.15, 0.05)[[2]]),

    ## Costs
    c_pfs = rnorm(n_sim, 1000, 1000 * sd_var),
    c_pd = rnorm(n_sim, 2000, 2000 * sd_var),
    c_d = rep(0, n_sim),
    c_thx = rnorm(n_sim, 10000, 100),
    c_ae = rgamma(n_sim, estimate_params_gamma(500, 500 * sd_var)[[1]], estimate_params_gamma(500, 500 * sd_var)[[2]])
  )
  return(df_output)
}

#' Perform the health economic simulation using partitioned survival model.
#'
#' @description This function performs the simulation of the partitioned survival health economic model developed to test the functionalities of the package.
#'
#' @param l_params list. List of inputs of the health economic model
#' @param min_fct logical. Should a minimum function be used to ensure PFS remains lower than OS? Default is TRUE
#'
#' @return A vector. This vector contains the (un)discouted intermediate and final outcomes of the health economic model.
#'
#' @examples
#' # Perform the simulation using the deterministic model inputs
#' l_inputs_det <- generate_det_inputs()
#' v_results_det <- perform_simulation(l_inputs_det)
#'
#' @export
perform_simulation_psm <- function(l_params,
                                   min_fct = TRUE) {

  with(as.list(l_params), {

    # Setting parameters
    n_cycles <- 30 # number of cycles
    n_years  <- 30 # number of years to simulate
    r_d_effects <- 0.015 # annual discount rate, health effects
    r_d_costs <- 0.04 # annual discount rate, costs
    v_names_hs <- c("PFS", "PD", "D") # vector of names of health states
    n_hs <- length(v_names_hs) # number of health states
    n_ind <- 10000 # number of individuals to simulate
    v_start_hs <- c(n_ind, 0, 0) # vector of starting position in the model
    v_time <- seq(0, n_years, length.out = n_cycles + 1)

    ## Define discount weights per cycle (years in this case)
    v_dw_e <- 1 / (1 + r_d_effects) ^ c(1:n_cycles)
    v_dw_c <- 1 / (1 + r_d_costs) ^ c(1:n_cycles)

    # Cohort simulation matrices
    m_hs_comp <- m_hs_int <- matrix(0,
                                    nrow = n_cycles + 1,
                                    ncol = length(v_names_hs),
                                    dimnames = list(c(0:n_cycles),
                                                    v_names_hs))

    # Fill in matrix using survival models
    m_hs_comp[, "D"]   <- n_ind * pweibull(v_time, shape = shape_weib_os, scale = scale_weib_os)
    if(min_fct == TRUE) {
      m_hs_comp[, "PFS"] <- n_ind * vapply(1:length(v_time), function(x){
        min(1 - pexp(v_time[x], rate = r_exp_pfs),
            1 - pweibull(v_time[x], shape = shape_weib_os, scale = scale_weib_os)
            )
      },
      numeric(1)
      )
    } else {
      m_hs_comp[, "PFS"] <- n_ind * (1 - pexp(v_time, rate = r_exp_pfs))
      }

    m_hs_comp[, "PD"]  <- n_ind - m_hs_comp[, "D"] - m_hs_comp[, "PFS"]

    m_hs_int[, "D"]   <- n_ind * pweibull(v_time, shape = shape_weib_os, scale = scale_weib_os * rr_thx_os)
    if(min_fct == TRUE) {
      m_hs_int[, "PFS"] <- n_ind * vapply(1:length(v_time), function(x){
        min(1 - pexp(v_time[x], rate = r_exp_pfs * rr_thx_pfs),
            1 - pweibull(v_time[x], shape = shape_weib_os, scale = scale_weib_os * rr_thx_os)
            )
      },
      numeric(1)
      )
    } else {
      m_hs_comp[, "PFS"] <- n_ind * (1 - pexp(v_time, rate = r_exp_pfs))
    }
    m_hs_int[, "PD"]  <- n_ind - m_hs_int[, "D"] - m_hs_int[, "PFS"]

    # Calculate undiscounted output
    ## Life years
    v_ly_comp <-  v_ly_int <- c("PFS" = 1,
                                "PD" = 1,
                                "D" = 0) # vector of rewards
    v_t_ly_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_ly_comp
    v_t_ly_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_ly_int

    ## QALY's
    v_qaly_comp <- v_qaly_int <- c("PFS" = u_pfs,
                                   "PD" = u_pd,
                                   "D" = u_d)
    v_t_qaly_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_qaly_comp
    v_t_qaly_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_qaly_int

    ## Costs
    v_c_comp <- c("PFS" = c_pfs,
                  "PD" = c_pd,
                  "D" = c_d)

    v_c_int <- c("PFS" = c_pfs + c_thx,
                 "PD" = c_pd,
                 "D" = c_d)

    v_t_c_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_c_comp
    v_t_c_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_c_int

    # Calculate discounted output

    ## Life years
    n_t_ly_comp_d <- t(v_t_ly_comp) %*% v_dw_e
    n_t_ly_int_d  <- t(v_t_ly_int) %*% v_dw_e
    t_ly_pfs_comp <- sum(t(m_hs_comp[2:nrow(m_hs_comp), 1]) %*% v_dw_e)
    t_ly_pd_comp  <- sum(t(m_hs_comp[2:nrow(m_hs_comp), 2]) %*% v_dw_e)
    t_ly_pfs_int  <- sum(t(m_hs_int[2:nrow(m_hs_int), 1]) %*% v_dw_e)
    t_ly_pd_int   <- sum(t(m_hs_int[2:nrow(m_hs_int), 2]) %*% v_dw_e)

    ## QALYs
    t_qaly_ae_int <- n_ind * p_ae * u_ae # total qaly loss adverse events

    n_t_qaly_comp_d <- t(v_t_qaly_comp) %*% v_dw_e
    n_t_qaly_int_d  <- t(v_t_qaly_int) %*% v_dw_e - t_qaly_ae_int
    t_qaly_pfs_comp <- sum(t(m_hs_comp[2:nrow(m_hs_comp), 1] * u_pfs) %*% v_dw_e)
    t_qaly_pd_comp  <- sum(t(m_hs_comp[2:nrow(m_hs_comp), 2] * u_pd) %*% v_dw_e)
    t_qaly_pfs_int  <- sum(t(m_hs_int[2:nrow(m_hs_int), 1] * u_pfs) %*% v_dw_e)
    t_qaly_pd_int   <- sum(t(m_hs_int[2:nrow(m_hs_int), 2] * u_pd) %*% v_dw_e)

    ## Costs
    t_c_ae_int <- n_ind * p_ae * c_ae # total costs adverse events

    n_t_c_comp_d <- t(v_t_c_comp) %*% v_dw_c
    n_t_c_int_d  <- (t(v_t_c_int) %*% v_dw_c) + t_c_ae_int
    t_c_pfs_comp <- sum(t(m_hs_comp[2:nrow(m_hs_comp), 1] * c_pfs) %*% v_dw_c)
    t_c_pd_comp  <- sum(t(m_hs_comp[2:nrow(m_hs_comp), 2] * c_pd) %*% v_dw_c)
    t_c_pfs_int  <- sum(t(m_hs_int[2:nrow(m_hs_int), 1] * (c_pfs + c_thx)) %*% v_dw_c)
    t_c_pd_int   <- sum(t(m_hs_int[2:nrow(m_hs_int), 2] * c_pd) %*% v_dw_c)

    # Mean total and intermediate (un)discounted outputs
    v_res <- c(t_qaly_comp = sum(v_t_qaly_comp) / n_ind,
               t_qaly_int = (sum(v_t_qaly_int) - t_qaly_ae_int)  / n_ind,
               t_qaly_d_comp = n_t_qaly_comp_d / n_ind,
               t_qaly_d_int = n_t_qaly_int_d / n_ind,
               t_costs_comp = sum(v_t_c_comp) / n_ind,
               t_costs_int = (sum(v_t_c_int) + t_c_ae_int) / n_ind,
               t_costs_d_comp = n_t_c_comp_d / n_ind,
               t_costs_d_int = n_t_c_int_d / n_ind,
               t_ly_comp = sum(v_t_ly_comp) / n_ind,
               t_ly_int = sum(v_t_ly_int)  / n_ind,
               t_ly_d_comp = n_t_ly_comp_d / n_ind,
               t_ly_d_int = n_t_ly_int_d / n_ind,
               t_ly_pfs_d_comp = t_ly_pfs_comp / n_ind,
               t_ly_pfs_d_int = t_ly_pfs_int / n_ind,
               t_ly_pd_d_comp = t_ly_pd_comp / n_ind,
               t_ly_pd_d_int = t_ly_pd_int / n_ind,
               t_qaly_pfs_d_comp = t_qaly_pfs_comp / n_ind,
               t_qaly_pfs_d_int = t_qaly_pfs_int / n_ind,
               t_qaly_pd_d_comp = t_qaly_pd_comp / n_ind,
               t_qaly_pd_d_int = t_qaly_pd_int / n_ind,
               t_costs_pfs_d_comp = t_c_pfs_comp / n_ind,
               t_costs_pfs_d_int = t_c_pfs_int / n_ind,
               t_costs_pd_d_comp = t_c_pd_comp / n_ind,
               t_costs_pd_d_int = t_c_pd_int / n_ind,
               t_qaly_ae_int = t_qaly_ae_int / n_ind,
               t_costs_ae_int = t_c_ae_int / n_ind
    )

    return(v_res)

  }
  )
}
