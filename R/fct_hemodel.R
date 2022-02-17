#################################
#### FUNCTIONS FOR HE MODELS ####
#################################


# Function to determine deterministic inputs
generate_det_inputs <- function() {

   l_output = list(

    ## Rates & probabilities
     p_pfspd = 0.15,
     p_pfsd = 0.1,
     p_pdd = 0.2,
     p_dd = 1,
     p_ae = 0.05,

     ## Treatment effectiveness
     rr = 0.75,

     ## Utility values
     u_pfs = 0.8,
     u_pd = 0.6,
     u_d = 0,
     u_ae = 0.15,

     ## Costs
     c_pfs = 1000,
     c_pd = 2000,
     c_d = 0,
     c_ae = 500,

     c_thx = 10000
    )
  return(l_output)
}

# Function to determine probabilistic inputs
generate_pa_inputs <- function(n_sim = 10000,
                               sd_var = 0.2,
                               seed_num = 452) {

  require(gtools)
  set.seed(seed_num)

  # Function to calculate the parameters of beta distributions.
  estimate_params_beta <- function(mu, sd){
    alpha <- (((mu * (1-mu))/sd^2)-1)*mu
    beta  <- (((mu * (1-mu))/sd^2)-1)*(1-mu)
    return(params = list(alpha = alpha, beta = beta))
  }

  # Function to estimate parameters of a GAMMA distribution based on mean and sd, using the methods of moments
  estimate_params_gamma <- function(mu, sd)
  {
    shape <- mu^2/sd^2
    rate <- mu/sd^2

    return(params = list(shape = shape, rate = rate))
  }

  df_tp_pfs <- rdirichlet(n_sim, c(75, 15, 10))
  names(df_tp_pfs) <- c("p_pfspfs", "p_pfspd",  "p_pfsd")

  df_output = data.frame(

    ## Rates & probabilities
    p_pfspd = df_tp_pfs[, 2],
    p_pfsd = df_tp_pfs[, 3],
    p_pdd = rbeta(n_sim, 20, 80),
    p_dd = rep(1, n_sim),
    p_ae = rbeta(n_sim, 5, 95),

    ## Treatment effectiveness
    rr = exp(rnorm(n_sim, log(0.75), (log(0.88) - log(0.62))/ (2*1.96))),

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

# Function to perform the simulation
perform_simulation <- function(l_params,
                               verbose = FALSE) {

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
                                                    v_names_hs)) # a cohort state matrix, containing [n_cycles + 1] rows (because the first row is the start position), and as much column as the number of health states.
    ## fill this matrix with 0's for now

    ## We then need to define the starting positions of the cohort
    ## Assign all individuals to the "Healthy" health state in the first row of the `m_hs` matrix
    m_hs_comp[1, ] <- m_hs_int[1, ] <- v_start_hs

    ## Perform the matrix multiplication to determine the state membership over the cycles.
    ## To determine the number of individuals in each health state during each cycle, one need to multiply the vector of state membership in the previous cycle by the transition matrix
    ## Example: to calculate the number of individuals in each state in cycle 1, multiply the state membership in cycle 0 by the transition matrix
    ## HINT: to do so, use a a for loop over rows 2 to 41 of the `m_hs` matrix

    for(cycle in 1:n_cycles){

      # For your matrix of health state
      m_hs_comp[cycle + 1, ] <- m_hs_comp[cycle, ] %*% m_tp_comp # matrix multiplication
      m_hs_int[cycle + 1, ]  <- m_hs_int[cycle, ] %*% m_tp_int # matrix multiplication

    }


    # Calculate outcomes

    # Life years
    v_ly_comp <-  v_ly_int <- c("PFS" = 1,
                                "PD" = 1,
                                "D" = 0)

    ## Determine the number of life year gained over the cycles (reward at the end of the cycle!)
    v_t_ly_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_ly_comp
    v_t_ly_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_ly_int

    # QALY's
    ## Determine the number of QALYs won by 1 individual during 1 cycle
    v_qaly_comp <- v_qaly_int <- c("PFS" = u_pfs,
                                   "PD" = u_pd,
                                   "D" = u_d)

    ## Determine the number of QALYs gained over the cycles (reward at the end of the cycle!)
    v_t_qaly_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_qaly_comp
    v_t_qaly_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_qaly_int

    # Costs
    ## Determine the costs accrued by 1 individual during 1 cycle
    v_c_comp <- c("PFS" = c_pfs,
                  "PD" = c_pd,
                  "D" = c_d)

    v_c_int <- c("PFS" = c_pfs + c_thx,
                 "PD" = c_pd,
                 "D" = c_d)

    ## Determine the costs accrued over the cycles (reward at the end of the cycle!)
    v_t_c_comp <- m_hs_comp[2:nrow(m_hs_comp),] %*% v_c_comp
    v_t_c_int  <- m_hs_int[2:nrow(m_hs_int),] %*% v_c_int

    # Calculate discounted results

    # Life years
    ## Total discounted life years, using matrix multiplication
    n_t_ly_comp_d <- t(v_t_ly_comp) %*% v_dw_e
    n_t_ly_int_d  <- t(v_t_ly_int) %*% v_dw_e

    t_ly_pfs_comp <- sum(t(m_hs_comp[2:nrow(m_hs_comp), 1]) %*% v_dw_e)
    t_ly_pd_comp  <- sum(t(m_hs_comp[2:nrow(m_hs_comp), 2]) %*% v_dw_e)
    t_ly_pfs_int  <- sum(t(m_hs_int[2:nrow(m_hs_int), 1]) %*% v_dw_e)
    t_ly_pd_int   <- sum(t(m_hs_int[2:nrow(m_hs_int), 2]) %*% v_dw_e)

    # QALYs
    ## Total discounted qalys, using matrix multiplication
    t_qaly_ae_int <- n_ind * p_ae * u_ae # total qaly loss adverse events

    n_t_qaly_comp_d <- t(v_t_qaly_comp) %*% v_dw_e
    n_t_qaly_int_d  <- t(v_t_qaly_int) %*% v_dw_e - t_qaly_ae_int

    t_qaly_pfs_comp <- sum(t(m_hs_comp[2:nrow(m_hs_comp), 1] * u_pfs) %*% v_dw_e)
    t_qaly_pd_comp  <- sum(t(m_hs_comp[2:nrow(m_hs_comp), 2] * u_pd) %*% v_dw_e)
    t_qaly_pfs_int  <- sum(t(m_hs_int[2:nrow(m_hs_int), 1] * u_pfs) %*% v_dw_e)
    t_qaly_pd_int   <- sum(t(m_hs_int[2:nrow(m_hs_int), 2] * u_pd) %*% v_dw_e)

    # Costs
    ## Total discounted costs, using matrix multiplication
    t_c_ae_int <- n_ind * p_ae * c_ae # total costs adverse events

    n_t_c_comp_d <- t(v_t_c_comp) %*% v_dw_c
    n_t_c_int_d  <- t(v_t_c_int) %*% v_dw_c + t_c_ae_int

    t_c_pfs_comp <- sum(t(m_hs_comp[2:nrow(m_hs_comp), 1] * c_pfs) %*% v_dw_c)
    t_c_pd_comp  <- sum(t(m_hs_comp[2:nrow(m_hs_comp), 2] * c_pd) %*% v_dw_c)
    t_c_pfs_int  <- sum(t(m_hs_int[2:nrow(m_hs_int), 1] * c_pfs) %*% v_dw_c)
    t_c_pd_int   <- sum(t(m_hs_int[2:nrow(m_hs_int), 2] * c_pd) %*% v_dw_c)

    # Mean discounted outcomes per individual
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
