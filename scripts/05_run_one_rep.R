# scripts/05_run_one_rep.R

run_one_rep <- function(n, beta_treat, err_type, B, alpha = 0.05) {
  
  fit <- list(theta_hat = NA_real_, se_hat = NA_real_)
  for (attempt in 1:10) {
    dat <- generate_data(n = n, beta_treat = beta_treat, err_type = err_type)
    fit <- fit_fast_treat(dat)
    if (is.finite(fit$theta_hat) && is.finite(fit$se_hat) && fit$se_hat > 0) break
  }
  
  theta_hat <- fit$theta_hat
  se_hat <- fit$se_hat
  
  if (!is.finite(theta_hat) || !is.finite(se_hat) || se_hat <= 0) {
    return(tibble(
      theta_hat = NA_real_, se_wald = NA_real_, se_boot = NA_real_,
      ci_wald_l = NA_real_, ci_wald_u = NA_real_,
      ci_perc_l = NA_real_, ci_perc_u = NA_real_,
      ci_bt_l = NA_real_,   ci_bt_u = NA_real_,
      time_wald = NA_real_, time_perc = NA_real_, time_bt = NA_real_
    ))
  }
  
  t_wald <- system.time({
    ci_wald <- wald_ci(theta_hat, se_hat, alpha = alpha)
  })["elapsed"]
  
  outer <- bootstrap_outer_fast(dat, B = B)
  
  t_perc <- system.time({
    perc <- bootstrap_percentile_ci_from_outer(outer$theta_star, alpha = alpha)
    ci_perc <- perc$ci
    se_boot <- perc$boot_se
  })["elapsed"]
  
  t_bt <- system.time({
    bt <- bootstrap_t_ci_fast(theta_hat, outer_obj = outer, alpha = alpha)
    ci_bt <- bt$ci
  })["elapsed"]
  
  tibble(
    theta_hat = theta_hat,
    se_wald = se_hat,
    se_boot = se_boot,
    ci_wald_l = ci_wald["lower"],
    ci_wald_u = ci_wald["upper"],
    ci_perc_l = ci_perc["lower"],
    ci_perc_u = ci_perc["upper"],
    ci_bt_l = ci_bt["lower"],
    ci_bt_u = ci_bt["upper"],
    time_wald = as.numeric(t_wald),
    time_perc = as.numeric(t_perc),
    time_bt = as.numeric(t_bt)
  )
}

run_one_scenario <- function(n, beta_treat, err_type, nSim, B, alpha, out_file, print_every = 25) {
  cat("Start:", out_file, "\n")
  t0 <- Sys.time()
  
  res_list <- vector("list", nSim)
  for (i in seq_len(nSim)) {
    if (i %% print_every == 0 || i == 1 || i == nSim) {
      cat("  replicate", i, "of", nSim,
          sprintf("(n=%s, beta=%s, err=%s)", n, beta_treat, err_type),
          "\n")
    }
    
    res_list[[i]] <- run_one_rep(
      n = n,
      beta_treat = beta_treat,
      err_type = err_type,
      B = B,
      alpha = alpha
    )
  }
  
  res <- bind_rows(res_list) %>%
    mutate(
      n = n,
      beta_true = beta_treat,
      err_type = err_type,
      sim_id = row_number()
    )
  
  saveRDS(res, out_file)
  
  cat("Done:", out_file,
      "Elapsed:", round(difftime(Sys.time(), t0, units = "mins"), 2), "min\n")
  
  out_file
}
