# scripts/08_summarize_plot.R

load_all_results <- function(MODE) {
  rds_files <- list.files(
    path = here("data", "sim_results"),
    pattern = paste0("_", MODE, "\\.rds$"),
    full.names = TRUE
  )
  stopifnot(length(rds_files) > 0)
  
  rds_files %>%
    set_names(nm = basename(.)) %>%
    map(readRDS) %>%
    bind_rows(.id = "source_file")
}

summarize_all <- function(res_all) {
  
  ci_long <- res_all %>%
    transmute(
      n, beta_true, err_type, sim_id, theta_hat,
      wald_l = ci_wald_l, wald_u = ci_wald_u,
      perc_l = ci_perc_l, perc_u = ci_perc_u,
      bt_l   = ci_bt_l,   bt_u   = ci_bt_u
    ) %>%
    pivot_longer(
      cols = c(wald_l, wald_u, perc_l, perc_u, bt_l, bt_u),
      names_to = c("method", ".value"),
      names_pattern = "^(wald|perc|bt)_(l|u)$"
    ) %>%
    mutate(
      method = recode(method,
                      "wald" = "Wald",
                      "perc" = "Bootstrap percentile",
                      "bt"   = "Bootstrap-t"),
      covered = ifelse(is.finite(l) & is.finite(u),
                       (l <= beta_true & beta_true <= u),
                       NA)
    )
  
  summ_bias <- res_all %>%
    group_by(n, beta_true, err_type) %>%
    summarize(bias = mean(theta_hat - beta_true, na.rm = TRUE), .groups = "drop")
  
  summ_coverage <- ci_long %>%
    group_by(n, beta_true, err_type, method) %>%
    summarize(
      coverage = mean(covered, na.rm = TRUE),
      n_eff = sum(!is.na(covered)),
      .groups = "drop"
    )
  
  time_long <- res_all %>%
    select(n, beta_true, err_type, sim_id, time_wald, time_perc, time_bt) %>%
    pivot_longer(
      cols = c(time_wald, time_perc, time_bt),
      names_to = "method",
      values_to = "time_sec"
    ) %>%
    mutate(method = recode(method,
                           "time_wald" = "Wald",
                           "time_perc" = "Bootstrap percentile",
                           "time_bt"   = "Bootstrap-t"))
  
  summ_time <- time_long %>%
    group_by(n, beta_true, err_type, method) %>%
    summarize(
      time_median = median(time_sec, na.rm = TRUE),
      time_mean   = mean(time_sec, na.rm = TRUE),
      .groups = "drop"
    )
  
  se_long <- res_all %>%
    select(n, beta_true, err_type, sim_id, se_wald, se_boot) %>%
    pivot_longer(
      cols = c(se_wald, se_boot),
      names_to = "se_type",
      values_to = "se"
    ) %>%
    mutate(se_type = recode(se_type,
                            "se_wald" = "Plug-in SE (Wald)",
                            "se_boot" = "Bootstrap SD (theta*)")) %>%
    filter(is.finite(se))
  
  list(
    ci_long = ci_long,
    summ_bias = summ_bias,
    summ_coverage = summ_coverage,
    summ_time = summ_time,
    se_long = se_long
  )
}

make_and_save_plots <- function(summ, MODE) {
  
  p_bias <- ggplot(summ$summ_bias, aes(x = factor(beta_true), y = bias)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point() +
    geom_line(aes(group = 1)) +
    facet_grid(err_type ~ n, scales = "free_y") +
    labs(
      title = "Bias of treatment effect estimator",
      x = expression(beta[treat]~"(true)"),
      y = "Bias = E[theta_hat - beta_true]"
    )
  
  ggsave(
    filename = here("output", "figures", paste0("P1_4_bias_", MODE, ".png")),
    plot = p_bias, width = 8.5, height = 5.5, dpi = 300
  )
  
  p_cov <- ggplot(summ$summ_coverage, aes(x = method, y = coverage)) +
    geom_hline(yintercept = 0.95, linetype = "dashed") +
    geom_point() +
    facet_grid(err_type ~ n) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
      title = "Empirical coverage of 95% confidence intervals",
      x = "CI method",
      y = "Coverage"
    ) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
  
  ggsave(
    filename = here("output", "figures", paste0("P1_4_coverage_", MODE, ".png")),
    plot = p_cov, width = 9.5, height = 5.5, dpi = 300
  )
  
  p_se <- ggplot(summ$se_long, aes(x = se_type, y = se)) +
    geom_boxplot(outlier.size = 0.7) +
    facet_grid(err_type ~ n, scales = "free_y") +
    labs(
      title = "Distribution of SE estimates across simulation replicates",
      x = "SE type",
      y = "SE"
    ) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
  
  ggsave(
    filename = here("output", "figures", paste0("P1_4_se_dist_", MODE, ".png")),
    plot = p_se, width = 9.5, height = 5.5, dpi = 300
  )
  
  p_time <- ggplot(summ$summ_time, aes(x = method, y = time_median)) +
    geom_point() +
    facet_grid(err_type ~ n, scales = "free_y") +
    labs(
      title = "Computation time per replicate (median seconds)",
      x = "Method",
      y = "Median time (sec)"
    ) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
  
  ggsave(
    filename = here("output", "figures", paste0("P1_4_time_", MODE, ".png")),
    plot = p_time, width = 9.5, height = 5.5, dpi = 300
  )
  
  invisible(TRUE)
}
