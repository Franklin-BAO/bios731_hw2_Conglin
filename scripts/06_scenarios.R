# scripts/06_scenarios.R

make_scenarios <- function(MODE) {
  expand.grid(
    n = c(10, 50, 500),
    beta_treat = c(0, 0.5, 2),
    err_type = c("normal", "t3"),
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(
      scenario_id = sprintf("n%s_b%s_%s", n, beta_treat, err_type),
      out_file = here("data", "sim_results", paste0(scenario_id, "_", MODE, ".rds"))
    )
}
