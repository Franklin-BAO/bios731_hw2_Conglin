# scripts/07_run_parallel.R

maybe_clear_results <- function(force_rerun = FALSE) {
  if (isTRUE(force_rerun)) {
    unlink(here("data", "sim_results"), recursive = TRUE)
    dir.create(here("data", "sim_results"), showWarnings = FALSE)
  }
  invisible(TRUE)
}

run_all_scenarios_parallel <- function(scenarios, nSim, B, alpha, run_full = FALSE) {
  
  if (!isTRUE(run_full)) return(invisible(FALSE))
  
  plan(multisession)
  
  todo <- scenarios %>% filter(!file.exists(out_file))
  if (nrow(todo) == 0) return(invisible(TRUE))
  
  with_progress({
    handlers("txtprogressbar")
    p <- progressor(steps = nrow(todo))
    
    future_lapply(
      seq_len(nrow(todo)),
      function(i) {
        s <- todo[i, ]
        p(sprintf("Scenario %d/%d: n=%s, beta=%s, err=%s",
                  i, nrow(todo), s$n, s$beta_treat, s$err_type))
        
        run_one_scenario(
          n = s$n,
          beta_treat = s$beta_treat,
          err_type = s$err_type,
          nSim = nSim,
          B = B,
          alpha = alpha,
          out_file = s$out_file
        )
      }
    )
  })
  
  invisible(TRUE)
}
