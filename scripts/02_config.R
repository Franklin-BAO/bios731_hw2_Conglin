# scripts/02_config.R

get_sim_config <- function(MODE = c("final", "quick"),
                           alpha = 0.05,
                           seed = 42,
                           run_full = FALSE,
                           force_rerun = FALSE) {
  MODE <- match.arg(MODE)
  
  if (MODE == "quick") {
    nSim <- 20
    B <- 100
    B_inner <- 20
  } else {
    nSim <- 500
    B <- 500
    B_inner <- 100
  }
  
  list(
    MODE = MODE,
    alpha = alpha,
    seed = seed,
    nSim = nSim,
    B = B,
    B_inner = B_inner,
    run_full = run_full,
    force_rerun = force_rerun
  )
}
