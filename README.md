# Reproducibility and Running the Simulation

This project is fully reproducible.

After cloning the repository, you can directly open `HW2.Rmd` and knit the report.
All simulation results (`.rds` files) have already been generated and saved under:

```bash
data/sim_results/
```

Therefore, by default, knitting will only load the saved results and reproduce the figures, which is much faster and avoids re-running the computationally expensive simulations.

# Default (recommended for knitting)
```
cfg <- get_sim_config(
  MODE = "final",
  alpha = 0.05,
  seed = 42,
  run_full = FALSE,
  force_rerun = FALSE
)
```
Meaning:
* run_full = FALSE → do NOT rerun simulations, only read saved results
* force_rerun = FALSE → do NOT delete existing .rds files
This allows the report to knit quickly using the pre-generated data.

# If you want to regenerate all simulation data from scratch
If you prefer to fully reproduce the simulation yourself, change both options to TRUE:
```
cfg <- get_sim_config(
  MODE = "final",
  alpha = 0.05,
  seed = 42,
  run_full = TRUE,
  force_rerun = TRUE
)
```

Meaning:
* force_rerun = TRUE → delete existing results
* run_full = TRUE → rerun all scenarios and regenerate .rds files
Note: This step can take several minutes because it runs all bootstrap simulations across all scenarios.

# Recommendation
* For grading / knitting → keep both FALSE
* For full reproducibility testing → set both TRUE
