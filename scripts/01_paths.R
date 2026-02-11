# scripts/01_paths.R

init_project_paths <- function() {
  here::i_am("HW2.Rmd")
  
  dir.create(here("scripts"), showWarnings = FALSE)
  dir.create(here("data"), showWarnings = FALSE)
  dir.create(here("data", "sim_results"), showWarnings = FALSE)
  dir.create(here("output"), showWarnings = FALSE)
  dir.create(here("output", "figures"), showWarnings = FALSE)
  
  invisible(TRUE)
}
