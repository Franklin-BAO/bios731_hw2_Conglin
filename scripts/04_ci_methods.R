# scripts/04_ci_methods.R

wald_ci <- function(theta_hat, se_hat, alpha = 0.05) {
  z <- qnorm(1 - alpha / 2)
  c(lower = theta_hat - z * se_hat, upper = theta_hat + z * se_hat)
}

bootstrap_outer_fast <- function(dat, B, fit_fun = fit_fast_treat) {
  n <- nrow(dat)
  idx_mat <- replicate(B, sample.int(n, size = n, replace = TRUE))
  
  theta_star <- numeric(B)
  se_star <- numeric(B)
  
  for (b in seq_len(B)) {
    dat_b <- dat[idx_mat[, b], ]
    fb <- fit_fun(dat_b)
    theta_star[b] <- fb$theta_hat
    se_star[b] <- fb$se_hat
  }
  
  list(theta_star = theta_star, se_star = se_star)
}

bootstrap_percentile_ci_from_outer <- function(theta_star, alpha = 0.05) {
  thetas <- theta_star[is.finite(theta_star)]
  if (length(thetas) < 50) {
    return(list(ci = c(lower = NA_real_, upper = NA_real_),
                boot_se = NA_real_))
  }
  
  ci <- quantile(thetas, probs = c(alpha/2, 1 - alpha/2), names = FALSE)
  names(ci) <- c("lower", "upper")
  
  list(ci = ci, boot_se = sd(thetas))
}

bootstrap_t_ci_fast <- function(theta_hat, outer_obj, alpha = 0.05) {
  theta_star <- outer_obj$theta_star
  se_star <- outer_obj$se_star
  
  ok <- is.finite(theta_star) & is.finite(se_star) & (se_star > 0)
  if (sum(ok) < 50) {
    return(list(ci = c(lower = NA_real_, upper = NA_real_),
                tstars = numeric(0),
                se_theta_hat = NA_real_))
  }
  
  tstars <- (theta_star[ok] - theta_hat) / se_star[ok]
  
  se_theta_hat <- sd(theta_star[ok])
  if (!is.finite(se_theta_hat) || se_theta_hat <= 0) {
    return(list(ci = c(lower = NA_real_, upper = NA_real_),
                tstars = tstars,
                se_theta_hat = se_theta_hat))
  }
  
  q_low  <- quantile(tstars, probs = alpha/2,     names = FALSE)
  q_high <- quantile(tstars, probs = 1 - alpha/2, names = FALSE)
  
  ci <- c(
    lower = theta_hat - q_high * se_theta_hat,
    upper = theta_hat - q_low  * se_theta_hat
  )
  
  list(ci = ci, tstars = tstars, se_theta_hat = se_theta_hat)
}
