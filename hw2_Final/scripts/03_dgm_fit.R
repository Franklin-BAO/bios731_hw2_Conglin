# scripts/03_dgm_fit.R

generate_data <- function(n, beta_treat, err_type = c("normal", "t3"), beta0 = 0) {
  err_type <- match.arg(err_type)
  if (n < 2) stop("n must be >= 2")
  
  x <- c(0, 1, rbinom(n - 2, size = 1, prob = 0.5))
  x <- sample(x)
  
  if (err_type == "normal") {
    eps <- rnorm(n, mean = 0, sd = sqrt(2))
  } else {
    nu <- 3
    u <- rt(n, df = nu)
    eps <- u * sqrt(2 * (nu - 2) / nu)
  }
  
  y <- beta0 + beta_treat * x + eps
  data.frame(y = y, x = x)
}

fit_fast_treat <- function(dat) {
  x <- dat$x
  y <- dat$y
  
  y1 <- y[x == 1]
  y0 <- y[x == 0]
  
  n1 <- length(y1); n0 <- length(y0)
  if (n1 < 2 || n0 < 2) return(list(theta_hat = NA_real_, se_hat = NA_real_))
  
  theta_hat <- mean(y1) - mean(y0)
  s1 <- sd(y1); s0 <- sd(y0)
  
  se_hat <- sqrt((s1^2) / n1 + (s0^2) / n0)
  list(theta_hat = theta_hat, se_hat = se_hat)
}
