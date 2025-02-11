approx_grad <- function(func, par, dx = .Machine$double.eps^(1/3)) {
  gradient_vector <- rep(0, length(par))

  for (i in 1:length(par)) {
    a_vector <- par
    b_vector <- par
    a_vector[i] = par[i]+dx
    b_vector[i] = par[i]-dx
    gradient_vector[i] = (func(a_vector)-func(b_vector))/(2*dx)
  }

  return(gradient_vector)
}

n_obs <- 32
n_pred <- 4
data <- simulate_data(n_obs, n_pred, seed=1918)
outcome <- data$outcome
design <- data$design

par = rnorm(n = dim(design)[2])

analytical_grad <- gradient(par, design, outcome)
numerical_grad <- approx_grad(function(par) log_likelihood(par, design, outcome), par)

testthat::expect_true(are_all_close(
  analytical_grad, numerical_grad, abs_tol = Inf, rel_tol = 1e-3
))
