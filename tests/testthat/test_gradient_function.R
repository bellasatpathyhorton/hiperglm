test_that("analytical and numerical gradients align", {
  # simulate data
  n_obs <- 32
  n_pred <- 4
  data <- simulate_data(n_obs, n_pred, seed = 1918)
  outcome <- data$outcome
  design <- data$design
  par = rnorm(n = dim(design)[2])

  # generate analytical gradient using gradient function
  analytical_grad <- gradient(par, design, outcome)

  #generate numerical gradient using function above
  numerical_grad <- approx_grad(function(par) log_likelihood(par, design, outcome), par)

  # compare the two
  expect_true(are_all_close(
    analytical_grad, numerical_grad, abs_tol = 1e-6, rel_tol = 1e-6
  ))
})
