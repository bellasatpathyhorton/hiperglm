#' @noRd

pseudoinverse_finder <- function(design, outcome) {
  pseudoinverse_coef = solve(t(design) %*% design, t(design) %*% outcome)
  return(pseudoinverse_coef)
}


#' @noRd

BFGS_finder <- function(design, outcome) {
  optimal = stats::optim(par = rnorm(n = dim(design)[2]), fn = log_likelihood, design = design, outcome = outcome, gr = gradient, method = "BFGS")
  BFGS_coef = optimal$par
  return(BFGS_coef)
}

