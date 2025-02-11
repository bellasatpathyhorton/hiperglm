#' @noRd

gradient <- function(par, design, outcome, noise_var = 1) {
  yhat <- design %*% par
  residual <- outcome - yhat
  gradient <- (1/noise_var)*t(residual) %*% design
  return(gradient)
}
