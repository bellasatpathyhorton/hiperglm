#' @noRd

log_likelihood <- function(par, design, outcome, noise_var = 1) {
  yhat <- design %*% par
  residual <- outcome - yhat
  dimension <- length(outcome)
  likelihood <- -(0.5*dimension*log(2*pi)) - (0.5*log(noise_var)) -((0.5/noise_var)*sum(residual^2))
  return(likelihood)
}
