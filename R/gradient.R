#' @noRd

gradient <- function(par, design, outcome, noise_var = 1) {
  yhat <- design %*% par
  residual <- outcome - yhat
  gradient <- t(design) %*% residual / noise_var
  return(gradient)
}


