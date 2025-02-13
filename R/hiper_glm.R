#' @export
hiper_glm <- function(design, outcome, model = "linear", mle_finder) {
  supported_model <- c("linear", "logit")
  if (!(model %in% supported_model)) {
    stop(sprintf("The model %s is not supported", model))
  }

  if (mle_finder == "pseudoinverse") {
    coef = solve(t(design) %*% design, t(design) %*% outcome)
  }

  else if (mle_finder == "BFGS") {
    optimal = stats::optim(par = rnorm(n = dim(design)[2]), fn = log_likelihood, design = design, outcome = outcome, gr = gradient, method = "BFGS")
    coef = optimal$par
  }
  hglm_out <- list(coef)
  class(hglm_out) <- "hglm"
  return(hglm_out)
}
