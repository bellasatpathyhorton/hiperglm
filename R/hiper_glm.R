#' @export
hiper_glm <- function(design, outcome, model = "linear", mle_finder) {
  supported_model <- c("linear", "logit")
  if (!(model %in% supported_model)) {
    stop(sprintf("The model %s is not supported", model))
  }

  if (mle_finder == "pseudoinverse") {
    coef = pseudoinverse_finder(design, outcome)
  }

  else if (mle_finder == "BFGS") {
    coef = BFGS_finder(design, outcome)
  }

  hglm_out <- list(coef)
  class(hglm_out) <- "hglm"
  return(hglm_out)
}
