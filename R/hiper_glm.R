#' @export
hiper_glm <- function(design, outcome, model = "linear", option = list()) {
  supported_model <- c("linear", "logit")
  if (!(model %in% supported_model)) {
    stop(sprintf("The model %s is not supported", model))
  }
  #TO DO: maximize likelihood
  coef = solve(t(design) %*% design, t(design) %*% outcome)
  hglm_out <- list(coef)
  class(hglm_out) <- "hglm"
  return(hglm_out)
}

#usethis::use_testthat()
