#' @export
print.hglm <- function(hglm_out) {
  cat("'hiper_glm' output\n")
}

#' @export
coef.hglm <- function(hglm_out) {
  return(coef)  # should be hglm_out$coef, but only returns NULL if i do that
}

#' @export
vcov.hglm <- function(hglm_out) {
  warning("This function is yet to be implemented")
}
