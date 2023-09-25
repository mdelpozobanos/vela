#' Mimics SQL coalesce function in R
#'
#' @author Marcos del Pozo Banos
#' @export
#'
coalesce <- function(..., default = NA) {
  return(apply(cbind(..., default), 1, function(x) x[which(!is.na(x))[1]]))
}
