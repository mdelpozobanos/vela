#' Return name of calling function
#'
#' @author Marcos del Pozo Banos
#' @export
#'
utils.fcn_name <- function(){
  call_str = deparse(sys.calls()[[sys.nframe() - 1]])
  str = substr(call_str, 1, gregexpr("(", call_str, fixed = TRUE)[[1]][[1]] - 1)
  return(str)
}
