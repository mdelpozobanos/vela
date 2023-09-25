#' Lazy check arguments list for `prj` at the beginning of the list, add it from
#' the global environment if not present, and return the full list.
#'
#' @author Marcos del Pozo Banos
#'
utils.lazy_prj <- function(args){
  # Lazily get `prj` from global environment if not passed
  if (length(args) == 0){
    args = c(list(prj = get("prj", envir = .GlobalEnv)), args);
  } else if ("prj" %in% names(args)){
    # Do nothing
  } else if (is.VELA.prj(args[[1]])){
    prj = args[[1]]
    args[[1]] = NULL
    args = c(list(prj = prj), args);
  } else {
    args = c(list(prj = get("prj", envir = .GlobalEnv)), args);
  }
  return(args)
}
