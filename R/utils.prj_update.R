#' Update a `prj` variable with the specified arguments.
#'
#' Most of VELA's functions share a common structure of input arguments.
#' They expect a `prj` argument and allow additional input arguments that
#' overwrite variables in the specified `prj` variable. This function updates
#' `prj` with such additional arguments.
#'
#' @param prj A list() with project variables.
#' @param args A list() with modifiers to `prj`.
#'
#' @seealso prj.set_up
#'
#' @author Marcos del Pozo Banos
#' @keywords internal
#'
utils.prj_update <- function(prj, kwargs){

  # Update prj with passed arguments
  for (arg_n in names(kwargs)){
    if (!(arg_n %in% names(prj))){
      stop(paste0("Unused input argument ", arg_n));
    }
    if (is.list(prj[[arg_n]])) {
      prj[[arg_n]] = list_update(prj[[arg_n]], kwargs[[arg_n]], insert.new=TRUE);
    } else {
      prj[[arg_n]] = kwargs[[arg_n]];
    }
  }

  return(prj)

}
