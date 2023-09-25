#' Lazy check kwargs and get default values from the global variable `prj`
#' @author Marcos del Pozo Banos
#'
utils.sql.check_kwargs <- function(kwargs){
  # Lazily get `prj` from global environment if not passed
  if ("prj" %in% kwargs){
    # Do nothing
  } else if (is.VELA.prj(kwargs[[1]])){
    kwargs$prj = kwargs[[1]]
    kwargs[[1]] = NULL
  } else {
    kwargs$prj = get("prj", envir = .GlobalEnv);
  }
  # Add `db_conn` argument from `prj`
  kwargs$db_conn = prj$conx;
  # Add `echo` if not specified
  if ("echo" %nin% kwargs){
    # Expects global variable called `prj`
    kwargs$echo = FALSE;
  }
  return(kwargs);
}
