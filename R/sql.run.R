#' Run an SQL query. Lazy wraper around SAILDBUtils::runSQL
#'
#' See `SAILDBUtils::runSQL` for a list of input and output arguments.
#' If parameters `db_conn` and `echo` are not specified, the function expected to
#' find varaible `prj` (see `prj.set_up`) in the global environment, and will
#' populate `db_conn` and `echo` from `prj`.
#'
#' @author Marcos del Pozo Banos
#' @export
#'
sql.run <- function(...){

  # Extract arguments
  args = utils.lazy_prj(list(...))
  kwargs = utils.check_kwargs(args,
                              required = c("prj", "query"),
                              optional = list(silent = TRUE, echo = FALSE));
  prj = kwargs$prj
  db_conn = prj$conx
  query = kwargs$query
  echo = kwargs$echo
  query = utils.qq(query, prj)

  # Interpolate SQL code
  query = utils.qq(query, prj)

  return(SAILDBUtils::runSQL(db_conn = db_conn, query = query, echo = echo))
}
