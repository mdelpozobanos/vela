#' Drop table.
#'
#' @param prj VELA.prj
#' @param table_name Name of table to drop.
#' @param silent If TRUE, it will not rise any errors or warnings if the table
#'  does not exist.
#'
#' @author Marcos del Pozo Banos
#' @export
#'
sql.drop_table <- function (...){ # (prj, table_name, silent) or (table_name, silent)

  # Extract arguments
  args = utils.lazy_prj(list(...))
  kwargs = utils.check_kwargs(args,
                              required = c("prj", "table_name"),
                              optional = list(silent = TRUE, echo = FALSE));
  prj = kwargs$prj
  db_conn = prj$conx
  table_name = kwargs$table_name
  silent = kwargs$silent
  echo = kwargs$echo

  # Drop table
  res <- tryCatch({
    SAILDBUtils::runSQL(db_conn, utils.qq(paste("DROP TABLE", table_name), prj),
                        echo = echo)
    res <- TRUE
  }, error = function(e) {
    if (!silent){
      message("ERROR: Error occurred when trying to drop table. Please ensure that the table exists and that\n                    you have DROP priviledges on it.")
    }
    return(FALSE)
  })
  return(res)
}
