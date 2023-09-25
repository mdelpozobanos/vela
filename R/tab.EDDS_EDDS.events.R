#' Filter EDDS_EDDS table
#'
#' @param prj A project list
#' @param key A character specifying the filter (see filter.example for details).
#' @param output_table A character specifying the name of the table to be
#'   created with the result of the filter.
#' @param sql_queries A list with SQL queries to be used in the final query.
#'   (see `filter.reference` for details).
#' @param ... Any additional parameters overwriting values in the project list (prj).
#' @return NA. The result is saved in the database.
#'
#' @seealso filter.reference, prj.set_up, filter.alg_list
#'
#' @author Marcos del Pozo Banos
#' @export
#'
tab.EDDS_EDDS.events <- function(...){

  # Define table specific variables
  calling_table = "EDDS_EDDS";
  table_vars = list(
    TAB = paste0("@T{", calling_table, "}"),
    tab = paste0("@t{", calling_table, "}"),
    src = calling_table,
    DT = "@v{tab}.ADMIN_ARR_DT",
    START_DT = "@v{tab}.ADMIN_ARR_DT",
    END_DT = "@v{tab}.ADMIN_END_DT",
    CODE = NA,
    CODE_FILTER = utils.replicate_character(
      "@v{tab}.DIAG_CD_@{n} in (@v{code_list})",
      1, 6, "or")
  )

  # Prepare input arguments.
  # + Project variables are tuned to the current table
  # + Project variables are updated with any additional user input
  # see `utils.tab.TABLE.events_args` for more details
  # + Additional variables are generated (e.g. `sql_info`)
  args = list(...);
  args$table_vars = table_vars;
  args = do.call(utils.tab.TABLE.events_args, args);

  # Apply filtering variables to the current table
  utils.tab.TABLE.events(
    prj = args$prj,
    output_table = args$output_table,
    key = args$key,
    sql_info = args$sql_info,
    max_code_depth = 3,
    echo = args$echo
  );

}
