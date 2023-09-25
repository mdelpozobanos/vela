#' Filter ADDE_DEATHS table
#'
#' @param prj A project list
#' @param key A character specifying the filter. (see filter.example for details).
#' @param output_table A character specifying the name of the table to be
#'   created with the result of the filter.
#' @param sql_queries A list with SQL queries to be used in the final query.
#'   (see filter.example for details).
#' @param ... Any additional parameters overwriting values in the project list (prj).
#' @return NA. The result is saved in the database.
#'
#' @seealso filter.example, prj.set_up, filter.alg_list
#'
#' @author Marcos del Pozo Banos
#' @export
#'
tab.ADDE_DEATHS.events <- function(...){


  # Define table specific variables
  calling_table = "ADDE_DEATHS";
  table_vars = list(
    TAB = paste0("@T{", calling_table, "}"),
    tab = paste0("@t{", calling_table, "}"),
    src = calling_table,
    DT = "@v{tab}.DEATH_DT",
    START_DT = "@v{tab}.DEATH_DT",
    END_DT = "@v{tab}.DEATH_DT",
    NOTES = "'DEATH_DT_VALID: ' || @v{tab}.DEATH_DT_VALID",
    CODE = NA,
    CODE_FILTER = paste(
      "RPAD(TRIM(@v{tab}.DEATHCAUSE_DIAG_UNDERLYING_CD), 4, '.') in (@v{code_list}) or",
      utils.replicate_character(
        paste("RPAD(TRIM(@v{tab}.DEATHCAUSE_DIAG_@{n}_CD), 4, '.') in (@v{code_list})"),
        1, 8, "or")
    )
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
    max_code_depth = 4,
    echo = args$echo
  );

}
