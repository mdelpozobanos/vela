#' Filter OPDW_OUTPATIENTS_DIAG table
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
tab.OPDW_OUTPATIENTS_DIAG.events <- function(...){

  # Define table specific variables
  calling_table = "OPDW_OUTPATIENTS_DIAG";
  table_vars = list(
    TAB = paste0("@T{", calling_table, "}"),
    tab = paste0("@t{", calling_table, "}"),
    src = calling_table,
    DT = "@t{OPDW_OUTPATIENTS}.ATTEND_DT",
    START_DT = "@t{OPDW_OUTPATIENTS}.ATTEND_DT",
    END_DT = "@t{OPDW_OUTPATIENTS}.ATTEND_DT",
    CODE = "
      rpad(
        trim(@v{tab}.DIAG_CD_123)
          || trim(translate(coalesce(@v{tab}.DIAG_CD_4, ''), '', '.,-!#&*/\\~`]QWERTYUIOPASDFGHJKLZXCCVBNM'))
          || trim(translate(coalesce(@v{tab}.DIAG_CD_56, ''), '', '.,-!#&*/\\~`]QWERTYUIOPASDFGHJKLZXCCVBNM'))
        , 6, '.')
    ",
    NOTES = "'' ||
        case
          when @t{OPDW_OUTPATIENTS}.ATTEND_CD is NULL then '[ATTEND_CD: invalid]'
          when @t{OPDW_OUTPATIENTS}.ATTEND_CD in ('5', '6') then '[ATTEND_CD: seen]'
          when @t{OPDW_OUTPATIENTS}.ATTEND_CD in ('2', '3', '4', '7', '8') then '[ATTEND_CD: not seen]'
          end || '[DIAG_NUM: ' || lpad(coalesce(@v{tab}.DIAG_NUM, 99), 2, 0) || ']'",

    # Additional variables that need to be modified
    # ---------------------------------------------

    ALF = "@t{OPDW_OUTPATIENTS}.ALF_@v{enc}"
  )

  # Prepare input arguments.
  # + Project variables are tuned to the current table
  # + Project variables are updated with any additional user input
  # see `utils.tab.TABLE.events_args` for more details
  # + Additional variables are generated (e.g. `sql_info`)
  args = list(...);
  args$table_vars = table_vars;
  args = do.call(utils.tab.TABLE.events_args, args);

  # ALF comes from OPDW_OUTPATIENTS. OPDW_OUTPATIENTS should be second
  args$sql_info$from = rbind(
    args$sql_info$from[1, ],
    data.frame(
      full_name = "@T{OPDW_OUTPATIENTS}",
      nickname = "@t{OPDW_OUTPATIENTS}",
      where = I(list(list("@v{tab}.PROV_UNIT_CD = @t{OPDW_OUTPATIENTS}.PROV_UNIT_CD",
                          "@v{tab}.CASE_REC_NUM_@v{enc} = @t{OPDW_OUTPATIENTS}.CASE_REC_NUM_@v{enc}"
      ))),
      join_type = "inner"
    ),
    args$sql_info$from[-1, ]
  )

  # Apply filtering variables to the current table
  utils.tab.TABLE.events(
    prj = args$prj,
    output_table = args$output_table,
    key = args$key,
    sql_info = args$sql_info,
    max_code_depth = 5,
    echo = args$echo
  );

}
