#' Filter PEDW_SINGLE_DIAG table
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
tab.PEDW_SINGLE_DIAG.events <- function(...){

  # Define table specific variables
  calling_table = "PEDW_SINGLE_DIAG";
  table_vars = list(
    TAB = paste0("@T{", calling_table, "}"),
    tab = paste0("@t{", calling_table, "}"),
    src = calling_table,
    DT = "@v{tab}.START_DATE",
    START_DT = "@v{tab}.START_DATE",
    END_DT = "@v{tab}.END_DATE",
    # CODE is split across columns and have unwanted characters that need to be removed.
    CODE = "
      rpad(
        trim(left(@v{tab}.DIAG_CD_1234, 3))
          || trim(translate(coalesce(right(@v{tab}.DIAG_CD_1234, 1), ''), '', '.,-!#&*/\\~`]QWERTYUIOPASDFGHJKLZXCCVBNM'))
        , 4, '.')
      ",

    # Additional variables that need to be modified
    # ---------------------------------------------

    LNK_STS_CD = "9",
    # NOTE ON DIAG_NUM!!!
    # ==========================================================================
    # DIAG_NUM was originally using the minimum diag number. But "min" cannot be used during "group by" should
    # it has been changed to record all DIAG_NUMs instead.
    # '[DIAG_NUM: ' || lpad(min(coalesce(@v{tab}.DIAG_NUM, 99)), 2, 0) || ']'
    # NOTES on SPECIALTY
    # Specialty was usually recorded in the notes but this slowed down the query drastically
    # since it needed an extra join (see code further below)
    # || ' [SPEC: ' || coalesce(trim(spec.DESCRIPTION), 'NA') || ']'
    NOTES = "'[DIAG_NUM: ' || lpad(coalesce(@v{tab}.DIAG_SEQ, 99), 2, 0) || ']'
          || '[EPI_SEQ: ' || lpad(coalesce(@v{tab}.EPI_SEQ, 99), 2, 0) || ']'"
  )

  # Prepare input arguments
  args = list(...);
  args$table_vars = table_vars;
  args = do.call(utils.tab.TABLE.events_args, args);

  # Filter
  utils.tab.TABLE.events(
    prj = args$prj,
    output_table = args$output_table,
    key = args$key,
    sql_info = args$sql_info,
    max_code_depth = 4,
    echo = args$echo
  );

}
