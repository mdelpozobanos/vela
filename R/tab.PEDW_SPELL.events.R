#' Filter PEDW_SPELL table
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
tab.PEDW_SPELL.events <- function(...){

  # Define table specific variables
  calling_table = "PEDW_SPELL";
  table_vars = list(
    TAB = paste0("@T{", calling_table, "}"),
    tab = paste0("@t{", calling_table, "}"),
    src = calling_table,
    DT = "@t{PEDW_SPELL}.ADMIS_DT",
    START_DT = "@t{PEDW_SPELL}.ADMIS_DT",
    END_DT = "@t{PEDW_SPELL}.DISCH_DT",
    # CODE is split across columns and have unwanted characters that need to be removed.
    # CODE = "RPAD(regexp_replace(@v{tab}DIAG_CD, '[A-Z \.,\-!#&\*/\\~`\]]*', ''), 6, '.')",
    CODE = NA,

    # Additional variables that need to be modified
    # ---------------------------------------------

    ALF = "@t{PEDW_SPELL}.ALF_@v{enc}",
    LNK_STS_CD = "
      case @t{PEDW_SPELL}.ALF_STS_CD
        when  '1' then 3  -- Deterministic
        when  '2' then 3  -- Deterministic
        when  '4' then 3  -- Deterministic
        when '39' then 2  -- Probabilistic w. score > 0.9
        when '35' then 1  -- Probabilistic w. score > 0.5 but < 0.9
        when '99' then 0  -- Probabilistic w. score < 0.5
        else -1
        end
      ",
    # NOTE ON DIAG_NUM!!!
    # ==========================================================================
    # DIAG_NUM was originally using the minimum diag number. But "min" cannot be used during "group by" should
    # it has been changed to record all DIAG_NUMs instead.
    # '[DIAG_NUM: ' || lpad(min(coalesce(@v{tab}.DIAG_NUM, 99)), 2, 0) || ']'
    # NOTES on SPECIALTY
    # Specialty was usually recorded in the notes but this slowed down the query drastically
    # since it needed an extra join (see code further below)
    # || ' [SPEC: ' || coalesce(trim(spec.DESCRIPTION), 'NA') || ']'
    NOTES = "
          '[ADMIS: ' || case
                            when @t{PEDW_SPELL}.ADMIS_MTHD_CD is NULL then 'NA'
                            when @t{PEDW_SPELL}.ADMIS_MTHD_CD in ('11', '12', '13', '14', '15') then 'Elective'
                            when @t{PEDW_SPELL}.ADMIS_MTHD_CD in ('20', '21', '22', '23', '24', '25', '27', '28', '29') then 'Emergency'
                            when @t{PEDW_SPELL}.ADMIS_MTHD_CD in ('31', '32') then 'Maternity'
                            when @t{PEDW_SPELL}.ADMIS_MTHD_CD in ('81', '82', '83', '98', '99') then 'Other'
                            else 'NA'
                        end || ']'"
  )

  # Prepare input arguments
  args = list(...);
  args$table_vars = table_vars;
  args = do.call(utils.tab.TABLE.events_args, args);

  # ALF comes from PEDW_SPELL. PEDW_SPELL should be second
  args$sql_info$from = rbind(
    args$sql_info$from[1, ],
    data.frame(
      full_name = "@T{PEDW_SPELL}",
      nickname = "@t{PEDW_SPELL}",
      where = I(list(list("@v{tab}.PROV_UNIT_CD = @t{PEDW_SPELL}.PROV_UNIT_CD",
                          "@v{tab}.SPELL_NUM_@v{enc} = @t{PEDW_SPELL}.SPELL_NUM_@v{enc}"
      ))),
      join_type = "inner"
    ),
    args$sql_info$from[-1, ]
  )

  # SPECIALY comes from SAILW0843V.PEDW_SPEC_CD with a left join.
  # Add this at the end
  # args$sql_info$from = rbind(
  #   args$sql_info$from,
  #   data.frame(
  #     full_name = "SAILW0843V.PEDW_SPEC_CD",
  #     nickname = "spec",
  #     where = I(list(list("@t{PEDW_SPELL}.ADMIS_SPEC_CD = spec.CD"))),
  #     join_type = "left"
  #   )
  # )
  # SPECIALTY no longer recorded, see NOTES above

  # Filter
  utils.tab.TABLE.events(
    prj = args$prj,
    output_table = args$output_table,
    key = args$key,
    sql_info = args$sql_info,
    max_code_depth = 0,
    echo = args$echo
  );

}
