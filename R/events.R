#' Find events in project.
#'
#' Convenient function calling filtering function for individual tables.
#'
#' @param prj Project list (see `prj.set_up`)
#' @param output_table A character with the name of the output/resulting table.
#' @param sql_queries A list specified parts of the SQL query used to filter
#'  (see `filter.reference`).
#' @param event_args A list (with character indices) of events to filter. Each
#'  element is a list of table names and keys (i.e. the additional argument
#'  `...` above) to be used in multiple calls to filter.db. For each call, the
#'  index/name of the corresponding list element is used as `event_name`.
#' @param event_name A character with a common name used across all filtered #
#'  tables.
#' @param default_sql_queries An integer specifying the shape of the output.
#'  - 1 (default): The resulting table will have columns ALF, START_DT, END_DT,
#'    EVENT_NAME (with value `event_name`), SRC (e.g. 'WLGP', 'EDDS', 'PEDW',
#'    etc), NOTES and LNK_STS_CD.
#'  - 2 (default): The resulting table will have columns ALF, FIRST_DT (date of
#'    the first event found), LAST_DT (date of the last event found),
#'    EVENT_NAME (with value `event_name`), SRC (e.g. 'WLGP', 'EDDS', 'PEDW',
#'    etc), NOTES and LNK_STS_CD.
#' @param echo_progress If TRUE (default) it will print progress information.
#' @param ... Names of tables to filter and their filtering key (see `filter.reference`).
#'
#' @seealso prj.set_up, filter.reference
#'
#' @author Marcos del Pozo Banos
#' @export
#'
events <- function(...){

  args = list(...);
  # Check argument "prj"
  args = utils.lazy_prj(args)
  # Extract arguments by key or by position...
  # ... output_table
  tmp_tab = "SESSION.VELA__EVENTS"
  if ("output_table" %in% names(args)){
    output_table = args[["output_table"]]
    args[["output_table"]] = tmp_tab
  } else {
    output_table = args[[2]]
    args[[2]] = tmp_tab
  }
  # ... key_list
  if ("key_list" %in% names(args)){
    key_list = args[["key_list"]]
    args["key_list"] = NULL
    key_ind = "key"
  } else {
    key_list = args[[3]]
    key_ind = 3
  }
  # ... result_shape
  result_shape = args["result_shape"]
  args["result_shape"] = NULL

  # Argument "result_shape" is ignored if argument "select" is specified
  if ("select" %nin% names(args)){
    if (result_shape == "expanded"){

      # ALF, EVENT, START_DT, END_DT, SRC, NOTES, KEY, LNK_STS_CD
      select = c(
        "@v{ALF} as ALF_@v{enc}",
        "cast('@u{event_name}' as varchar(100)) as EVENT",
        "@v{START_DT} as START_DT",
        "@v{END_DT} as END_DT",
        "@v{src} as SRC",
        "@v{NOTES} as NOTES",
        "@v{KEY} as KEY",
        "max(@v{LNK_STS_CD}) as LNK_STS_CD"
      )
      group_by = c("@v{ALF}", "@v{START_DT}", "@v{END_DT}", "@v{NOTES}")

    } else if (result_shape == "collapsed"){

      # ALF, EVENT, FIRST_DT, LAST_DT, SRC, NOTES, KEY, LNK_STS_CD
      select = c(
        "@v{ALF} as ALF_@v{enc}",
        "cast('@u{event_name}' as varchar(100)) as EVENT",
        "min(@v{DT}) as FIRST_DT",
        "max(@v{DT}) as LAST_DT",
        "@v{src} as SRC",
        "@v{NOTES} as NOTES",
        "@v{KEY} as KEY",
        "max(@v{LNK_STS_CD}) as LNK_STS_CD"
      )
      group_by = c("@v{ALF}", "@v{NOTES}", "@v{LNK_STS_CD}")

    } else {
      stop(paste0("Argument `result_shape` does not support value '", result_shape, "'"))
    }
    args[["select"]] = select
    args[["group_by"]] = group_by

  }

  # Check for vars arguments
  if ("vars" %nin% names(args)){
    vars = list()
  }

  for (event_name in names(key_list)){
    args[["vars"]][["event_name"]] = event_name
    for (tab in names(key_list[[event_name]])){
      print(paste0("Processing event '", event_name, "' in table '", tab, "'"));
      args[[key_ind]] = key_list[[event_name]][[tab]]
      # Drop table
      sql.drop_table(tmp_tab);
      # Run table event function
      fcn = get(paste0("tab.", tab, ".events"));
      do.call(fcn, args)
      # Insert result into final table
      if (sql.run(prj, paste("select count(*) from", tmp_tab)) == 0){
        warning(paste0("No '", event_name, "' events found in table '", tab, "'."))
      } else {
        sql.insert_into_table(
          args$prj,
          output_table,
          paste("select * from", tmp_tab))
        # Commit
        sql.run("commit");
      }
    }
  }

}
