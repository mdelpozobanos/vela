#' Process input arguments for a `tab.TABLE.events` function
#'
#' @param prj A project variable
#' @param key A character specifying the selected events (see `events.key for details`).
#' @param output_table A character specifying the name of the table to be
#'   created with the result of `tab.TABLE.events`.
#' @param sql_queries A list with SQL queries to be used in the final query.
#'   (see `filter.reference` for details).
#' @param ... Any additional parameters overwriting values in `prj`.
#' @return NA. The result is saved in the database.
#'
#' @seealso filter.reference, prj.set_up, filter.alg_list
#'
#' @author Marcos del Pozo Banos
#'
utils.tab.TABLE.events_args <- function(prj,
                                        output_table,
                                        key,
                                        table_vars,
                                        vars = list(),
                                        sql_info = list(),
                                        echo = FALSE,
                                        select = c(),
                                        from = data.frame(),
                                        where = c(),
                                        group_by = c(),
                                        only_extract_ = FALSE
){

  if (only_extract_){
    return(list(prj=prj,
                output_table=output_table,
                key=key,
                table_vars=table_vars,
                vars = vars,
                sql_info = sql_info,
                echo = echo,
                select = select,
                from = from,
                where = where,
                group_by = group_by
                ))
  }

  # Create a list version of project variables, and update it with table variables
  prj$vars_list = as.list(prj$vars$definition)
  prj$vars_list = setNames(prj$vars_list, row.names(prj$vars))
  prj$vars_list = utils.list_update(prj$vars_list, table_vars)

  # Attach user specified variables to `prj`
  prj$user_vars_list = vars

  # Process key and save character version to `vars_list`
  key = key.init(prj, key)
  # Save key character to the variables
  prj$vars_list$KEY = key$str

  # Build SQL data.frame with information about the query. Additional parameters
  # pertain to the SQL
  sql_info = utils.tab.TABLE.events.get_sql_info(prj, select, from, where, group_by);

  return(list(
    prj = prj,
    output_table = output_table,
    key = key,
    sql_info = sql_info,
    echo = echo
  ))
}
