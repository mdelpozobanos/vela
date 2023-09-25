#' Prepare SQL information for `tab.TABLE.events` functions
#'
#' Uses default values (see `filter.reference`) or those specified in the input
#' argument `sql_info`.
#'
#' @param prj A project list.
#' @param sql_info A list with non-default sql_info queries.
#' @return A list with sql_info queries used when filtering tables
#'
#' @seealso filter.reference
#'
#' @author Marcos del Pozo Banos
#'
utils.tab.TABLE.events.get_sql_info <- function(prj,
                                                 select,
                                                 from,
                                                 where,
                                                 group_by
                                                 ){

  # The default information select all columns from the calling table (@v{tab}.*),
  # which is filtered by the selected code list (@v{cd_list}).
  sql_info = list(
    select = c("@v{tab}.*"),
    from = data.frame(
      full_name = "@v{TAB}",
      nickname = "@v{tab}",
      where = I(list(c())),
      join_type = "inner"),
    where = c(),
    group_by = c())

  # If the project defines an ALF table, full join the table to the calling
  # table to limit the scope of the function.
  if ("ALF" %in% row.names(prj$tables)){
    sql_info$from = rbind(sql_info$from,
                          data.frame(
                            full_name = prj$tables["ALF", "full_name"],
                            nickname = prj$tables["ALF", "nickname"],
                            where = I(list(c("@v{ALF} = @t{ALF}.ALF_@v{enc}"))),
                            join_type = "inner"
                            )
                          )
  }

  # Update the default information
  # 1. select
  if (length(select)>0){
    # This fully overwrites the existing value. Is this what we want?
    # sql_info$select = c(sql_info$select, select);
    sql_info$select = select;
  }
  # 2. from
  if (nrow(from)>0){
    a = b
  }
  # 3. where
  if (length(where)>0){
    sql_info$where = c(sql_info$where, where)
  }
  # 4. group by
  if (length(group_by)>0){
    sql_info$group_by = c(sql_info$group_by, group_by)
  }

  return(sql_info)

}
