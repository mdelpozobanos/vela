#' Construct and run and SQL query using `sql_info`
#'
utils.sql <- function(prj, output_table, sql_info, echo, add_if_exists=FALSE){

  # Complete SQL information
  # ========================

  # Resolve all variables `@*{}` to their full definitions
  output_table = utils.qq(output_table, prj)
  sql_info$select = unlist(lapply(sql_info$select, function(x) utils.qq(x, prj)))
  # [SEE BELOW] sql_info$from$full_name = unlist(lapply(sql_info$from$full_name, function(x) utils.qq(x, prj)))
  # [SEE BELOW] sql_info$from$nickname = unlist(lapply(sql_info$from$nickname, function(x) utils.qq(x, prj)))
  # [SEE BELOW] sql_info$from$where = unlist(lapply(sql_info$from$where, function(x) utils.qq(x, prj)))
  sql_info$where = unlist(lapply(sql_info$where, function(x) utils.qq(x, prj)))
  sql_info$group_by = unlist(lapply(sql_info$group_by, function(x) utils.qq(x, prj)))

  # Identify all table references in `sql_info`
  # ... `sql_info$select`
  sql_info$select = data.frame(col = sql_info$select, refers = NA)
  for (s in 1:nrow(sql_info$select)){  # s = 1
    select = gsub("['](.*?)[']", "", sql_info$select[[s, "col"]])
    refers = stringr::str_match_all(select, "[^| |=|+|-|(][a-zA-Z][a-zA-Z0-9_]*[\\.]")[[1]]
    # Clear references
    refers = lapply(refers, FUN = function(x) stringr::str_replace(x, "[ =+-\\.]", ""))
    if (length(refers)>0){
      sql_info$select[s, "refers"] = list(list(unique(unlist(refers))))
    }
  }
  # ... `sql_info.from`
  # [SEE BELOW]
  # ... `sql_info.where`
  if (length(sql_info$where) == 0){
    sql_info$where = data.frame()
  } else {
    sql_info$where = data.frame(col = sql_info$where, refers = NA)
    for (s in 1:nrow(sql_info$where)){  # s = 1
      where = gsub("['](.*?)[']", "", sql_info$where[[s, "col"]])
      refers = stringr::str_match_all(where, "[^| |=|+|-|(][a-zA-Z][a-zA-Z0-9_]*[\\.][a-zA-Z]")[[1]]
      # Clear references
      refers = lapply(refers, FUN = function(x) substr(x, 1, nchar(x)-2))
      if (length(refers)>0){
        sql_info$where[s, "refers"] = list(list(unique(unlist(refers))))
      }
    }
  }
  # ... `sql_info.group_by`
  if (length(sql_info$group_by) == 0){
    sql_info$group_by = data.frame()
  } else {
    sql_info$group_by = data.frame(col = sql_info$group_by, refers = NA)
    for (s in 1:nrow(sql_info$group_by)){  # s = 1
      group_by = gsub("['](.*?)[']", "", sql_info$group_by[[s, "col"]])
      refers = stringr::str_match_all(group_by, "[^| |=|+|-|(][a-zA-Z][a-zA-Z0-9_]*[\\.]")[[1]]
      # Clear references
      refers = lapply(refers, FUN = function(x) stringr::str_replace(x, "[ =+-\\.]", ""))
      if (length(refers)>0){
        sql_info$group_by[s, "refers"] = list(list(unique(unlist(refers))))
      }
    }
  }

  # ... from -> The above two steps are combined into the support function
  # `resolve_from_references`
  sql_info$from = resolve_from_references(prj, sql_info$from)

  # Combine referenced tables into a single list
  referenced_tables = unlist(sql_info$select$refers)
  referenced_tables = c(referenced_tables, unlist(sql_info$from$refers))
  referenced_tables = c(referenced_tables, unlist(sql_info$where$refers))
  referenced_tables = c(referenced_tables, unlist(sql_info$group_by$refers))
  referenced_tables = unique(referenced_tables)
  referenced_tables = referenced_tables[!is.na(referenced_tables)]

  # Add any missing tables to `sql_info`
  num_new_references = 0
  if (length(referenced_tables) != nrow(sql_info$from)){
    for (t in referenced_tables){  # t = referenced_tables[[2]]
      if (t %nin% sql_info$from$nickname){
        num_new_references = num_new_references + 1
        # Extract table variable / common name
        t_var = row.names(prj$tables)[prj$tables$nickname == t]
        # Generate the table row
        t_row = data.frame(
          full_name = prj$tables[t_var, "full_name"],
          nickname = t,
          where = utils.qq(paste0('@v{ALF} = @t{', t_var, '}.ALF_@v{enc}'), prj),
          join_type = 'inner',
          refers = NA
        )
        # Add table row to `sql_info$from`
        sql_info$from = rbind(sql_info$from, t_row)
      }
    }

    # Re-resolve `sql_info$from` references
    sql_info$from = resolve_from_references(prj, sql_info$from)

  }

  # Organise SQL information
  # ========================

  # Move all inner joins to the top
  mask = sql_info$from$join_type == "inner"
  sql_info$from = rbind(sql_info$from[mask, ], sql_info$from[!mask, ])

  # Move "where" clauses to `sql_info$from`. This will improve speed
  if (nrow(sql_info$where)>0){
    not_moved_mask = rep(FALSE, nrow(sql_info$where))  # Flag where clauses moved to "from-join" clauses
    for (w in 1:nrow(sql_info$where)){  # w = 1
      add_to_ind = 1
      for (refers_r in sql_info$where[[w, "refers"]]){  # refers_r = sql_info$where[[w, "refers"]][2]
        from_ind = which(sql_info$from$nickname == refers_r)
        # If the reference table is joined one sided, the where cannot be moved to the from-join clause
        if (sql_info$from[from_ind, "join_type"] != "inner"){
          not_moved_mask[w] = TRUE
        }
        add_to_ind = max(add_to_ind, which(sql_info$from$nickname == refers_r))
      }
      # Move the where clause if possible
      if (!not_moved_mask[w]){
        aux = c(sql_info$from[[add_to_ind, "where"]], sql_info$where[[w, "col"]])
        sql_info$from[[add_to_ind, "where"]] = aux[!is.na(aux)]
        aux = c(sql_info$from[[add_to_ind, "refers"]], sql_info$where[[w, "refers"]])
        sql_info$from[[add_to_ind, "refers"]] = aux[!is.na(aux)]
      }
    }
    # Remove moved clauses from `sql_info$where`
    sql_info$where = sql_info$where[not_moved_mask, ]
  }

  # Finally, the where clauses of the first table will be:
  # + Moved to the where clauses of the closest inner joined table
  # + If there is no inner joined table, moved to the where clause.
  inner_join_ind = which(sql_info$from$join_type == 'inner')
  if (length(inner_join_ind) > 1){
    # Move to the closest inner joined table
    # ... where
    sql_info$from[[inner_join_ind[2], "where"]] = c(
      sql_info$from[[inner_join_ind[2], "where"]],
      sql_info$from[[1, "where"]]
    )
    sql_info$from[[1, "where"]] = NA
    # ... refers
    sql_info$from[[inner_join_ind[2], "refers"]] = c(
      sql_info$from[[inner_join_ind[2], "refers"]],
      sql_info$from[[1, "refers"]]
    )
    sql_info$from[[1, "refers"]] = NA
  } else {
    # Move to the where clause
    # ... where
    sql_info$where = rbind(
      sql_info$where,
      data.frame(col = sql_info$from[[1, "where"]],
                 refers = sql_info$from[[1, "refers"]])
    )
    sql_info$from[[1, "where"]] = NA
    sql_info$from[[1, "refers"]] = NA
  }

  # Build SQL query
  # ===============

  # select
  select_query = paste("select\n\t", paste0(sql_info$select$col, collapse = ',\n\t'))

  # from
  from_query = paste("from", sql_info$from[[1, 'full_name']], sql_info$from[[1, 'nickname']])

  # join
  num_tables = nrow(sql_info$from)
  if (num_tables > 1){
    for (t in 2:nrow(sql_info$from)){  # t = 2
      join_query = paste(sql_info$from[[t, 'join_type']], "join", sql_info$from[[t, 'full_name']], sql_info$from[[t, 'nickname']])
      join_query = paste0(join_query, "\non\n\t(",
                          paste0(sql_info$from[[t, "where"]], collapse = ')\n\tand ('),
                          ")")
      from_query = paste(from_query, join_query, sep='\n')
    }
    # cat(from_query)
  }

  # where
  if (nrow(sql_info$where)>0){
    where_query = paste0("where\n\t(",
                         paste0(sql_info$where$col, collapse = ')\n\tand ('),
                         ")")
  } else {
    where_query = ""
  }
  # cat(where_query)

  # group by
  if (nrow(sql_info$group_by)>0){
    group_by_query = paste0("group by\n\t(",
                            paste0(sql_info$group_by$col, collapse = '),\n\t('),
                            ")")
  } else {
    group_by_query = ""
  }
  #cat(group_by_query)

  # Build SQL query
  sql_query = paste(select_query, from_query, where_query, group_by_query, sep="\n")
  # cat(sql_query)

  # Run query
  if (add_if_exists){
    # Insert filtering result into final table
    sql.insert_into_table(prj, output_table=output_table, select_query=sql_query, echo = echo);
  } else {
    # Create new table. It will raise an error if the table already exist
    SAILDBUtils::create_table_as(prj$conx, output_table=output_table, select_query=sql_query, echo = echo);
  }

}


resolve_from_references <- function(prj, sql_from){

  # Resolve all variables `@*{}` to their full definitions
  sql_from$full_name = unlist(lapply(sql_from$full_name, function(x) utils.qq(x, prj)))
  sql_from$nickname = unlist(lapply(sql_from$nickname, function(x) utils.qq(x, prj)))
  sql_from$where = lapply(sql_from$where, function(x) unlist(lapply(x, function(x) utils.qq(x, prj))))

  # Identify all table references
  sql_from$refers = list(NA)
  for (f in 1:nrow(sql_from)){  # f = 1
    where_f = sql_from$where[[f]]
    if (!is.null(where_f)){
      sql_from$refers[[f]] = I(list(NA))
      for (w in 1:length(where_f)){  # w = 1
        where_fw = where_f[[w]]
        # Find references to tables
        where_fw = gsub("['](.*?)[']", "", where_fw)
        refers = stringr::str_match_all(where_fw, "[^| |=|+|-|(][a-zA-Z][a-zA-Z0-9_]*[\\.]")[[1]]
        # Clear references
        refers = lapply(refers, FUN = function(x) stringr::str_replace(x, "[ =+-\\.]", ""))
        sql_from$refers[[f]][[w]] = I(unique(unlist(refers)))
      }
    }
  }

  return(sql_from)
}
