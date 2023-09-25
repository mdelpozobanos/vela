#' Common filtering process across filter functions
#'
utils.tab.TABLE.events <- function(
    prj,
    output_table,
    key,
    sql_info,
    max_code_depth,
    echo = FALSE
){

  # Check type of key
  if (key$type == "alg"){

    # Call algorithm
    kwargs = c(list(prj = prj,
                    vela__key_mode = FALSE,
                    output_table = output_table,
                    sql_info = sql_info,
                    max_code_depth = max_code_depth,
                    echo = echo),
               key$kwargs)
    do.call(key$value, kwargs)

  } else if (key$type == "all"){

    # Construct and run SQL query
    utils.sql(prj, output_table, sql_info, echo)

  } else {

    # If max_code_depth is 0, the calling table cannot be filtered using a code list
    if (max_code_depth == 0){
      return(FALSE)
    }

    # Add the code filtering to `sql_info`
    sql_info$from[[1, 'where']] = c(sql_info$from[[1, 'where']], c("@v{CODE_FILTER}"))

    # Retrieve code list string and define variable `code_list`
    prj$vars_list$code_list = utils.key2str(prj, key, max_code_depth);

    # Construct and run SQL query
    utils.sql(prj, output_table, sql_info, echo)
  }

}
