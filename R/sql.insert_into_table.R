#' "create table as" or, if the table exists, "insert into" table.
#'
#' @param db_conn SAIL DB connection.
#' @param output_table Name of table to insert.
#' @param select_query SQL select query to retrieve data to insert.
#' @param echo Boolean
#' @return A data.frame with the specified code list
#'
#' @author Marcos del Pozo Banos
#' @export
#'
sql.insert_into_table <- function(...) {  # (prj, output_table, select_query, echo)
  args = utils.lazy_prj(list(...))
  kwargs = utils.check_kwargs(args,
                              required = c("prj", "output_table", "select_query"),
                              optional = list(echo = FALSE));
  prj = kwargs$prj
  db_conn = prj$conx
  output_table = kwargs$output_table
  select_query = kwargs$select_query
  echo = kwargs$echo

  # Interpolate SQL code
  output_table = utils.qq(output_table, prj)
  select_query = utils.qq(select_query, prj)

  if (SAILDBUtils::is_session(output_table)){

    # If this is a session table, we need to try to insert and catch the error if the table does not exist
    result = SAILDBUtils::runSQL(db_conn,
                                 GetoptLong::qq("insert into @{output_table} @{select_query}"),
                                 echo = echo,
                                 stop_on_error = FALSE);

    # Check result is not holding an error
    if (length(result) == 0){
      # Run well. Do nothing
    } else if (is.character(result) &
               (length(result) == 2) &
               grepl(GetoptLong::qq('"@{output_table}" is an undefined name'), result[1], fixed = TRUE) &
               grepl("ERROR", result[2], fixed = TRUE)){

      # Table does not exist
      SAILDBUtils::create_table_as(db_conn, output_table, select_query, echo = echo);

    } else if (is.character(result) &
               (length(result) == 2) &
               grepl(GetoptLong::qq('No row was found for FETCH, UPDATE or DELETE'), result[1], fixed = TRUE) &
               grepl("ERROR: Could not SQLExecDirect", result[2], fixed = TRUE)){

      warning(result)

    } else {

      stop(result);

    }

  } else {

    if (SAILDBUtils::table_exists(db_conn, output_table)){

      # Table exists, just insert data
      SAILDBUtils::runSQL(db_conn,
                          GetoptLong::qq("insert into @{output_table} @{select_query}"),
                          echo = echo);

    } else {

      # Table does not exist, create table as.
      SAILDBUtils::create_table_as(db_conn, output_table, select_query, echo = echo);

    }

  }

  return(TRUE);
}
