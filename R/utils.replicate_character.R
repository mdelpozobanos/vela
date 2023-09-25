#' Repeat a string with a reference to numbered variables.
#'
#' This is useful when building SQL queries checking the value of multiple
#' numbered columns (e.g. DIAG_CD_1, DIAG_CD_2, DIAG_CD_3, etc).
#'
#' @param qq_str A string mentioning "@{n}" (reference to the index)
#' @param start_n Starting index
#' @param stop_n Ending index
#' @param joint A string specifying how to join each iteration of the qq_str
#'   (default = "or")
#' @return Resulting string
#'
#' @author Marcos del Pozo Banos
#' @export
#'
utils.replicate_character <- function(qq_str, start_n, stop_n, joint="or"){
  n = start_n
  sql_query = GetoptLong::qq(qq_str)
  for (n in (start_n + 1):stop_n){
    sql_query = paste(sql_query, joint, GetoptLong::qq(qq_str))
  }
  return(sql_query)
}
