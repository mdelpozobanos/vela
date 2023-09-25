#' Template for VELA algorithm
#'
#' This is a template for a VELA algorithm. Unless you know what you are doing,
#' you should introduce your code between the "[COMPLETE START]" and
#' "[COMPLETE END]" marks. You will find further instructions in each of these
#' COMPLETE blocks.
#'
#' This function can be run in several modes:
#'
#' KEY mode: `@{alg_name}(prj, kwargs)`
#'   Check that they input keyword arguments meet the requirements of the
#'   algorithm and return the name of the algorithm and the completed list
#'   of keyword arguments (including default values where necessary).
#'
#' APPLY mode: `@{alg_name}(prj, output_table, sql_info, max_code_depth, echo, ...)`
#'   Apply the algorithm with the specified parameters. All arguments must be
#'   passed as keyword arguments (not positional arguments).
#'
#' @export
#'
alg.TABLE.key_where <- function(...){

  # Extract `prj` from arguments
  kwargs = list(...);  # All arguments need to be passed as key arguments
  prj = kwargs$prj
  kwargs$prj = NULL

  # Working modes
  # =============

  # 1. KEY mode
  # -----------
  # When the argument "kwargs" is passed. No additional arguments expected

  if ("kwargs" %in% names(kwargs)){
    kwargs = utils.check_kwargs(kwargs, required = c("kwargs"));
    keys_where = kwargs$kwargs

    # Check all key_where arguments and transform to complete form if necessary:
    # i.e. lists of the form `(key, where)`
    for (k in 1:length(keys_where)){
      arg_k = keys_where[[k]]
      if (is.character(arg_k) && length(arg_k) > 1){
        if (length(arg_k) > 2){  # Too many arguments
          stop(paste("Invalid key_where argument:", arg_k))
        }
        arg_k[[1]] = key.init(prj, arg_k[[1]])$str
      } else if (is.character(arg_k)){
        # key without a where clause. Normalize to vector
        aux = key.init(prj, arg_k)
        arg_k = c(aux$str, NA)
      }
      keys_where[[k]] = arg_k
    }

    return(list(name = "@{alg_name}", kwargs = keys_where));
  }


  # 2. APPLY mode
  # -------------

  # Retrieve common input arguments
  output_table = kwargs$output_table
  kwargs$output_table = NULL
  sql_info = kwargs$sql_info
  kwargs$sql_info = NULL
  max_code_depth = kwargs$max_code_depth
  kwargs$max_code_depth = NULL
  echo = kwargs$echo
  kwargs$echo = NULL
  keys_where = kwargs

  # Build a where clause combining all the code filters and where clauses
  code_filter_where = c()
  for (k in (1:length(keys_where))){  # k = 1
    key_str = utils.key2str(prj, keys_where[[k]][[1]], max_code_depth)
    prj$vars_list$code_list = key_str
    if (is.na(keys_where[[k]][[2]])){
      code_filter_where_k = "@v{CODE_FILTER}"
    } else {
      code_filter_where_k = paste0("(@v{CODE_FILTER})\n\t\tand (", keys_where[[k]][[2]], ")")
    }
    code_filter_where_k = utils.qq(code_filter_where_k, prj)
    code_filter_where = c(code_filter_where, code_filter_where_k)
  }
  code_filter_where = paste0("\n\t\t(\n\t\t\t", paste0(code_filter_where, collapse = "\n\t\t)\n\t\tor (\n\t\t\t"), ")\n")
  sql_info$where = c(sql_info$where, code_filter_where)

  # Run query
  utils.sql(prj, output_table, sql_info, echo)

}
