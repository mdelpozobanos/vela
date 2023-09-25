#' Update a project object
#'
#' @param prj A project variable
#' @param action The type of update. Possible values are:
#'   + 'table': Add/update entry into/from `prj$tables`
#'   + 'var': Add/update entry into/from `prj$vars`
#'
#' When `action == 'table'`
#' ------------------------
#'
#' @param var_name [if `action == 'table'`] Common/variable name of the added table (row.name)
#' @param full_name [if `action == 'table'`] Full name of the added table
#' @param nickname [if `action == 'table'`] Nickname of the added table
#'
#' @seealso filter.reference, prj.set_up, filter.alg_list
#'
#' Special/internal calls
#' ----------------------
#'
#' This function is also used internally by VELA, using special values for
#' `action`.
#'
#' @param vars_list [if `action == 'vars_list'`] Updates `prj.vars_list` with
#' the specified value.
#'
#'
#' @author Marcos del Pozo Banos
#' @export
#'
prj.update <- function(prj, action, ...){
  kwargs = list(...)

  if (action == "table"){

    for (kw in c("full_name", "nickname")){
      if (kw %in% names(kwargs)){
        prj$tables[kwargs$name, kw] = kwargs[[kw]]
      }
    }

  } else if (action == "var"){

    for (kw in c("description", "definition")){
      if (kw %in% names(kwargs)){
        prj$vars[kwargs$name, kw] = kwargs[[kw]]
      }
    }

  }

  return(prj)
}

