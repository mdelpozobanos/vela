#' Update/add elements of a list elements from a second list.
#'
#' @param l1: List to be updated
#' @param l2: List used to update
#' @param insert_new: If TRUE, items in l2 not in l1 will be inserted. If
#'  FALSE, l2 cannot have items not in l1
#' @return An updates list.
#'
#' @example
#' list1 = list(A = 1, B = 2);
#' list2 = list(B = 20, C = 30);
#' list12 = list_update(list1, list2);
#' print(list12);
#' list21 = list_update(list1, list2);
#' print(list21);
#'
#' @author Marcos del Pozo Banos
#'
utils.list_update <- function(l1, l2, insert_new=TRUE){
  if (insert_new){
    for (e in names(l2)){
      l1[[e]] = l2[[e]]
    }
  } else {
    for (e in names(l2)){
      if (e %in% names(l1)){
        l1[[e]] = l2[[e]]
      } else {
        stop(paste0("Base list does not have item '", e, "'"));
      }
    }
  }
  return(l1)
}
