#' Format a table with "counts" into "counts; % (95% CI)".
#'
#' Confidence intervals are computed using the Wilson score with continuity
#' correction.
#'
#' @param tab A data.frame with counts. This must have a column with the
#   description of each row, and a row with the description "Total" containing
#   the denominator user for the computation of proportions.
#' @param description_column Name of the column with the description field.
#'  By default "DESCRIPTION"
#' @return A list with the key's "str", "type", "value" and "arg" values.
#'
#' @seealso events.reference
#'
#' @author Marcos del Pozo Banos
#'
format_count_table <- function(tab, description_column="DESCRIPTION"){

  # Allocate result table
  res_tab = data.frame(DESCRIPTION = tab[, description_column])
  colnames(res_tab) = description_column

  # Find totals
  total_row = tab[tab[, description_column] == 'Total', ]

  # Compute values for each column
  for (col in colnames(tab)){  # col = colnames(tab)[2]
    if (col == description_column){
      next
    }
    CI = binom::binom.confint(tab[, col], total_row[, col], methods='wilson')
    CI$mean = sprintf("%.1f", round(CI$mean*100, 1))
    CI$lower = sprintf("%.1f", round(CI$lower*100, 1))
    CI$upper = sprintf("%.1f", round(CI$upper*100, 1))
    res_tab[, col] =apply(CI, 1, function(r){paste0(r["x"], "; ", r["mean"], '% (', r["lower"], ", ", r["upper"], ")")})
  }

  return(res_tab)
}
