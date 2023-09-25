#' Retrieve a list of codes from a CSV file
#'
#' @param file_name A character with the file name. If not specified, a file
#'   selection window will pop up. The CSV file must have columns 'code',
#'   'include' (with value 0 to exclude/ignore the code and 1 to include it) and
#'   'description'.
#' @return A data.frame with the specified code list
#'
#' @author Marcos del Pozo Banos
#'
utils.get_concept_from_file <- function(file_name=NA) {

  # GUI file selection
  if (is.na(file_name)){
    file_name <- utils::choose.files(default='',
                                     caption = 'Select code list file',
                                     filters = cbind('Comma separated csv (*.csv)', "*.csv"),
                                     multi = FALSE)
    if (length(file_name) == 0){
      stop('Code list file selection aborted')
    }
  }

  # Read file
  concept = utils::read.csv(file_name)
  if ("include" %in% colnames(concept)){
    concept = concept[concept$include == 1, ]
  }
  if ("code" %nin% colnames(concept)){
    stop("Column 'code' not in CSV file '", file_name, "'");
  }
  if ("description" %nin% colnames(concept)){
    stop("Column 'code' not in CSV file '", file_name, "'");
  }
  concept = concept[, c("code", "description")]

  # Remove codes with too high depth
  # concept = concept[grep(GetoptLong::qq("^[a-zA-Z0-9]{1,@{max_code_depth}}[\\.]*$"), concept$code), ]
  # concept$code = lapply(concept$code, substr, 1, max_code_depth)

  return(concept)
}
