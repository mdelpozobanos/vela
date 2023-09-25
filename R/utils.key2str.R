#' Convert an event key into a string of code list
#'
#' @param key Filter key. See `filter.reference` for details.
#' @param max_code_depth Integer with the maximum code depth included. Codes
#'   deeper than `max_code_depth` will be excluded/ignored.
#' @return A character with a comma separated list of codes
#'
#' @seealso filter.reference
#'
#' @author Marcos del Pozo Banos
#' @export
#'
utils.key2str <- function(prj, key, max_code_depth){
  if (!is.list(key)){
    key = key.init(prj, key)
  }
  if (key$type == 'con'){  # Concept Library
    concept = ConceptLibraryClient::get_concept_code_list_by_version(
      key$kwargs[[1]], key$kwargs[[2]], prj$ConceptLibrary$conx
    )
  } else if (key$type == 'csv') {  # CSV file
    concept = utils.get_concept_from_file(key$value);
  } else if (key$type == 'lst') {  # Code list provided
    concept = data.frame(code=unlist(key$value), description="")
  } else {
    stop(GetoptLong::qq("key type '@{key$type}' not supported"))
  }
  # Check the concept is not empty
  if (nrow(concept) == 0){
    stop(paste0("Key '", paste0(key_value, collapse = '/'), "' returned an empty list of codes." ))
  }
  # Remove full stops and white spaces
  concept$code = gsub(".", "", concept$code, fixed = TRUE)
  concept$code = gsub(" ", "", concept$code, fixed = TRUE)
  # Remove codes with too high depth
  code_list = concept[nchar(concept$code) <= max_code_depth , ]$code
  # Truncate codes to `code_length` characters
  code_list = unique(lapply(code_list, substr, 1, max_code_depth))
  # Extend codes to `code_length` characters
  code_list = lapply(code_list, stringr::str_pad, max_code_depth, "right", ".")
  code_list_str = paste(
    "'", paste(code_list, collapse = "', '"), "'", sep = ""
  );
  return(code_list_str)
}
