#' Check Concept Library key
#'
#' @param key Filter key. See `events.reference` for details.
#' @return A list with the key's "str", "type", "value" and "arg" values.
#'
#' @seealso events.reference
#'
#' @author Marcos del Pozo Banos
#'
key.init <- function(prj, key){

  # Identify the value argument and define key
  if (is.list(key)){

    if ("fcn" %in% names(key) & "name" %in% names(key) & "kwargs" %in% names(key)){
      # If the list contain variables "fcn", the input is an algorithm

      # ==========================================
      # Algorithm
      # ==========================================

      type = "alg"
      value = key$fcn
      name = key$name
      key_kwargs = key$kwargs

      # Build string fully defining the current key
      key_str = key_kwargs
      if (length(key_str)>0){
        for (k in 1:length(key_str)){  # k = 1
          # Dodgy way to identify a VELA.key. This needs to improve
          if (is.list(key_str[[k]]) && length(key_str[[k]])>2){
            key_str[[k]] = key_str[[k]]$str
          }
        }
      }
      key_str = paste0(lapply(deparse(key_str), trimws), collapse = ' ')
      str = paste0(name, "(", substr(key_str, 6, nchar(key_str)-1), ')')

      # Put everything together
      key = list(
        str = str,
        type = type,
        value = value,
        kwargs = key_kwargs
      );

      return(key)

    } else {
      # Otherwise, the list is assumed to be a list of codes

      # ==========================================
      # Code list
      # ==========================================

      # Put everything together
      key = list(
        str = paste0('list("', paste(key, collapse='", "'), '")'),
        type = "lst",
        value = key,
        kwargs = list()
      );

      return(key)

    }
  } else if (is.character(key) || is.function(key)) {
    value = key
    kwargs = list()
  } else {
    stop(paste0("Argument `key` must be of type `list` or `character`. Found `", class(key), "' instead."))
  }

  # Identify the type of key: concept, algorithm or csv file
  if (is.function(value)){
    type = "alg"
  } else if (value == "ALL") {
    type = "all"
  } else if (endsWith(value, ".csv")) {
    type = "csv"
  } else {
    type = "con"
  }

  # Collect arguments depending on key type
  if (type == "all"){
    # Nothing to do
    key_kwargs = list()
    str = value
  } else if (type == "con"){
    # A concept contains two parts separated by "/": ID and version
    value = strsplit(value, "/")[[1]];
    # A version may not be specified. In this case, find the latest version
    if (length(value) == 1){
      value[[2]] = utils.conlib.latest_version(prj, value[[1]])
    }
    key_kwargs = as.list(value)
    value = paste0(value, collapse="/")
    str = value
  } else if (type == "csv"){
    # Nothing to do
    key_kwargs = list()
    str = value
  }

  # Put everything together
  key = list(
    str = str,
    type = type,
    value = value,
    kwargs = key_kwargs
    );

  return(key)
}
