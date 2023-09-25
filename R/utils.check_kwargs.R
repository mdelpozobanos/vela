#' Check list of keyword arguments against required and optional arguments and
#' return a complete list.
#'
#' @param kwargs List of arguments to check
#' @param required Vector of arguments required to be defined in `kwargs`
#' @param optional List <name> = <default_value> of optional arguments in
#'   `kwargs`. If an argument is not found in `kwargs`, it is added with the
#'   specified default value.
#'
#' @author Marcos del Pozo Banos
#' @export
#'
utils.check_kwargs = function(kwargs,
                              required = c(),
                              optional = list()){

  kwargs_names = names(kwargs)
  required_names = required
  optional_names = names(optional);
  supported_names = c(required_names, optional_names)

  # Make sure required arguments are specified
  if (length(required_names)>0){
    for (i in 1:length(required_names)){  # i = 2
      if (required_names[i] %nin% kwargs_names){
        # Try by position
        if (length(kwargs_names) >= i && nchar(kwargs_names[i]) == 0){
          kwargs_names[i] = required_names[i]
        } else {
          stop(paste0("Unspecified required argument ", required_names[i]));
        }
      }
    }
    names(kwargs) <- kwargs_names
  }

  # When an optional argument is not specified, set with default value
  if (length(optional_names) > 0){
    for (i in 1:length(optional_names)){
      if (optional_names[i] %nin% kwargs_names){
        # Try by position
        if ((length(kwargs_names) >= i) && (nchar(kwargs_names[i]) == 0)){
          kwargs_names[i] = optional_names[i]
          names(kwargs) <- kwargs_names
        } else {
          kwargs[[optional_names[i]]] = optional[[optional_names[i]]];
          kwargs_names = names(kwargs)
        }
      }
    }
  }

  # Make sure all specified arguments are supported
  for (kwarg_i in kwargs_names){
    if (kwarg_i %nin% supported_names){
      stop(paste0("Unused argument ", kwarg_i));
    }
  }

  return(kwargs)
}
