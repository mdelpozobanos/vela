#' String replacement with GetoptLong::qq
#'
#' Delimiters:
#' "@T{}": table full names from `prj$tables`
#' "@t{}": table nicknames from `prj$tables`
#' "@v{}": variables from `prj$vars` tailored for the calling table
#' "@u{}": additional user specified variables in `prj$user_vars`
#'
#' @author Marcos del Pozo Banos
#'
utils.qq = function(x, prj, ...){
  if (is.na(x)){
    return(NA)
  }

  # Normalize `varchar` variables. If they start with single quote, we consider
  # the string is already in SQL form and it won't be wrapped in single quotes.
  if (!is.null(prj$vars_list)){
    if (startsWith(prj$vars_list$KEY, "'")){
      prj$vars_list$KEY = paste0("cast(", prj$vars_list$KEY, " as varchar(1000))")
    } else {
      prj$vars_list$KEY = paste0("cast('", prj$vars_list$KEY, "' as varchar(1000))")
    }
    if (startsWith(prj$vars_list$src, "'")){
      prj$vars_list$src = paste0("cast(", prj$vars_list$src, " as varchar(1000))")
    } else {
      prj$vars_list$src = paste0("cast('", prj$vars_list$src, "' as varchar(1000))")
    }
    if (startsWith(prj$vars_list$NOTES, "'")){
      prj$vars_list$NOTES = paste0("cast(", prj$vars_list$NOTES, " as varchar(5000))")
    } else {
      prj$vars_list$NOTES = paste0("cast('", prj$vars_list$NOTES, "' as varchar(5000))")
    }
  }

  # Prepare environments
  envirs = list(
    T = new.env(parent = emptyenv()),
    t = new.env(parent = emptyenv()),
    v = new.env(parent = emptyenv())
    )
  list2env(setNames(as.list(prj$tables$full_name), row.names(prj$tables)),
           envirs$T)
  list2env(setNames(as.list(prj$tables$nickname), row.names(prj$tables)),
           envirs$t)
  if ("vars_list" %in% names(prj)){
    list2env(prj$vars_list, envirs$v)
  } else {
    list2env(setNames(as.list(prj$vars$definition), row.names(prj$vars)),
             envirs$v)
  }
  if ("user_vars_list" %in% names(prj)){
    # envirs$u = new.env(parent = emptyenv())
    # These environments cannot start empty, as they will potentially need
    # access to certain functions (e.g. $ and [])
    envirs$u = new.env()
    list2env(prj$user_vars_list, envirs$u)
  }
  # Additional variables...
  kwargs = list(...)
  for (n in names(kwargs)){
    # envirs[[n]] = new.env(parent = emptyenv())
    # These environments cannot start empty, as they will potentially need
    # access to certain functions (e.g. $ and [])
    envirs[[n]] = new.env()
    list2env(kwargs[[n]], envirs[[n]])
  }

  # Interpolate while there are variables to interpolate
  pattern = paste0("@(", paste(names(envirs), collapse = "|"), ")\\{.*\\}")
  while (grepl(pattern, x)){
    x = aux_.qq_(x, envirs)
  }
  return(x)
}
#' Supporting function
aux_.qq_ <- function(x, envirs){
  for (envir in names(envirs)){
    code.pattern = paste0("@", envir, "\\{CODE\\}")
    x = GetoptLong::qq(x, code.pattern = code.pattern, envir = envirs[[envir]])
  }
  return(x)
}
