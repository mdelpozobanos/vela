#' Initialized a script with an algorithm template.
#'
#' @param alg_name Name of the algorithm (function name)
#'
#' @author Marcos del Pozo Banos
#' @export
#'
alg.init <- function(alg_name){
  alg_file = paste0(alg_name, ".R")
  # Make a copy of the file
  file.copy(from = paste0(system.file(package="vela"), "/alg.template.R"),
            to = alg_file)
  # Set function name
  txt = readLines(alg_file)
  txt = gsub(pattern = "@{alg_name}", replacement = alg_name, x = txt, fixed = TRUE)
  writeLines(txt, con = alg_file)
}
