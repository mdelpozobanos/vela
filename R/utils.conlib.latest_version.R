#' Return latest version of the specified concept
#'
#' @param prj A project list (see `prj.set_up`)
#' @param concept_id A string with the concept ID.
#'
#' @seealso
#'
#' @author Marcos del Pozo Banos
#' @export
#'
utils.conlib.latest_version <- function(prj, concept_id){
  concept = ConceptLibraryClient::get_concept_detail(concept_id, prj$ConceptLibrary$conx);
  concept_vers = concept$version_id

  concept_vers = tryCatch({
    ConceptLibraryClient::get_concept_code_list_by_version(
      concept_id,
      concept_vers,
      prj$ConceptLibrary$conx)
    return(concept_vers)
  },
  error = function(msg){
    if (grepl("Forbidden Request", msg)){
      concept_versions = ConceptLibraryClient::get_concept_versions(concept_id, prj$ConceptLibrary$conx)
      concept_vers = concept_versions[concept_versions$is_published == TRUE, ][1, "version_id"]
      ConceptLibraryClient::get_concept_code_list_by_version(
        concept_id,
        concept_vers,
        prj$ConceptLibrary$conx)
      return(concept_versions[concept_versions$is_published == TRUE, ][1, "version_id"])
    } else {
      stop(msg)
    }
  })



  return(concept$version_id)
}
