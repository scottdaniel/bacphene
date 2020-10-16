#' Gets the strains from bacdive that belong to a supplied species
#'
#' @param genus
#' @param species
#' @param userpassword
#'
#' @return
#' @export
#'
#' @examples
getStrains <- function(genus, species, userpassword) {

  url_species <- URLencode(paste0(taxon,searchterm_genus,'/',searchterm_species,'/?page=',page,'&format=json'))

  response <- getURL(url_species,userpwd=userpassword, httpauth = 1L)
  jsondata <- fromJSON(response)

}

#' Gets the phenotype data for a specific strain
#'
#' @param jsondata
#' @param selection
#' @param userpassword
#'
#' @return
#' @export
#'
#' @examples
getStrainData <- function(jsondata, selection=1, userpassword) {

  response <- getURL(paste0(jsondata[["results"]][[selection]][["url"]],'/?format=json'), userpwd=userpassword, httpauth = 1L)

  jsondata <- fromJSON(response)

}
