#' Gets the strains from bacdive that belong to a supplied species
#'
#' @param page You get entries only from the first page (page = '1'), to consider the other pages you have to change the parameter page
#' @param genus Genus of the species
#' @param species The species
#' @param userpassword The userpassword in the form of "user:password" that you got when you registered at bacdive.org
#'
#' @return A list of strains from bacdive.org
#' @export
#'
#' @importFrom RCurl getURL
#' @importFrom rjson fromJSON
#' @importFrom utils URLencode
#'
#' @examples
#' \dontrun{
#' getStrains(page = 1,
#' genus = 'Bacteroides',
#' species = 'xylanisolvens',
#' userpassword = paste0(user,':',passwd)))
#' }
getStrains <- function(page, genus, species, userpassword) {

  api_entry <- 'https://bacdive.dsmz.de/api/bacdive/taxon/'
  url_species <- URLencode(paste0(api_entry, genus, '/', species, '/?page=', page, '&format=json'))

  response <- getURL(url_species,userpwd=userpassword, httpauth = 1L)
  jsondata <- fromJSON(response)

}

#' Gets the phenotype data for a specific strain
#'
#' @param jsondata The list object that you got from \code{\link{getStrains}}
#' @param selection Which strain you want from the list object
#' @param userpassword The userpassword in the form of "user:password" that you got when you registered at bacdive.org
#'
#' @return A list of phenotype data from bacdive.org
#' @export
#'
#' @importFrom RCurl getURL
#' @importFrom rjson fromJSON
#' @importFrom utils URLencode
#'
#' @examples
#' \dontrun{
#' strain_list <- getStrains(page = 1,
#' genus = 'Bacteroides',
#' species = 'xylanisolvens',
#' userpassword = paste0(user,':',passwd)))
#' getStrainData(strain_list,
#' selection=1,
#' userpassword = paste0(user,':',passwd))
#' }
getStrainData <- function(jsondata, selection=1, userpassword) {

  response <- getURL(paste0(jsondata[["results"]][[selection]][["url"]],'/?format=json'), userpwd=userpassword, httpauth = 1L)

  jsondata <- fromJSON(response)

}
