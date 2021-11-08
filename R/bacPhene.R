#' Searching for strains with taxon specification
#'
#' @param credentials Your login credentials for bacdive.org. Defaults to DSMZ_API_USER and DSMZ_API_PASSWORD in your Renviron file. Supplied as a character vector c("username", "password").
#' @param sleep A waiting period in seconds between successive API requests, if any.
#'
#' @return A list of strains with phenotype information from bacdive.org
#' @export
#'
#' @importFrom BacDive open_bacdive retrieve
#'
#' @examples
#' \dontrun{
#' strain_list <- getStrains(query = "Bacteroides xylanisolvens",
#' sleep = 0.1)
#' }
getStrains <- function(credentials = Sys.getenv(c("DSMZ_API_USER", "DSMZ_API_PASSWORD")), query, sleep = 0.1, handler = NULL, getList = BacDive::retrieve) {

  bacdive <- BacDive::open_bacdive(credentials[[1L]], credentials[[2L]])
  getList(object = bacdive, query = query, search = "taxon", sleep = sleep, handler = handler)

}

#' Gets a single type strain of a species
#'
#' @param rda An RData file representing a full download of type strain from BacDive.org's API. Can be created using download_from_bacdive.R
#' @param query A species
#'
#' @return A list containing information on the type strain of a species
#' @export
#'
#' @importFrom rlist list.filter
#'
#' @examples
#' \dontrun{
#' abyss <- getTypeStrainLocal(query = "Abyssibacter profundi")
#' }
getTypeStrainLocal <- function(rda = "data-raw/type_strain_large_list.rda", query) {

  my_list <- readRDS(rda)

  rlist::list.filter(my_list, `Name and taxonomic classification`$species %in% query)

}

