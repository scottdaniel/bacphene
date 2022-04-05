# A set of functions for accessing strains from bacdive.org

#' Opens a portal to the world of BacDive
#'
#' @param credentials Your login credentials for bacdive.org. Defaults to DSMZ_API_USER and DSMZ_API_PASSWORD in your Renviron file. Supplied as a character vector c("username", "password").
#'
#' @return A 'dsmz_keycloak' object that allows access to the BacDive API. See also \code{\link[BacDive]{open_bacdive}}.
#' @export
#' @importFrom BacDive open_bacdive
#'
#' @examples
#' \dontrun{
#' bacdive <- getBacDiveAccess()
#' }
getBacDiveAccess <- function(credentials = Sys.getenv(c("DSMZ_API_USER", "DSMZ_API_PASSWORD"))) {

  bacdive <- BacDive::open_bacdive(credentials[[1L]], credentials[[2L]])

}

#' Gets a single strain of a species
#'
#' @param list A list object containing strain information already present in the R environment. Required if an rda file is not given.
#' @param rda An RData file representing a full download of strain from BacDive.org's API. Can be created using download_from_bacdive.Rmd. Required if list is not given.
#' @param query Either a bacdive ID (integer) or species name (character).
#' @param typestrain_only Whether to filter down to typestrains only.
#'
#' @return A list containing information on the strain of a species.
#' @export
#' @importFrom rlist list.filter
#'
#' @examples
#' \dontrun{
#' abyss <- getStrainLocal(query = "Abyssibacter profundi")
#' }
#' @details If you give typestrain_only = T and your BacDive-ID query is not a typestrain you will get zero results.
getStrainLocal <- function(list = NULL, rda = "data-raw/strain_large_list.rda", query, typestrain_only = F) {

  if (is.null(list)) {
    list <- readRDS(rda)
  }

  if (typestrain_only) {
    list <- rlist::list.filter(list, `Name and taxonomic classification`$`type strain` %in% "yes")
  }

  if (is.character(query)) {
    rlist::list.filter(list, `Name and taxonomic classification`$species %in% query)
  } else {
    rlist::list.filter(list, General$`BacDive-ID` == query)
  }
}

#' Downloads BacDive strains from a list of supplied ID's
#'
#' @param strain_list A csv file containing at least two columns: "ID" - The BacDive ID and "is_type_strain_header". Can be gotten using \href{https://bacdive.dsmz.de/advsearch}{BacDive's advanced search}.
#' @param typestrain_only Whether to filter down to typestrains only.
#' @param bacdive_keycloak A 'dsmz_keycloak' object that allows access to the BacDive API. Created using \code{\link{getBacDiveAccess}} or \code{\link[BacDive]{open_bacdive}}.
#' @param save_rda If this is being run for the first time, the list of strains will be saved to the specificed RData file. Otherwise, the function will load strains from the specified data file.
#'
#' @return A list object of strain information. If running for the first time, saves an RData file of the list.
#' @export
#' @importFrom here here
#' @importFrom readr read_rds read_csv write_rds
#' @importFrom magrittr %<>%
#' @importFrom BacDive fetch
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' large_list <- getStrains(strain_list = "my_csv_file.csv", typestrain_only = F)
#' }
getStrains <- function(strain_list = "data-raw/full_list_of_bacteria_from_bacdive_20211027.csv",
                       typestrain_only = T,
                       bacdive_keycloak = getBacDiveAccess(),
                       rda = "data-raw/strain_large_list.rda") {

  # This can take a few minutes to download all the type strain data from bacdive.org
  if (file.exists(here::here(rda))) {
    list_holder <- readr::read_rds(here::here(rda))
  } else {

    my_list <-
      readr::read_csv(
        here::here(strain_list),
        skip = 2,
        show_col_types = FALSE
      )
    # full_list_of_bacteria_from_bacdive_20211027.csv was downloaded here: https://bacdive.dsmz.de/advsearch?filter-group%5B1%5D%5Bgroup-condition%5D=OR&filter-group%5B1%5D%5Bfilters%5D%5B1%5D%5Bfield%5D=Domain&filter-group%5B1%5D%5Bfilters%5D%5B1%5D%5Bfield-option%5D=contains&filter-group%5B1%5D%5Bfilters%5D%5B1%5D%5Bfield-value%5D=Bacteria&filter-group%5B1%5D%5Bfilters%5D%5B1%5D%5Bfield-validation%5D=strains-domain-1
    if (typestrain_only) {
      my_list %<>%
        dplyr::filter(is_type_strain_header == 1)
    }
    # If you try to download more than 100 you get warnings and errors like this one:
    # Warning message:
    #   In download_json_with_retry(url, object) :
    #   [API] title: BacDive API error; code: 400; message: You exceeded the maximum amount of 100 ids per request.
    # so we have to chunk the ID's into blocks of 100 (or less) which is what this function does:

    chunk <- function(x, n) (mapply(function(a, b) (x[a:b]), seq.int(from=1, to=length(x), by=n), pmin(seq.int(from=1, to=length(x), by=n)+(n-1), length(x)), SIMPLIFY=FALSE)) #https://stackoverflow.com/a/27626007/408202
    my_indices <- chunk(my_list$ID, 100)
    list_holder <- list()
    for (i in my_indices) {
      temp_list <- BacDive::fetch(bacdive_keycloak, i)
      list_holder <- c(list_holder, temp_list$results)
    }
    attr(list_holder, "date_downloaded") <- Sys.Date() # have this so we know when the data was downloaded
    readr::write_rds(list_holder, here::here(rda)) #so if we re-run the script we don't have to keep GETing data from bacdive
  }
  return(list_holder)
}
