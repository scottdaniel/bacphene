
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
#' strain_list <- getStrains(page = 1,
#' genus = 'Bacteroides',
#' species = 'xylanisolvens',
#' userpassword = paste0(user,':',passwd))
#' }
getStrains <- function(credentials = Sys.getenv(c("DSMZ_API_USER", "DSMZ_API_PASSWORD")), query, sleep = 0.1, handler = NULL, getList = BacDive::retrieve) {

  bacdive <- BacDive::open_bacdive(credentials[[1L]], credentials[[2L]])
  bg2h <- list()
  getList(object = bacdive, query = query, search = "taxon", sleep = sleep, handler = function(x) bg2h <<- c(bg2h, x))
  bg2h

}

