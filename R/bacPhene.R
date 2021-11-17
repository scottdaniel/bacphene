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

  # maybe just have this function open bacdive with default credentials and nothing else
  bacdive <- BacDive::open_bacdive(credentials[[1L]], credentials[[2L]])
  getList(object = bacdive, query = query, search = "taxon", sleep = sleep, handler = handler)

}

#' Gets a single type strain of a species
#'
#' @param rda An RData file representing a full download of type strain from BacDive.org's API. Can be created using download_from_bacdive.R OR
#' @param list A list object containing strain information already present in the R environment
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
getTypeStrainLocal <- function(list = NULL, rda = "data-raw/type_strain_large_list.rda", query) {

  if (is.null(list)) {
    list <- readRDS(rda)
  }

  rlist::list.filter(list, `Name and taxonomic classification`$species %in% query)

}

#' Extracts cell morphology data from a bacdive list
#'
#' @param list_holder A list object containing strain information already present in the R environment
#'
#' @return A dataframe of cell morphology information about taxa in the list
#' @export
#'
#' @importFrom dplyr bind_rows mutate left_join select
#' @importFrom tibble tibble
#'
#' @examples
#' \dontrun{
#' morphology_df <- getMorphology(list_holder)
#' }
getMorphology <- function(list_holder = list_holder) {
  bacdive_morphology <- tibble::tibble()
  for (i in 1:length(list_holder)) {
    if (!is.null(list_holder[[i]]$Morphology$`cell morphology`)) {
      ref_df <-
        dplyr::bind_rows(list_holder[[i]]$Reference) %>% dplyr::select(`@id`, `doi/url`)
      cell_morphology_df <-
        dplyr::bind_rows(list_holder[[i]]$Morphology$`cell morphology`) %>%
        dplyr::mutate(
          ID = list_holder[[i]]$General$`BacDive-ID`,
          taxon = list_holder[[i]]$`Name and taxonomic classification`$species,
          rank = "Species"
        ) %>%
        dplyr::left_join(ref_df, by = c("@ref" = "@id"))
      bacdive_morphology <-
        dplyr::bind_rows(bacdive_morphology, cell_morphology_df)
    }
  }
  attr(bacdive_morphology, "date_downloaded") <-
    attr(list_holder, "date_downloaded")
  return(bacdive_morphology)
}

#' Extracts oxygen tolerance data from a bacdive list
#'
#' @param list_holder A list object containing strain information already present in the R environment
#'
#' @return A dataframe of oxygen tolerance information about taxa in the list
#' @export
#'
#' @importFrom dplyr bind_rows mutate left_join select
#' @importFrom tibble tibble
#'
#' @examples
#' \dontrun{
#' oxygen_df <- getOxygen(list_holder)
#' }
getOxygen <- function(list_holder = list_holder) {
  bacdive_oxygen <- tibble::tibble()
  for (i in 1:length(list_holder)) {
    if (!is.null(list_holder[[i]]$`Physiology and metabolism`$`oxygen tolerance`)) {
      ref_df <-
        dplyr::bind_rows(list_holder[[i]]$Reference) %>% dplyr::select(`@id`, `doi/url`)
      oxygen_df <-
        dplyr::bind_rows(list_holder[[i]]$`Physiology and metabolism`$`oxygen tolerance`) %>%
        dplyr::mutate(
          ID = list_holder[[i]]$General$`BacDive-ID`,
          taxon = list_holder[[i]]$`Name and taxonomic classification`$species,
          rank = "Species"
        ) %>%
        dplyr::left_join(ref_df, by = c("@ref" = "@id"))
      bacdive_oxygen <-
        dplyr::bind_rows(bacdive_oxygen, oxygen_df)
    }
  }
  attr(bacdive_oxygen, "date_downloaded") <-
    attr(list_holder, "date_downloaded")
  return(bacdive_oxygen)
}

#' Combined oxygen tolerance and gram stain data for use with other packages
#'
#' @param morphology_df A dataframe from \code{\link{getMorphology}}
#' @param oxygen_df A dataframe from \code{\link{getOxygen}}
#'
#' @return A dataframe of oxygen tolerance and gram stain information about taxa in the list
#' @export
#'
#' @importFrom dplyr select full_join filter group_by count ungroup
#' @importFrom magrittr %<>%
#'
#' @examples
#' \dontrun{
#' morphology_df <- getMorphology(list_holder)
#' oxygen_df <- getOxygen(list_holder)
#' phenotype_df <- getPhenotypes(morphology_df, oxygen_df)
#' }
getPhenotypes <- function(morphology_df, oxygen_df) {
  # this introduces duplicates due to multiple sources of information
  bacdive_phenotypes <- oxygen_df %>%
    dplyr::select(ID, taxon, rank, aerobic_status = `oxygen tolerance`) %>%
    dplyr::full_join(morphology_df %>% dplyr::select(ID, gram_stain = `gram stain`), by = "ID")

  # if both gram_stain and aerobic_status are NA then it's not useful to keep
  bacdive_phenotypes %<>%
    dplyr::filter(!is.na(gram_stain) | !is.na(aerobic_status))

  # only keeps the most common combination of aerobic status and gram stain
  bacdive_phenotypes %<>%
    dplyr::group_by(ID, taxon, rank) %>%
    dplyr::count(gram_stain, aerobic_status) %>%
    dplyr::filter(n == max(n)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-n)

  if (!attr(oxygen_df, "date_downloaded") %in% attr(morphology_df, "date_downloaded")) {
    simpleError(paste0("Error: Date downloaded does not match for oxygen_df ", attr(oxygen_df, "date_downloaded"), " and morphology_df ", attr(morphology_df, "date_downloaded"), ". Did you get your dataframes elsewhere?"))
  }

  attr(bacdive_phenotypes, "date_downloaded") <-
    attr(oxygen_df, "date_downloaded")

  return(bacdive_phenotypes)
}
