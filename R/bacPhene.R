#' Opens a portal to the world of BacDive
#'
#' @param credentials Your login credentials for bacdive.org. Defaults to DSMZ_API_USER and DSMZ_API_PASSWORD in your Renviron file. Supplied as a character vector c("username", "password").
#'
#' @return A 'dsmz_keycloak' object that allows access to the BacDive API. See also \code{\link[BacDive]{open_bacdive}}.
#' @export
#'
#' @importFrom BacDive open_bacdive
#'
#' @examples
#' \dontrun{
#' bacdive <- getBacDiveAccess()
#' }
getBacDiveAccess <- function(credentials = Sys.getenv(c("DSMZ_API_USER", "DSMZ_API_PASSWORD"))) {

  bacdive <- BacDive::open_bacdive(credentials[[1L]], credentials[[2L]])

}

#' Gets a single type strain of a species
#'
#' @param list A list object containing strain information already present in the R environment. Required if an rda file is not given.
#' @param rda An RData file representing a full download of type strain from BacDive.org's API. Can be created using download_from_bacdive.Rmd. Required if list is not given.
#' @param query A species.
#'
#' @return A list containing information on the type strain of a species.
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
#' @param list_holder A list object containing strain information already present in the R environment.
#'
#' @return A dataframe of cell morphology information about taxa in the list.
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
          rank = "Species",
          type_strain = list_holder[[i]]$`Name and taxonomic classification`$`type strain`
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
#' @param list_holder A list object containing strain information already present in the R environment.
#'
#' @return A dataframe of oxygen tolerance information about taxa in the list.
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
          rank = "Species",
          type_strain = list_holder[[i]]$`Name and taxonomic classification`$`type strain`
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
#' @param morphology_df A dataframe from \code{\link{getMorphology}}.
#' @param oxygen_df A dataframe from \code{\link{getOxygen}}.
#'
#' @return A dataframe of oxygen tolerance and gram stain information about taxa in the list.
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
    dplyr::full_join(morphology_df %>% dplyr::select(ID, taxon, rank, gram_stain = `gram stain`), by = c("ID", "taxon", "rank"))

  # if both gram_stain and aerobic_status are NA then it's not useful to keep
  bacdive_phenotypes %<>%
    dplyr::filter(!is.na(gram_stain) | !is.na(aerobic_status))

  # only keeps the most common combination of aerobic status and gram stain
  # we lose the ID because each taxon has different ID's
  bacdive_phenotypes %<>%
    dplyr::group_by(taxon, rank) %>%
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

#' Downloads BacDive strains from a list of supplied ID's
#'
#' @param strain_list A csv file containing at least two columns: "ID" - The BacDive ID and "is_type_strain_header". Can be gotten using \href{https://bacdive.dsmz.de/advsearch}{BacDive's advanced search}.
#' @param typestrain_only Whether to filter down to typestrains only.
#' @param bacdive_keycloak A 'dsmz_keycloak' object that allows access to the BacDive API. Created using \code{\link{getBacDiveAccess}} or \code{\link[BacDive]{open_bacdive}}.
#' @param save_rda If this is being run for the first time, the list of strains will be saved to the specificed RData file. Otherwise, the function will load strains from the specified data file.
#'
#' @return A list object of strain information. If running for the first time, saves an RData file of the list.
#' @export
#'
#' @importFrom here here
#' @importFrom readr read_rds read_csv write_rds
#' @importFrom magrittr %<>%
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
        filter(is_type_strain_header == 1)
    }
    # If you try to download more than 100 you get warnings and errors like this one:
    # Warning message:
    #   In download_json_with_retry(url, object) :
    #   [API] title: BacDive API error; code: 400; message: You exceeded the maximum amount of 100 ids per request.
    list_holder <- list()
    for (i in seq(1, length(my_list$ID), 100)) {
      if (i + 99 > length(my_list$ID)) {
        k = length(my_list$ID) #since the last block would include NA's otherwise
      } else {
        k = i + 99
      }
      temp_list <- fetch(bacdive_keycloak, my_list$ID[i:k])
      list_holder <- c(list_holder, temp_list$results)
    }
    attr(list_holder, "date_downloaded") <- Sys.Date() # have this so we know when the data was downloaded
    readr::write_rds(list_holder, here::here(rda)) #so if we re-run the script we don't have to keep GETing data from bacdive
  }
  return(list_holder)
}
