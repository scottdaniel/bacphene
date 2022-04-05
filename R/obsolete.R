#' Obsolete -- do no use -- for testing porpoises only
#'
#' @param list_holder A list object containing strain information already present in the R environment.
#'
#' @return A dataframe of cell morphology information about taxa in the list.
#' @importFrom dplyr bind_rows mutate left_join select
#' @importFrom tibble tibble
#'
#' @examples
#' \dontrun{
#' morphology_df <- getMorphologyOld(list_holder)
#' }
getMorphologyOld <- function(list_holder = list_holder) {
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

#' Get full antibiotic susceptibility / resistance information
#'
#' @param list_holder A list object containing strain information already present in the R environment.
#'
#' @return A dataframe of antibiotic susceptibility / resistance information about taxa in the list.
#'
#' @importFrom dplyr bind_rows mutate left_join select all_of
#' @importFrom tibble tibble
#' @importFrom magrittr %<>%
#' @importFrom tidyr pivot_longer
#'
#' @return
#'
#' @examples
#' \dontrun{
#' bacdive_abx <- getAbx(list_holder)
#' }
getAbxOld <- function(list_holder = list_holder) {
  bacdive_abx <- tibble::tibble()
  for (i in 1:length(list_holder)) {
    if (!is.null(list_holder[[i]]$`Physiology and metabolism`$`antibiotic resistance`)) {
      ref_df <-
        dplyr::bind_rows(list_holder[[i]]$Reference)

      if ("doi/url" %in% names(ref_df)) {
        ref_df %<>% dplyr::select(`@id`, `doi/url`)
      } else {
        ref_df %<>% dplyr::select(`@id`) %>% mutate(`doi/url` = "Unknown")
      }

      abx_df <-
        dplyr::bind_rows(list_holder[[i]]$`Physiology and metabolism`$`antibiotic resistance`) %>%
        dplyr::mutate(
          ID = list_holder[[i]]$General$`BacDive-ID`,
          taxon = list_holder[[i]]$`Name and taxonomic classification`$species,
          rank = "Species",
          type_strain = list_holder[[i]]$`Name and taxonomic classification`$`type strain`
        ) %>%
        dplyr::left_join(ref_df, by = c("@ref" = "@id"))
      bacdive_abx <-
        dplyr::bind_rows(bacdive_abx, abx_df)
    } else if (!is.null(list_holder[[i]]$`Physiology and metabolism`$antibiogram)) {

      ref_df <-
        dplyr::bind_rows(list_holder[[i]]$Reference)

      if ("doi/url" %in% names(ref_df)) {
        ref_df %<>% dplyr::select(`@id`, `doi/url`)
      } else {
        ref_df %<>% dplyr::select(`@id`) %>% dplyr::mutate(`doi/url` = "Unknown")
      }

      abx_df <-
        dplyr::bind_rows(list_holder[[i]]$`Physiology and metabolism`$antibiogram)

      contains_columns <- intersect(names(abx_df), c("@ref", "medium", "incubation temperature", "oxygen condition", "incubation time"))

      abx_df %<>%
        tidyr::pivot_longer(cols = -c(dplyr::all_of(contains_columns)), names_to = "metabolite", values_to = "diameter") %>%
        dplyr::mutate(
          ID = list_holder[[i]]$General$`BacDive-ID`,
          taxon = list_holder[[i]]$`Name and taxonomic classification`$species,
          rank = "Species",
          type_strain = list_holder[[i]]$`Name and taxonomic classification`$`type strain`
        ) %>%
        dplyr::left_join(ref_df, by = c("@ref" = "@id"))
      bacdive_abx <-
        dplyr::bind_rows(bacdive_abx, abx_df)
    }
  }
  attr(bacdive_abx, "date_downloaded") <-
    attr(list_holder, "date_downloaded")
  return(bacdive_abx)
}
