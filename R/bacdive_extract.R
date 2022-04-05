# A set of functions to get data about strains from bacdive.org

#' Extracts cell morphology data from a single entry in a bacdive structured list
#'
#' @param bacdive_entry A single entry representing a strain within a bacdive list.
#'
#' @return A dataframe of cell morphology information about the strain.
#' @export
#' @importFrom dplyr bind_rows mutate left_join select
#'
#' @examples
#' \dontrun{
#' morphology_df <- bind_rows(lapply(list_holder, getMorphologySingle))
#' # OR
#' single_strain <- getMorphologySingle(list_holder[[1]])
#' }
#' @details Used in \code{\link{getMorphology}}.
getMorphologySingle <- function(bacdive_entry) {
  if (!is.null(bacdive_entry$Morphology$`cell morphology`)) {
    ref_df <-
      dplyr::bind_rows(bacdive_entry$Reference) %>% dplyr::select(`@id`, `doi/url`)
    cell_morphology_df <-
      dplyr::bind_rows(bacdive_entry$Morphology$`cell morphology`) %>%
      dplyr::mutate(
        ID = bacdive_entry$General$`BacDive-ID`,
        taxon = bacdive_entry$`Name and taxonomic classification`$species,
        rank = "Species",
        type_strain = bacdive_entry$`Name and taxonomic classification`$`type strain`
      ) %>%
      dplyr::left_join(ref_df, by = c("@ref" = "@id"))
  }
}

#' Extracts cell morphology data from a bacdive list
#'
#' @param list_holder A list object containing strain information already present in the R environment.
#'
#' @return A dataframe of cell morphology information about taxa in the list.
#' @export
#' @importFrom dplyr bind_rows
#'
#' @examples
#' \dontrun{
#' morphology_df <- getMorphology(list_holder)
#' }
#' @details Essentially a wrapper for applying \code{\link{getMorphologySingle}} to a list and turning the result into a dataframe. Also adds the "date_downloaded" attribute from the list to the dataframe.
getMorphology <- function(list_holder = list_holder) {
  bacdive_morphology <- dplyr::bind_rows(lapply(list_holder, getMorphologySingle))
  attr(bacdive_morphology, "date_downloaded") <-
    attr(list_holder, "date_downloaded")
  return(bacdive_morphology)
}

#' Extracts oxygen tolerance data from a single entry in a bacdive structured list
#'
#' @param bacdive_entry A single entry representing a strain within a bacdive list.
#'
#' @return A dataframe of oxygen tolerance information about the strain.
#' @export
#' @importFrom dplyr bind_rows mutate left_join select
#'
#' @examples
#' \dontrun{
#' morphology_df <- bind_rows(lapply(list_holder, getOxygenSingle))
#' # OR
#' single_strain <- getOxygenSingle(list_holder[[1]])
#' }
#' @details Used in \code{\link{getOxygen}}.
getOxygenSingle <- function(bacdive_entry) {
    if (!is.null(bacdive_entry$`Physiology and metabolism`$`oxygen tolerance`)) {
      ref_df <-
        dplyr::bind_rows(bacdive_entry$Reference) %>% dplyr::select(`@id`, `doi/url`)
      oxygen_df <-
        dplyr::bind_rows(bacdive_entry$`Physiology and metabolism`$`oxygen tolerance`) %>%
        dplyr::mutate(
          ID = bacdive_entry$General$`BacDive-ID`,
          taxon = bacdive_entry$`Name and taxonomic classification`$species,
          rank = "Species",
          type_strain = bacdive_entry$`Name and taxonomic classification`$`type strain`
        ) %>%
        dplyr::left_join(ref_df, by = c("@ref" = "@id"))
  }
}

#' Extracts oxygen tolerance data from a bacdive list
#'
#' @param list_holder A list object containing strain information already present in the R environment.
#'
#' @return A dataframe of oxygen tolerance information about taxa in the list.
#' @export
#' @importFrom dplyr bind_rows
#'
#' @examples
#' \dontrun{
#' oxygen_df <- getOxygen(list_holder)
#' }
getOxygen <- function(list_holder = list_holder) {
  bacdive_oxygen <- dplyr::bind_rows(lapply(list_holder, getOxygenSingle))
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

#' Get the regular antibiotic susceptibility / resistance information from a single bacdive entry
#'
#' @param bacdive_entry A single entry representing a strain within a bacdive list.
#'
#' @return A dataframe of antibiotic susceptibility / resistance information for a single bacdive entry.
#' @export
#' @importFrom dplyr bind_rows mutate left_join select
#' @importFrom magrittr %<>%
#'
#' @details Some antibiotic information is encoded as an antibiogram, see \code{\link{getAntibiogramSingle}}
#' @examples
#' \dontrun{
#' abx_df <- bind_rows(lapply(list_holder, getAbxSingle))
#' # OR
#' single_strain <- getAbxSingle(list_holder[[1]])
#' }
getAbxSingle <- function(bacdive_entry) {
    if (!is.null(bacdive_entry$`Physiology and metabolism`$`antibiotic resistance`)) {
      ref_df <-
        dplyr::bind_rows(bacdive_entry$Reference)

      if ("doi/url" %in% names(ref_df)) {
        ref_df %<>% dplyr::select(`@id`, `doi/url`)
      } else {
        ref_df %<>% dplyr::select(`@id`) %>% mutate(`doi/url` = "Unknown")
      }
      abx_df <-
        dplyr::bind_rows(bacdive_entry$`Physiology and metabolism`$`antibiotic resistance`) %>%
        dplyr::mutate(
          ID = bacdive_entry$General$`BacDive-ID`,
          taxon = bacdive_entry$`Name and taxonomic classification`$species,
          rank = "Species",
          type_strain = bacdive_entry$`Name and taxonomic classification`$`type strain`
        ) %>%
        dplyr::left_join(ref_df, by = c("@ref" = "@id"))
    }
}

#' Get the regular antibiotic susceptibility / resistance information from a list of bacdive entries
#'
#' @param list_holder A list object containing strain information already present in the R environment.
#'
#' @return A dataframe of antibiotic susceptibility / resistance information about taxa in the list.
#' @export
#' @importFrom dplyr bind_rows
#'
#' @examples
#' \dontrun{
#' abx_df <- getAbx(list_holder)
#' }
getAbx <- function(list_holder = list_holder) {
  bacdive_abx <- dplyr::bind_rows(lapply(list_holder, getAbxSingle))
  attr(bacdive_abx, "date_downloaded") <-
    attr(list_holder, "date_downloaded")
  return(bacdive_abx)
}

#' Get the antibiogram susceptibility / resistance information for a single entry
#'
#' @param bacdive_entry A single entry representing a strain within a bacdive list.
#'
#' @return A dataframe of antibiogram susceptibility / resistance information for a single bacdive entry.
#' @export
#' @importFrom dplyr bind_rows mutate left_join select all_of
#' @importFrom magrittr %<>%
#' @importFrom tidyr pivot_longer
#'
#' @details This is similar information from what is gotten from \code{\link{getAbxSingle}} but uses a standardized format called the antibiogram
#' @examples
#' \dontrun{
#' antibiogram_df <- bind_rows(lapply(list_holder, getAntibiogramSingle))
#' # OR
#' single_strain <- getAntibiogramSingle(list_holder[[1]])
#' }
getAntibiogramSingle <- function(bacdive_entry) {
  if (!is.null(bacdive_entry$`Physiology and metabolism`$antibiogram)) {
    ref_df <-
      dplyr::bind_rows(bacdive_entry$Reference)

    if ("doi/url" %in% names(ref_df)) {
      ref_df %<>% dplyr::select(`@id`, `doi/url`)
    } else {
      ref_df %<>% dplyr::select(`@id`) %>% dplyr::mutate(`doi/url` = "Unknown")
    }
    abx_df <-
      dplyr::bind_rows(bacdive_entry$`Physiology and metabolism`$antibiogram)
    contains_columns <- intersect(names(abx_df), c("@ref", "medium", "incubation temperature", "oxygen condition", "incubation time"))
    abx_df %<>%
      tidyr::pivot_longer(cols = -c(dplyr::all_of(contains_columns)), names_to = "metabolite", values_to = "diameter") %>%
      dplyr::mutate(
        ID = bacdive_entry$General$`BacDive-ID`,
        taxon = bacdive_entry$`Name and taxonomic classification`$species,
        rank = "Species",
        type_strain = bacdive_entry$`Name and taxonomic classification`$`type strain`
      ) %>%
      dplyr::left_join(ref_df, by = c("@ref" = "@id"))
  }
}

#' Get the antibiogram susceptibility / resistance information from a list of bacdive entries
#'
#' @param list_holder A list object containing strain information already present in the R environment.
#'
#' @return A dataframe of antibiogram susceptibility / resistance information about taxa in the list.
#' @export
#' @importFrom dplyr bind_rows
#'
#' @examples
#' \dontrun{
#' antibiogram_df <- getAntibiogram(list_holder)
#' }
getAntibiogram <- function(list_holder = list_holder) {
  bacdive_antibiogram <- dplyr::bind_rows(lapply(list_holder, getAntibiogramSingle))
  attr(bacdive_antibiogram, "date_downloaded") <-
    attr(list_holder, "date_downloaded")
  return(bacdive_antibiogram)
}

#' Simplify the full antibiotic susceptibility / resistance information
#'
#' @param data A dataframe of antibiotic susceptibility / resistance information from \code{\link{getAbx}}.
#' @param extra_info Whether to return some extra info for testing purposes. This will include the columns `is resistant`, `is_sensitive`, and `diameter`.
#' @param most_common The function will take the most common value for any given antibiotic / species combination. See details.
#' @param remove_unknown Remove antibiotic / species combinations where the resistance / sensitivity information is unknown or variable.
#'
#' @return A simplified dataframe of antibiotic susceptibility / resistance information.
#' @export
#' @importFrom dplyr bind_rows mutate left_join select all_of
#' @importFrom stringr str_to_lower
#' @importFrom magrittr %<>%
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \dontrun{
#' bacdive_susceptibility <- getSimplifiedAbx(bacdive_abx)
#' }
#' @details Hypothetically, a species might have 50 strains that are resistant to Penicillin and 25 strains that are sensitive. With `most_common = T` the function will collapse the results to the most common value. Thus, the returned dataframe will have this species as being resistant to Penicillin.
getSimplifiedAbx <- function(data = bacdive_abx, extra_info = F, most_common = T, remove_unknown = T) {

  bacdive_susceptibility <- data %>% dplyr::select(-c("is antibiotic", "ChEBI", "resistance conc.", "sensitivity conc.", "medium", "incubation temperature", "oxygen condition", "incubation time", "group ID", "is intermediate", "intermediate conc.")) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        `is resistant` %in% "yes" | diameter %in% "0" ~ "resistant",
        `is sensitive` %in% "yes" |
          !(diameter %in% "0") ~ "sensitive"
      )
    ) %>%
    dplyr::mutate(value = ifelse((`is resistant` %in% "no" | `is sensitive` %in% "no") & is.na(diameter) | diameter %in% "n.d.", "unknown", value)) %>% # can not get this to work in the case_when
    dplyr::mutate(value = ifelse(`is sensitive` %in% "no" & `is resistant` %in% "yes", "resistant", value)) %>% # the weird ones
    dplyr::mutate(value = ifelse(`is sensitive` %in% "yes" & `is resistant` %in% "no", "sensitive", value)) %>% # there are not any of these but best to future-proof
    dplyr::mutate(value = ifelse(`is sensitive` %in% "yes" & `is resistant` %in% "yes", "unknown", value)) %>% # this should not be possible but there it is
    dplyr::mutate(antibiotic = stringr::str_to_lower(metabolite)) # to deal with some inconsistent capitlization

  if (extra_info) {
    bacdive_susceptibility %<>% dplyr::select(ID, taxon, rank, antibiotic, value, `is sensitive`, `is resistant`, diameter)
  } else {
    bacdive_susceptibility %<>% dplyr::select(ID, taxon, rank, antibiotic, value)
  }

  if (most_common) {
    bacdive_susceptibility %<>%
      dplyr::group_by(taxon, rank, antibiotic) %>%
      dplyr::count(value) %>%
      dplyr::filter(n == max(n)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-n)
  }

  if (remove_unknown) {
    bacdive_susceptibility %<>%
      dplyr::filter(!value %in% "unknown")
  }

  attr(bacdive_susceptibility, "date_downloaded") <-
    attr(data, "date_downloaded")
  return(bacdive_susceptibility)

}

#' Extracts enzyme data from a bacdive entry
#'
#' @param bacdive_entry A single entry representing a strain within a bacdive list.
#'
#' @return A dataframe of enzyme information for a single bacdive entry.
#' @export
#' @importFrom dplyr bind_rows mutate left_join select
#'
#' @examples
#' \dontrun{
#' enzymes_df <- bind_rows(lapply(list_holder, getEnzymesSingle))
#' # OR
#' single_strain <- getEnzymesSingle(list_holder[[1]])
#' }
getEnzymesSingle <- function(bacdive_entry) {
  if (!is.null(bacdive_entry$`Physiology and metabolism`$enzymes)) {
    ref_df <-
      dplyr::bind_rows(bacdive_entry$Reference) %>% dplyr::select(`@id`, `doi/url`)
    enzyme_df <-
      dplyr::bind_rows(bacdive_entry$`Physiology and metabolism`$enzymes) %>%
      dplyr::mutate(
        ID = bacdive_entry$General$`BacDive-ID`,
        taxon = bacdive_entry$`Name and taxonomic classification`$species,
        rank = "Species"
      ) %>%
      dplyr::left_join(ref_df, by = c("@ref" = "@id"))
    bacdive_enzymes <- dplyr::bind_rows(bacdive_enzymes, enzyme_df)
  }
}

#' Extracts enzyme data from a bacdive list
#'
#' @param list_holder A list object containing strain information already present in the R environment.
#' @param most_common The function will take the most common value for any given species enzyme activity combination. See details.
#' @param remove_unknown Remove enzyme activity entries where the value is NA.
#'
#' @return A dataframe of enzyme information about taxa in the list.
#' @export
#' @importFrom dplyr bind_rows
#'
#' @examples
#' \dontrun{
#' bacdive_enzymes <- getEnzymes(list_holder)
#' }
#' @details Staphylococcus aureus has 31 strains with positive catalase activity and 2 strains with negative catalase activity. With `most_common = T`, the function will return a dataframe denoting that S. aureus is most commonly positive for catalase activity.
getEnzymes <- function(list_holder = list_holder, most_common = T, remove_unknown = T) {
  bacdive_enzymes <- dplyr::bind_rows(lapply(list_holder, getEnzymesSingle))
  if (most_common) {
    bacdive_enzymes %<>%
      dplyr::group_by(taxon, rank, value, ec) %>%
      dplyr::count(activity) %>%
      dplyr::filter(n == max(n)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-n)
  }
  if (remove_unknown) {
    bacdive_enzymes %<>%
      dplyr::filter(!is.na(activity))
  }
  attr(bacdive_enzymes, "date_downloaded") <-
    attr(list_holder, "date_downloaded")
  return(bacdive_enzymes)
}

