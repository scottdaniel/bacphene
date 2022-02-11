#' Opens a portal to the world of BacDive
#'
#' @param credentials Your login credentials for bacdive.org. Defaults to DSMZ_API_USER and DSMZ_API_PASSWORD in your Renviron file. Supplied as a character vector c("username", "password").
#'
#' @return A 'dsmz_keycloak' object that allows access to the BacDive API. See also \code{\link[BacDive]{open_bacdive}}.
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

#' Gets a single strain of a species
#'
#' @param list A list object containing strain information already present in the R environment. Required if an rda file is not given.
#' @param rda An RData file representing a full download of strain from BacDive.org's API. Can be created using download_from_bacdive.Rmd. Required if list is not given.
#' @param query Either a bacdive ID (integer) or species name (character).
#' @param typestrain_only Whether to filter down to typestrains only.
#'
#' @return A list containing information on the strain of a species.
#'
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

#' Extracts cell morphology data from a single entry in a bacdive structured list
#'
#' @param bacdive_entry A single entry representing a strain within a bacdive list.
#'
#' @return A dataframe of cell morphology information about the strain.
#'
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
#'
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

#' Obsolete -- do no use -- for testing porpoises only
#'
#' @param list_holder A list object containing strain information already present in the R environment.
#'
#' @return A dataframe of cell morphology information about taxa in the list.
#'
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

#' Extracts oxygen tolerance data from a single entry in a bacdive structured list
#'
#' @param bacdive_entry A single entry representing a strain within a bacdive list.
#'
#' @return A dataframe of oxygen tolerance information about the strain.
#'
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
#'
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
#'
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

#' Get the regular antibiotic susceptibility / resistance information from a single bacdive entry
#'
#' @param bacdive_entry A single entry representing a strain within a bacdive list.
#'
#' @return A dataframe of antibiotic susceptibility / resistance information for a single bacdive entry.
#'
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
#'
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
#'
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
#'
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
#'
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
#'
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
#'
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
