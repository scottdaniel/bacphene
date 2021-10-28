#' Gets the strains from bacdive that belong to a supplied species
#'
#' @param credentials Your login credentials for bacdive.org. Defaults to DSMZ_API_USER and DSMZ_API_PASSWORD in your Renviron file. Supplied as a character vector c("username", "password").
#' @param query Official Genus species taxonomic name.
#' @param sleep A waiting period in seconds between successive API requests, if any.
#' @param userpassword The userpassword in the form of "user:password" that you got when you registered at bacdive.org
#'
#' @return A list of strains with phenotype information from bacdive.org
#' @export
#'
#' @importFrom BacDive open_bacdive, retrieve
#'
#' @examples
#' \dontrun{
#' strain_list <- getStrains(page = 1,
#' genus = 'Bacteroides',
#' species = 'xylanisolvens',
#' userpassword = paste0(user,':',passwd))
#' }
getStrains <- function(credentials = Sys.getenv(c("DSMZ_API_USER", "DSMZ_API_PASSWORD")),
                       query, sleep = 0.1, handler = NULL, getList = BacDive::retrieve) {

  bacdive <- open_bacdive(credentials[[1L]], credentials[[2L]])
  bg2h <- list()
  getList(object = bacdive, query = query, search = "taxon", sleep = sleep, handler = function(x) bg2h <<- c(bg2h, x))
  bg2h

}

# new way:
# credentials <- Sys.getenv(c("DSMZ_API_USER", "DSMZ_API_PASSWORD"))
# bacdive <- open_bacdive(credentials[[1L]], credentials[[2L]])
# bg2h <- list()
# retrieve(object = bacdive, query = "Bacteroides xylanisolvens", search = "taxon", sleep = 0.1, handler = function(x) bg2h <<- c(bg2h, x))


#' Gets the gram-stain of the strain
#'
#' @param strainData A list from the function \code{\link{getStrainData}}
#' @param reference Sometimes strains have multiple references for the phenotype information,
#' this is where you can select the certain reference (or iterate through them)
#'
#' @return A character string "negative" or "positive"
#' @export
#'
#' @examples
#' \dontrun{
#' strain_list <- getStrains(page = 1,
#' genus = 'Bacteroides',
#' species = 'xylanisolvens',
#' userpassword = paste0(user,':',passwd))
#' phenotypes_list <- getStrainData(strain_list,
#' selection=1,
#' userpassword = paste0(user,':',passwd))
#' gramStain(phenotypes_list)
#' }
gramStain <- function(strainData, reference=1) {

  strainData[["morphology_physiology"]][["cell_morphology"]][[reference]][["gram_stain"]]

}

# new way
# bg2h[["1626"]][["Morphology"]][["cell morphology"]][[1]][["gram stain"]]

#' Gets the oxygen tolerance of the strain
#'
#' @param strainData A list from the function \code{\link{getStrainData}}
#' @param reference Sometimes strains have multiple references for the phenotype information,
#' this is where you can select the certain reference (or iterate through them)
#'
#' @return A character string e.g. "aerobe", "anaerobe", etc.
#' @export
#'
#' @examples
#' \dontrun{
#' strain_list <- getStrains(page = 1,
#' genus = 'Bacteroides',
#' species = 'xylanisolvens',
#' userpassword = paste0(user,':',passwd))
#' phenotypes_list <- getStrainData(strain_list,
#' selection=1,
#' userpassword = paste0(user,':',passwd))
#' oxygenTolerance(phenotypes_list)
#' }
oxygenTolerance <- function(strainData, reference=1) {

  strainData[["morphology_physiology"]][["oxygen_tolerance"]][[reference]][["oxygen_tol"]]

}

# new way
# bg2h[["1626"]][["Physiology and metabolism"]][["oxygen tolerance"]][[2]][["oxygen tolerance"]]


#' Checks whether a strain has a specific antibiotic sensitivity
#'
#' @param strainData A list from the function \code{\link{getStrainData}}
#' @param abx A specific antibiotic e.g. "vancomycin"
#'
#' @importFrom rlist list.which
#'
#' @return TRUE, FALSE or NULL (antibiotic info is not found)
#' @export
#'
#' @examples
#' \dontrun{
#' strain_list <- getStrains(page = 1,
#' genus = 'Actibacterium',
#' species = 'pelagium',
#' userpassword = paste0(user,':',passwd))
#' phenotypes_list <- getStrainData(strain_list,
#' selection=1,
#' userpassword = paste0(user,':',passwd))
#' abxSensitive(phenotypes_list, abx = "vancomycin")
#' }
abxSensitive <- function(strainData, abx = "vancomycin") {

  abx_list <- strainData[["morphology_physiology"]][["met_antibiotica"]]

  index_num <- rlist::list.which(abx_list, abx %in% .)

  if (length(index_num)!=0) {

    specific_abx <- abx_list[[index_num]]

    specific_abx$ab_sensitive #note: this will be NULL if the strain is actually
    #resistant to this particulare antibiotic

  } else {

    return(NULL) #abx info not found

  }

}

# new way
# > bg2h[["158577"]][["Physiology and metabolism"]][["antibiotic resistance"]][[1]][["metabolite"]]
# [1] "gentamicin"

#' Checks whether a strain has a specific antibiotic resistance
#'
#' @param strainData A list from the function \code{\link{getStrainData}}
#' @param abx A specific antibiotic e.g. "vancomycin"
#'
#' @importFrom rlist list.which
#'
#' @return TRUE, FALSE or NULL (antibiotic info is not found)
#' @export
#'
#' @examples
#' \dontrun{
#' strain_list <- getStrains(page = 1,
#' genus = 'Pseudomonas',
#' species = 'tarimensis',
#' userpassword = paste0(user,':',passwd))
#' phenotypes_list <- getStrainData(strain_list,
#' selection=1,
#' userpassword = paste0(user,':',passwd))
#' abxResistant(phenotypes_list, abx = "gentamicin")
#' }
abxResistant <- function(strainData, abx = "vancomycin") {

  abx_list <- strainData[["morphology_physiology"]][["met_antibiotica"]]

  index_num <- rlist::list.which(abx_list, abx %in% .)

  if (length(index_num)!=0) {

    specific_abx <- abx_list[[index_num]]

    specific_abx$ab_resistant #note: this will be NULL if the strain is actually
    #sensitive to this particulare antibiotic

  } else {

    return(NULL) #abx info not found

  }

}
