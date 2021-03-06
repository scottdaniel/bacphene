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
#' strain_list <- getStrains(page = 1,
#' genus = 'Bacteroides',
#' species = 'xylanisolvens',
#' userpassword = paste0(user,':',passwd))
#' }
getStrains <- function(page, genus, species, userpassword) {

  #for more info on api go to https://bacdive.dsmz.de/api/bacdive/example/
  api_entry <- 'https://bacdive.dsmz.de/api/bacdive/taxon/'
  url_species <- utils::URLencode(paste0(api_entry, genus, '/', species, '/?page=', page, '&format=json'))

  response <- RCurl::getURL(url_species,userpwd=userpassword, httpauth = 1L) #getting data as a json object
  jsondata <- rjson::fromJSON(response) #converting the json into a R list object

}

#' Gets the phenotype data for a specific strain
#'
#' @param speciesData The list object that you got from \code{\link{getStrains}}
#' @param selection Which strain you want from the list object
#' @param userpassword The userpassword in the form of "user:password" that you got when you registered at bacdive.org
#'
#' @return A list of phenotype data from bacdive.org
#' @export
#'
#' @importFrom RCurl getURL
#' @importFrom rjson fromJSON
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
#' }
getStrainData <- function(speciesData, selection=1, userpassword) {

  response <- RCurl::getURL(paste0(speciesData[["results"]][[selection]][["url"]],'/?format=json'), userpwd=userpassword, httpauth = 1L) #getting the data as a json object

  strainData <- rjson::fromJSON(response) #converting the data to a R list

}

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
