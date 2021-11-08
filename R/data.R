#' Phenotype data of bacterial type strains from bacdive.org
#' @format A data frame with the following columns:
#' \describe{
#'   \item{ID}{The BacDive ID}
#'   \item{taxon}{The name of the taxon}
#'   \item{rank}{The rank of the taxon}
#'   \item{aerobic_status}{
#'     The aerobic status. One of "aerobe", "facultative anaerobe", or
#'     "obligate anaerobe".}
#'   \item{gram_stain}{
#'     How the taxon appears when Gram-stained. One of "Gram-positive" or
#'     "Gram-negative".}
#' }
"bacdive_phenotypes"

#' Antibiotic susceptibility data from bacdive.org
#' @format A data frame with the following columns:
#' \describe{
#'   \item{ID}{The BacDive ID}
#'   \item{taxon}{The name of the taxon}
#'   \item{rank}{The rank of the taxon}
#'   \item{antibiotic}{The antibiotic or antibiotic class}
#'   \item{value}{
#'     The susceptibility of the taxon to the antibiotic, one of "susceptible"
#'     or "resistant".}
#' }
"bacdive_susceptibility"
