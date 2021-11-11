#' Phenotype data of bacterial type strains from bacdive.org
#' @format A data frame with the following columns:
#' \describe{
#'   \item{ID}{The BacDive ID.}
#'   \item{taxon}{The name of the taxon.}
#'   \item{rank}{The rank of the taxon.}
#'   \item{aerobic_status}{
#'     The aerobic status. Multiple values including "aerobe", "facultative anaerobe", and
#'     "obligate anaerobe".}
#'   \item{gram_stain}{
#'     How the taxon appears when Gram-stained. One of "positive" or
#'     "negative".}
#' }
#' @note Data frame also has the attribute "date_downloaded" for when the data was downloaded from bacdive.org.
"bacdive_phenotypes"

#' Antibiotic susceptibility data from bacdive.org
#' @format A data frame with the following columns:
#' \describe{
#'   \item{ID}{The BacDive ID.}
#'   \item{taxon}{The name of the taxon.}
#'   \item{rank}{The rank of the taxon.}
#'   \item{antibiotic}{The antibiotic or antibiotic class.}
#'   \item{value}{
#'     The susceptibility of the taxon to the antibiotic, one of "susceptible"
#'     or "resistant".}
#' }
#' @note Data frame also has the attribute "date_downloaded" for when the data was downloaded from bacdive.org.
"bacdive_susceptibility"

#' Enzyme activity data from bacdive.org
#' @format A data frame with the following columns:
#' \describe{
#'   \item{ID}{The BacDive ID.}
#'   \item{taxon}{The name of the taxon.}
#'   \item{rank}{The rank of the taxon.}
#'   \item{activity}{Whether the enzyme is active or not with '+' being active, '-' being non-active, and '+/-' being variable.}
#'   \item{value}{The enzyme name.}
#'   \item{ec}{[The Enzyme Commission number.](https://en.wikipedia.org/wiki/Enzyme_Commission_number)}
#'   \item{doi}{Digital object identifier for the literature that supports the evidence of activity.}
#' }
#' @note Data frame also has the attribute "date_downloaded" for when the data was downloaded from bacdive.org.
"bacdive_enzymes"

# TODO
#' Data frame from Shen et al. 2021 (liver cirrhosis kraken results)
