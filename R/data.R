# TODO: Documentation for bacdive_morphology, bacdive_oxygen, and bacdive_abx. Check documentation for bacdive_phenotypes, bacdive_susceptibility, and bacdive_enzymes.

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
#'   \item{ec}{\href{https://en.wikipedia.org/wiki/Enzyme_Commission_number}{The Enzyme Commission number.}}
#'   \item{doi}{Digital object identifier for the literature that supports the evidence of activity.}
#' }
#' @note Data frame also has the attribute "date_downloaded" for when the data was downloaded from bacdive.org.
"bacdive_enzymes"

#' Taxonomic count data from shotgun metagenomics
#' @format A data frame with the following columns:
#' \describe{
#'   \item{SampleID}{The unique SampleID.}
#'   \item{SampleType}{The type of sample: either "Rectal swab" or "Feces".}
#'   \item{ETOH_etiology}{Whether the liver disease was alcohol related or not.}
#'   \item{Antibiotics}{The type of antibiotic the patient was taking.}
#'   \item{MELD}{MELD Score (Model For End-Stage Liver Disease). Higher numbers indicate worse prognosis.}
#'   \item{read_counts}{Total read counts in the sample.}
#'   \item{taxon}{Taxon at the species level.}
#'   \item{count}{Count of the Taxon.}
#' }
#' @note Data obtained from the following study: Shen, T.-C. D. et al. (2021) ‘The Mucosally-Adherent Rectal Microbiota Contains Features Unique to Alcohol-Related Cirrhosis’, Gut microbes, 13(1), p. 1987781. \url{https://www.ncbi.nlm.nih.gov/pubmed/34747331}.
"Shen2021"
