#' Antibiotics data from bacdive.org that is _not_ in the antibiogram format. Produced by \code{\link{getAbx}}.
#' @format A data frame with the following columns:
#' \describe{
#'   \item{`@ref`}{The literature reference in the bacdive database.}
#'   \item{ChEBI}{\href{https://www.ebi.ac.uk/chebi/}{ChEBI (Chemical entities of biological interest) identifier.}}
#'   \item{metabolite}{Common name of the antibiotic.}
#'   \item{`is antibiotic`}{Should all be "yes" values.}
#'   \item{`is sensitive`}{Whether or not strain is sensitive to this antibiotic.
#'   Possible values are "yes", "no", and NA.}
#'   \item{`is resistant`}{Whether or not strain is resistant to this antibiotic.
#'   Possible values are "yes", "no", and NA.}
#'   \item{ID}{bacdive ID of strain.}
#'   \item{taxon}{The name of the taxon.}
#'   \item{rank}{The rank of the taxon.}
#'   \item{type_strain}{Whether or not the strain is the type strain for the species.}
#'   \item{`doi/url`}{DOI (Digital Object Identifier) for the reference.}
#'   \item{`resistance conc.`}{The concentration of antibiotic at which resistance was measured.}
#'   \item{`sensitivity conc.`}{The concentration of antibiotic at which sensitivity was measured.}
#'   \item{`group ID`}{Group ID of combined antibiotics
#'   i.e. bacteria was tested against multiple antibiotics at once.}
#'   \item{`is intermediate`}{Whether or not strain is intermediate sensitive to this antibiotic.}
#'   \item{`intermediate conc.`}{The concentration of antibiotic at which intermediate sensistivity was measured.}
#' }
#' @note Data frame also has the attribute "date_downloaded" for when the data was downloaded from bacdive.org.
"bacdive_abx"

#' Antibiotics data from bacdive.org that is in the \href{https://bacdive.dsmz.de/help/what-are-antibiotic-susceptibi.htm}{antibiogram format.} Produced by \code{\link{getAntibiogram}}.
#' @format A data frame with the following columns:
#' \describe{
#'   \item{`@ref`}{The literature reference in the bacdive database.}
#'   \item{medium}{Growth medium used for sensitivity / resistance test.}
#'   \item{`incubation temperature`}{Incubation temp of the test in degrees Celsius.}
#'   \item{`oxygen condition`}{Oxygen exposure during the test.}
#'   \item{metabolite}{Common name of the antibiotic.}
#'   \item{diameter}{Inhibition zone diameter in mm.}
#'   \item{ID}{bacdive ID of strain.}
#'   \item{taxon}{The name of the taxon.}
#'   \item{rank}{The rank of the taxon.}
#'   \item{type_strain}{Whether or not the strain is the type strain for the species.}
#'   \item{`doi/url`}{DOI (Digital Object Identifier) for the reference.}
#'   \item{`incubation time`}{Incubation time of test in days.}
#' }
#' @note Data frame also has the attribute "date_downloaded" for when the data was downloaded from bacdive.org.
"bacdive_antibiogram"

#' Enzyme activity data from bacdive.org. Produced by \code{\link{getEnzymes}}.
#' @format A data frame with the following columns:
#' \describe{
#'   \item{taxon}{The name of the taxon.}
#'   \item{rank}{The rank of the taxon.}
#'   \item{value}{The enzyme name.}
#'   \item{ec}{\href{https://en.wikipedia.org/wiki/Enzyme_Commission_number}{The Enzyme Commission number.}}
#'   \item{activity}{Whether the enzyme is active or not with '+' being active, '-' being non-active, and '+/-' being variable. By default, NA values are removed and the most common value is shown for a given species
#'   if it has multiple strains.}
#'   }
#' @details See `data-raw/download_from_bacdive.Rmd` for code to create this.
#' @note Data frame also has the attribute "date_downloaded" for when the data was downloaded from bacdive.org.
"bacdive_enzymes"

#' Cell morphology data from bacdive.org. Produced by \code{\link{getMorphology}}.
#' @format A data frame with the following columns:
#' \describe{
#'   \item{`@ref`}{The literature reference in the bacdive database.}
#'   \item{`gram stain`}{How the taxon appears when Gram-stained.
#'   Multiple values including "positive", "negative" or "variable". NA means the status is unknown.}
#'   \item{`cell length`}{Cell length in micrometers.}
#'   \item{`cell width`}{Cell width in micrometers.}
#'   \item{`cell shape`}{Cell shape.}
#'   \item{motility}{Whether or not strain can move.}
#'   \item{`flagellum arrangement`}{Where the flagella are arranged on the cell.}
#'   \item{ID}{bacdive ID of strain.}
#'   \item{taxon}{The name of the taxon.}
#'   \item{rank}{The rank of the taxon.}
#'   \item{type_strain}{Whether or not the strain is the type strain for the species.}
#'   \item{`doi/url`}{DOI (Digital Object Identifier) for the reference.}
#' }
#' @note Data frame also has the attribute "date_downloaded" for when the data was downloaded from bacdive.org.
"bacdive_morphology"

#' Oxygen tolerance data from bacdive.org. Produced by \code{\link{getOxygen}}.
#' @format A data frame with the following columns:
#' \describe{
#'   \item{`@ref`}{The literature reference in the bacdive database.}
#'   \item{`oxygen tolerance`}{The oxygen tolerance of the strain.
#'   Multiple values including "aerobe", "facultative anaerobe", and
#'     "obligate anaerobe", etc. NA means the status is unknown or variable.}
#'   \item{ID}{bacdive ID of strain.}
#'   \item{taxon}{The name of the taxon.}
#'   \item{rank}{The rank of the taxon.}
#'   \item{type_strain}{Whether or not the strain is the type strain for the species.}
#'   \item{`doi/url`}{DOI (Digital Object Identifier) for the reference.}
#' }
#' @note Data frame also has the attribute "date_downloaded" for when the data was downloaded from bacdive.org.
"bacdive_oxygen"

#' Phenotype data of bacterial type strains from bacdive.org.  Produced by \code{\link{getPhenotypes}}.
#' @format A data frame with the following columns:
#' \describe{
#'   \item{taxon}{The name of the taxon.}
#'   \item{rank}{The rank of the taxon.}
#'   \item{aerobic_status}{The aerobic status.
#'   Multiple values including "aerobe", "facultative anaerobe", and
#'     "obligate anaerobe", etc. NA means the status is unknown or variable.}
#'   \item{gram_stain}{How the taxon appears when Gram-stained.
#'   Multiple values including "positive", "negative" or "variable". NA means the status is unknown.}
#' }
#' @note Data frame also has the attribute "date_downloaded" for when the data was downloaded from bacdive.org.
"bacdive_phenotypes"

#' Antibiotic susceptibility data from bacdive that combines and simplifies information from \code{\link{bacdive_abx}} and \code{\link{bacdive_antibiogram}}. Produced by \code{\link{getSimplifiedAbx}}.
#' @format A data frame with the following columns:
#' \describe{
#'   \item{taxon}{The name of the taxon.}
#'   \item{rank}{The rank of the taxon.}
#'   \item{antibiotic}{Common name of the antibiotic.}
#'   \item{value}{Whether or not species is resistant or sensitive to antibiotic.
#'   By default, NA values are removed and the most common value is shown for a given species
#'   if it has multiple strains.}
#' }
#' @details See `data-raw/download_from_bacdive.Rmd` for code to create this.
#' @note Data frame also has the attribute "date_downloaded" for when the data was downloaded from bacdive.org.
"bacdive_susceptibility"

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
