% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bacPhene.R
\name{gramStain}
\alias{gramStain}
\title{Gets the gram-stain of the strain}
\usage{
gramStain(strainData, reference = 1)
}
\arguments{
\item{strainData}{A list from the function \code{\link{getStrainData}}}

\item{reference}{Sometimes strains have multiple references for the phenotype information,
this is where you can select the certain reference (or iterate through them)}
}
\value{
A character string "negative" or "positive"
}
\description{
Gets the gram-stain of the strain
}
\examples{
\dontrun{
strain_list <- getStrains(page = 1,
genus = 'Bacteroides',
species = 'xylanisolvens',
userpassword = paste0(user,':',passwd))
phenotypes_list <- getStrainData(strain_list,
selection=1,
userpassword = paste0(user,':',passwd))
gramStain(phenotypes_list)
}
}
