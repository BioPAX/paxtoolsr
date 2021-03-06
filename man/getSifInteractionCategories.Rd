% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSifInteractionCategories.R
\name{getSifInteractionCategories}
\alias{getSifInteractionCategories}
\title{Get a list of categories of SIF interactions}
\usage{
getSifInteractionCategories()
}
\value{
a list of interactions in categories
}
\description{
Get a list of categories of SIF interactions
}
\details{
Description of interaction types: http://www.pathwaycommons.org/pc2/formats
Categories provided: 
  BetweenProteins, 
  BetweenProteinsOther (often from high-throughput experiments),
  BetweenProteinSmallMolecule, 
  BetweenSmallMolecules,
  SignedInteractions
}
\examples{
sifCat <- getSifInteractionCategories()
sifCat[["BetweenProteins"]]

}
\concept{paxtoolsr}
