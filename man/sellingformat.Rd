% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{sellingformat}
\alias{sellingformat}
\title{Selling formats and impact on sales}
\format{
A data frame with 397 rows and 5 variables:
\describe{
  \item{\code{format}}{[factor] experimental condition, either \code{quantity-integrated} or \code{quantity-sequential}}
  \item{\code{purchased}}{[integer] binary variable indicating whether the respondant purchased the item (\code{1}) or not (\code{0})}
  \item{\code{amount}}{[integer] amount purchased}
  \item{\code{gender}}{[factor] gender of respondant, one of \code{female}, \code{male} or \code{other}}
  \item{\code{age}}{[integer] age of respondant}
}
}
\source{
Research Box 602, \url{https://researchbox.org/602}, licensed under CC BY 4.0
}
\usage{
sellingformat
}
\description{
This is a subset of the data from Experiment 1 of Duke and Amir (2023). The study investigated the impact of presenting customers with a sequential choice (first decide whether or not to buy, then pick quantity) as opposed to an integrated decision (choose not to buy, or one of different quantities) on sales and the number of items bought.
}
\references{
Duke, K.E. and O. Amir (2023). \emph{The Importance of Selling Formats: When Integrating Purchase and Quantity Decisions Increases Sales}, Marketing Science,42(1), 87-109. \doi{10.1287/mksc.2022.1364}
}
\keyword{datasets}
