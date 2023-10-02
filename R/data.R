#' Sesame street story headline
#'
#' Data for impressions on different headlines for a story on
#' Sesame street by Upworthy (id 546f9c5c92f391a45e00003e).
#'
#' @format A data frame with 4 rows and 3 variables:
#' \describe{
#'   \item{\code{headline}}{[factor] headline of story, one of \code{H1}, \code{H2}, \code{H3} or \code{H4}}
#'   \item{\code{impressions}}{[integer] number of views}
#'   \item{\code{clicks}}{[integer] number of clicks}
#'}
"upworthy_sesame"


#' Upworthy headlines with questions
#'
#' Aggregated data from the Upworthy archive. Each observation represents the counts and impressions for a story that contained headlines with both questions or not, detected by searching for a question mark character. Only stories with both are preserved, so data are balanced
#'
#' @format A data frame with 10590 rows and 4 variables:
#' \describe{
#'   \item{\code{id}}{[factor] dummy identifier for questions}
#'   \item{\code{question}}{[factor] does headline contain a question, either \code{"yes"} or \code{"no"}}
#'   \item{\code{impressions}}{[integer] number of views}
#'   \item{\code{clicks}}{[integer] number of clicks}
#'}
"upworthy_question"


#' Distractions from smartwatches while driving
#'
#' Experiment from Tech3Lab using a driving simulator to study
#' the impact of various distraction on attention and performance.
#' The experimental conditions were compared in Table 3 of Brodeur et al. (2021)
#' using Wilcoxon's signed rank test.
#' @format A data frame with 124 rows and 3 variables:
#' \describe{
#'   \item{\code{id}}{[factor] participant identifier}
#'   \item{\code{task}}{[factor] type of distraction}
#'   \item{\code{nviolation}}{[integer] total number of violations}
#'}
#' @source Shang Lin Chen, personal communication, distributed under CC BY-NC-SA 4.0
#' @references Brodeur, M., Ruer, P. Léger, P. and S. Sénécal (2021).Smartwatches are more distracting than mobile phones while driving: Results from an experimental study, \emph{Accident Analysis & Prevention}, 149, 1-9.
"smartwatch"

#' Euro-American dollar exchange rate
#'
#' Irregular time series of official exchange rate
#' from the European Central Bank to American dollar from 1999 until April 2023.
#' Week-ends and official holidays are excluded.
#'
#' @format A data frame with 6335 rows and 2 variables:
#' \describe{
#'   \item{\code{date}}{[Date] date of measurement}
#'   \item{\code{rate}}{[double] exchange rate EUR/USD}
#'}
#' @docType data
#' @source European Central Bank’s Statistical Data Warehouse, accessed September 25th, 2023
"exchangerate"
