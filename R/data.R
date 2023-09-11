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
