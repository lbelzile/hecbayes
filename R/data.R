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


#' Road traffic accidents and speed limits experiment on motorway in Sweden
#'
#' In this experiment, data were collected on the same days in two consecutive years, and
#' speed limits were sometimes enforced in either. The purpose of the analysis is to analyse
#' the effect of the traffic limits.
#'
#' @references Svensson, A. (1981) \emph{On a goodness-of-fit test for multiplicative Poisson models}. Annals of Statistics \bold{9}, 697–704.
#'
#' @docType data
#' @format a data frame in long format of 184 observations on the following variables
#' \describe{
#' \item{\code{accidents}}{integer; daily number of accident}
#' \item{\code{day}}{integer; day of the experiment, ranging from 1 to 92}
#' \item{\code{year}}{integer; year of the measurement, either 1961 or 1962}
#' \item{\code{limit}}{logical: was a speed limit enforced?}
#' }
"sweden"

#' Electoral participation
#'
#' @description Data from a Ipsos survey commissioned by FiveThirtyEight to study determinant of electoral participation during national elections. Data have been coupled with census registers and weighted to reflect the US population as a whole. For more details, consult the \href{https://raw.githubusercontent.com/fivethirtyeight/data/master/non-voters/README.md}{description} and the \href{https://github.com/fivethirtyeight/data/raw/master/non-voters/nonvoters_codebook.pdf}{survey questionaire}. Data were used in the article \href{https://projects.fivethirtyeight.com/non-voters-poll-2020-election/}{\emph{Why Millions of Americans Don't Vote}}.
#' @format A data frame with 5836 rows and 8 variables:
#' \describe{
#'   \item{\code{age}}{[integer] age of respondant}
#'   \item{\code{race}}{[factor] race, one of \code{black}, \code{hispanic}, \code{other/mixed} or \code{white}}
#'   \item{\code{gender}}{[factor] gender, either \code{male} or \code{female}}
#'   \item{\code{income}}{[factor] income category}
#'   \item{\code{vote}}{[ordered] frequency of vote in national election, either \code{rarely/never}, \code{sporadic} or \code{always}}
#'   \item{\code{educ}}{[factor] education level of respondent}
#'   \item{\code{weight}}{[double] sampling weight}
#'   \item{\code{party}}{[factor] party affiliation, either Democrat (\code{Dem}), Republican (\code{GOP}) or \code{other} for non-response, independent, third party or no preference}
#'}
#' @note Data shared under  Creative Commons Attribution 4.0 International License
#' @source FiveThirtyEight, `non-voters` data
"voting"


#' Population of largest American cities
#'
#' Size of cities above 200 000 inhabitants according to 2020 US census.
#'
#' @format A data frame with 113 rows and 2 variables:
#' \describe{
#'   \item{\code{city}}{[character] name of city}
#'   \item{\code{population}}{[integer] number of inhabitants}
#'}
#' @source Wikipedia, List of United States Cities by population. Accessed October 10th, 2023. \url{https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population}
"uscitypopn"


#' @title Combining probability forecasts
#' @description This data from Mislavsky and Gaertig Study 4 studied how participants combine expert forecasts under different settings. Participants were recruited on Amazon MTurk and provided with expert prediction about NFL games, which charts translating probabilities into verbal statements. Participants could be on the outcome using a slide. The outcome is whether the participants makes a prediction more extreme than the most bullish advisor.
#' @format A data frame with 1197 rows and 5 variables:
#' \describe{
#'   \item{\code{extreme}}{[integer] response variable, did participant make forecast more extreme than most extreme advisor? Either yes (\code{1}) or no (\code{0})}
#'   \item{\code{answer}}{[integer] participant answer, either \code{1} for first answer or \code{2} for second answer}
#'   \item{\code{stock}}{[factor] stock shown to participants}
#'   \item{\code{id}}{[factor] unique participant identifier}
#'   \item{\code{condition}}{[factor] experimental condition, one of \code{numeric}, \code{numeric with direction} or \code{verbal}} 
#'}
#' @source ResearchBox 62, \url{https://researchbox.org/62}
#' @references Mislavsky R, Gaertig C. (2022). \emph{Combining Probability Forecasts: 60% and 60% Is 60%, but Likely and Likely Is Very Likely}. Management Science. 68(\bold{1}):541-563, doi: \url{https://doi.org/10.1287/mnsc.2020.3902}
"xforecast"
