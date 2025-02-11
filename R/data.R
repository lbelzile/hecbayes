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
#' @description This data from Study 4 of Mislavsky and Gaertig studied how participants combine expert forecasts under different settings. Participants were recruited on Amazon MTurk and provided with expert prediction about NFL games, which charts translating probabilities into verbal statements. Participants could be on the outcome using a slide. The outcome is whether the participants makes a prediction more extreme than the most bullish advisor.
#' @format A data frame with 1197 rows and 5 variables:
#' \describe{
#'   \item{\code{extreme}}{[integer] response variable, did participant make forecast more extreme than most extreme advisor? Either yes (\code{1}) or no (\code{0})}
#'   \item{\code{answer}}{[integer] participant answer, either \code{1} for first answer or \code{2} for second answer}
#'   \item{\code{stock}}{[factor] stock shown to participants}
#'   \item{\code{id}}{[factor] unique participant identifier}
#'   \item{\code{condition}}{[factor] experimental condition, one of \code{numeric}, \code{numeric with direction} or \code{verbal}}
#'}
#' @source ResearchBox 62, \url{https://researchbox.org/62}
#' @references Mislavsky R, Gaertig C. (2022). Combining Probability Forecasts: 60% and 60% Is 60%, but Likely and Likely Is Very Likely. Management Science. 68 (\bold{1}), 541-563, doi: \url{https://doi.org/10.1287/mnsc.2020.3902}
"xforecast"


#' Florida's 2000 presidential election results
#'
#' This database contains demographic variables and the number
#' of votes for the major candidates, Bush and Gore, as well
#' as the ballots cast for Pat Buchanan for each of the 67 counties.
#'
#'
#' @source Smith, R.L. (2002). \emph{A statistical assessment of Buchanan's vote in Palm Beach County}, \bold{17}(4), 441-457.
#' @docType data
#' @format a data frame in long format of 67 observations on the following variables
#' \describe{
#' \item{\code{accidents}}{integer; daily number of accident}
#' \item{\code{county}}{name of county}
#' \item{\code{popn}}{population of the county in 1997}
#' \item{\code{white}}{percentage of whites in 1996}
#' \item{\code{black}}{percentage of blacks in 1996}
#' \item{\code{hisp}}{percentage of Hispanics in 1996}
#' \item{\code{geq65}}{percentage of the population aged 65 and above based on 1996 and 1997 population estimates}
#' \item{\code{highsc}}{percentage of the population with a high school degree (1990 Census data)}
#' \item{\code{coll}}{percentage of the population that are college graduates (1990 Census data)}
#' \item{\code{income}}{mean personal income in 1994}
#' \item{\code{buch}}{total ballots cast for Pat Buchanan (Reform)}
#' \item{\code{bush}}{total ballots cast for Georges W. Bush (GOP)}
#' \item{\code{gore}}{total ballots cast for Al Gore (Democrat)}
#' \item{\code{totmb}}{the total number of votes cast for the presidential election in each county, minus Buchanan votes}
#' }
"buchanan"



#' Waiting time for the Montreal metro
#'
#' Time (in seconds) from 17:59 until the departure of the next metro at the Universite de Montreal station
#' during week-days over three consecutive months
#'
#' @docType data
#' @format a numeric vector with 62 observations
#' \describe{
#' \item{\code{waiting}}{time (in seconds) before the next metro departure}
#' }
#' @source STM
"waiting"

#' @title Selling formats and impact on sales
#' @description This is a subset of the data from Experiment 1 of Duke and Amir (2023). The study investigated the impact of presenting customers with a sequential choice (first decide whether or not to buy, then pick quantity) as opposed to an integrated decision (choose not to buy, or one of different quantities) on sales and the number of items bought.
#'
#' @docType data
#' @format A data frame with 397 rows and 5 variables:
#' \describe{
#'   \item{\code{format}}{[factor] experimental condition, either \code{quantity-integrated} or \code{quantity-sequential}}
#'   \item{\code{purchased}}{[integer] binary variable indicating whether the respondant purchased the item (\code{1}) or not (\code{0})}
#'   \item{\code{amount}}{[integer] amount purchased}
#'   \item{\code{gender}}{[factor] gender of respondant, one of \code{female}, \code{male} or \code{other}}
#'   \item{\code{age}}{[integer] age of respondant}
#'}
#' @source Research Box 602, \url{https://researchbox.org/602}, licensed under CC BY 4.0
#' @references Duke, K.E. and O. Amir (2023). \emph{The Importance of Selling Formats: When Integrating Purchase and Quantity Decisions Increases Sales}, Marketing Science,42(1), 87-109. \doi{10.1287/mksc.2022.1364}
"sellingformat"

#' Montreal precipitation data
#'
#' Amount of precipitation (in mm) in Montreal YUL airport
#' weather station for 2020-2024, by day of the year
#'
#' @format A data frame with 1827 rows and 3 variables:
#' \describe{
#'   \item{\code{year}}{[integer] year of observation}
#'   \item{\code{day}}{[integer] day of the year}
#'   \item{\code{precip}}{[double] amount of precipitation (in mm)}
#'}
#' @source Environment Canada
"mtlprecip"


#' Rat tumor data
#'
#' Data from Tarone (1982), extracted from Table 5. It contains observations from
#' 70 experiments on animal carcinogenesis bioassay conducted with female F344 rats,
#' with data on sample sizes and records of the number of lung cancer tumors developed.
#'
#' @format A data frame with 70 rows and 2 variables:
#' \describe{
#'   \item{\code{counts}}{[integer] counts of tumors out of \code{n}}
#'   \item{\code{n}}{[integer] experiment sample size}
#'}
#' @references Tarone, R. E. (1982) The use of historical information in
#'   testing for a trend in proportions. \emph{Biometrics}, \strong{38},
#'   215-220. \doi{10.2307/2530304}
"rats"


#' Mid 21st Century Global Temperature Projection Data
#'
#' Indices of global temperature change from late 20th century (1970-1999)
#' to the end of the 21st century (2069-2098) based on data produced by the Fifth
#' Coupled Model Intercomparison Project (CMIP5).
#'
#' @format A data frame with 270 rows and 3 variables:
#' \describe{
#'   \item{\code{temp}}{[double] anomaly of 2069-2098 mean relative to
#'       the 1970-1999 mean}
#'   \item{\code{GCM}}{[factor] family of general circulation model provider}
#'   \item{\code{RCP}}{[double] representative concentration pathway}
#'  }
#' @source Extracted from package \code{bang}. The raw data from which the indices are calculated are monthly
#'   CMIP5 scenario runs for global surface air temperature from the KNMI Climate Explorer (\url{https://climexp.knmi.nl/})
#'   on 4/3/2015.
#' @references Northrop, P.J. and B.D. Hall (2024) bang: Bayesian Analysis, No Gibbs, R package, version 1.0.4, \doi{10.32614/CRAN.package.bang}
#' @references Northrop, P.J. and R.E. Chandler (2014). Quantifying
#'   Sources of Uncertainty in Projections of Future Climate.
#'   \emph{Journal of Climate}, \strong{27}, 8793-8808.
#'   \doi{10.1175/JCLI-D-14-00265.1}
#' @references Van Vuuren, D. P., Edmonds, J., Kainuma, M., Riahi, K.
#'   Thomson, A., Hibbard, K., Hurtt, G. C., Kram, T., Krey, V.,
#'   Lamarque, J.-F. (2011). The representative concentration pathways:
#'   an overview. \emph{Climatic change}, \strong{109}, 5-31.
#'   \doi{10.1007/s10584-011-0148-z}
"climatechange"
