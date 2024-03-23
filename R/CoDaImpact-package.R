#' @keywords internal
#' @importFrom stats coef nobs resid
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL


# ---- Data Documentation ---------------------------------------------------------------

# ... Rice yield ----
#' Data on the rice yields in the Vietnamese provinces
#'
#' @description
#' The data is presented in  Trinh et al. (2023) for studying the impact
#' of climate change on  rice production in Vietnam. \cr
#' It contains the following information:
#'   +  `PROVINCE`: a factor for the 63 provinces of Vietnam
#'   +  `REGION`: a factor with the 6 main regions
#'   +  `YEAR`: a numeric corresponding to the year
#'   +  `YIELD`: a numeric for the rice production in tons per hectare
#'   +  `PRECIPITATION`: a numeric for the annual precipitation in liters
#'   +  `TEMPERATURES`: a compositional variable represented as a matrix \cr
#'       whose columns correspond to the proportion of days in a year where the maximal
#'       temperature (in Celsius degrees) falls into one of the three categories:
#'       "LOW" (from -6, to 25.1), "MEDIUM" (from 25.1 to 35.4)  and "HIGH" (from 35.4 to 45).
#'
#' @references
#'   Thi-Huong Trinh, Michel Simioni, and Christine Thomas-Agnan,
#'   “Discrete and Smooth Scalar-on-Density Compositional Regression
#'   for Assessing the Impact of Climate Change on Rice Yield in Vietnam”,
#'   TSE Working Paper, n. 23-1410, February 2023.
#' @author Lukas Dargel, Christine Thomas-Agnan
#' @name rice_yields
#' @docType data
#' @keywords data
"rice_yields"

# ... Elections ----
#' Results of french departmental elections in 2015
#'
#' The data is used by Nguyen et. al (2020) and originally disseminated by the
#' French ministry (Ministère de l'Intérieur et des Outre-Mer).
#' Information about the population characteristics comes from the french
#' national statistics institute (INSEE).
#'
#' @details
#'
#'   + `left`, `right`, `extreme_right`: Vote shares during the election grouped into three blocks
#'   + `Age_1839`, `Age_4064`, `Age_65plus`: Share of the population falling into one of three age categories
#'   + `Educ_BeforeHighschool`, `Educ_Highschool`, `Educ_Higher`: Share of the population having completed a certain level of education.
#'   + `asset_owner_rate`: The proportion of people who own assets
#'   + `income_taxpayer_rate`: The proportion of people who pay income tax
#'   + `forgeigner_rate`: The proportion of foreigners
#'
#'
#' @source
#'   - https://www.data.gouv.fr/fr/datasets/elections-departementales-2015-resultats-par-bureaux-de-vote
#'   - https://www.insee.fr/fr/accueil
#' @references
#'   Nguyen THA, Laurent T, Thomas-Agnan C, Ruiz-Gazen A. Analyzing the impacts of socio-economic factors on French departmental elections with CoDa methods. J Appl Stat. 2020 Dec 9;49(5):1235-1251. doi: 10.1080/02664763.2020.1858274. PMID: 35707505; PMCID: PMC9041641.
#' @author Lukas Dargel, Christine Thomas-Agnan
#' @docType data
#' @keywords data
#' @name election
"election"


# ... Car market ----
#' French car market data
#'
#' This data set shows monthly data of the French car market between 2003 and 2015.
#' The market is divided into 5 main segments (A to E), according to the size of the vehicle chassis.
#' Morais et. al (2018) first used this data to compare compositional and Dirichlet models for market shares.
#'
#' @details
#'   + `SEG_`: Corresponds to the shares of sales in each of the five market
#'     segments A,B,C,D and E. Where A are the smallest cars and E the largest.
#'     The segmentation is explained in [Wikipedia](https://en.wikipedia.org/wiki/Car_classification).
#'   + `GDP`: GDP figures in millions at current prices
#'   + `HOUSEHOLD_EXPENDITURE`: total household expenditure in millions at previous years prices
#'   + `GAS_PRICE`: Corresponds to the gas price including VAT.
#'   + `SCRAPPING_SUBSIDY`: A dummy indicating periods where the French government provided subsidies for scrapping a car.
#'
#' @source
#'   + The figures for GDP and household expenditure are originally provided by the The National Institute of Statistics and Economic Studies (INSEE).
#'   + The gas prices are from the OECD.
#'   + The market share of each segment of come from a simulation by Renault.
#' @references
#'    Joanna Morais, Christine Thomas-Agnan & Michel Simioni (2018) Using compositional and Dirichlet models for market share regression, Journal of Applied Statistics, 45:9, 1670-1689, DOI: 10.1080/02664763.2017.1389864
#' @author Lukas Dargel, Christine Thomas-Agnan
#' @docType data
#' @keywords data
#' @name car_market
"car_market"


# ... Toulouse retails ----
#' Simulated retail data for nine shopping malls in the city of Toulouse
#'
#' This data set provides an example for the use of CoDa models in geomarketing applications.
#' The data is simulated, but realistic in the sense that the parameters used for the simulation
#' were estimated on a real, but confidential data set (Dargel and Thomas-Agnan 2024).
#'
#' @details
#'   + `ID_IRIS`: Identifies the geographic region.
#'   + `POP`: Population number within the region
#'   + `MEDIAN_INCOME`: The median income in the region
#'   + `dist_km`: Distances from the region to all nine shopping malls.
#'   + `visits`: The share of visitors that coming from the region and going to each of the malls.
#'   + `geometry`: The geometry (polygon) of the region
#'   + The "mall_locations" and the "simulation_parameters" are given as additional attributes.
#'
#' @source
#'   + The figures for `POP` and `MEDIAN_INCOME` come from the [French census data prided by INSEE](https://www.insee.fr/fr/information/4467366).
#'   + The polygon `geometry` is [provided by the IGN](https://geoservices.ign.fr/contoursiris).
#'   + The locations of the nine shopping malls around the city center are derived from online mapping services (Google Maps and OpenStreetMap).
#'   + The distances (`dist_km`) are derived from location information.
#'   + The number of shopping trips (`visits`) are simulated by the authors.
#' @references
#'     Lukas Dargel & Christine Thomas-Agnan (2024) “The link between multiplicative competitive interaction models and compositional data regression with a total”, Journal of Applied Statistics, DOI: 10.1080/02664763.2024.2329923
#' @author Lukas Dargel, Christine Thomas-Agnan
#' @docType data
#' @keywords data
#' @name toulouse_retail
"toulouse_retail"

# ---- Reexports from package compositions ---------------------------------------------
#' @importFrom compositions alr
#' @export
compositions::alr

#' @importFrom compositions alrInv
#' @export
compositions::alrInv

#' @importFrom compositions clr
#' @export
compositions::clr

#' @importFrom compositions clrInv
#' @export
compositions::clrInv

#' @importFrom compositions ilr
#' @export
compositions::ilr

#' @importFrom compositions ilrInv
#' @export
compositions::ilrInv
