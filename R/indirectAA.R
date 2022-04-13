#' @title indirectAA
#'
#' @description Calculates age-adjusted mortality rate using the indirect method
#'
#' @param deaths Vector of death counts for each age category
#' @param population Vector of population counts for each age category
#' @param refDeathRates Vector of standard population death rates for each age category
#' @param refMortality Single integer describing overall mortality in reference population
#' @param perPopulation Optional input for "per population" that applies to rate (default: per 1000 members of population)
#' @param decimal Optional input for rounding rate to certain decimal (default: two decimals)
#'
#' @return Character string including age-adjusted mortality rate, method of adjustment (indirect), and per population
#'
#' @example
#' testDeaths <- c(10, 20, 22, 98, 174, 112)
#' testPop <- c(74598, 85077, 80845, 148870, 102649, 42494)
#' testRefRates <- c(12.26, 16.12, 21.54, 33.96, 56.82, 75.23) / 100000
#' testRefCMR <- 10.9/1000
#' indirectAA(testDeaths, testPop, testRefRates, testRefCMR)
#'
#' @export
#'

indirectAA <- function(deaths, population, refDeathRates, refMortality, perPopulation, decimal) {
  stopifnot(is.vector(deaths))
  stopifnot(is.vector(population))
  stopifnot(is.vector(refDeathRates))
  if(length(deaths) != length(population)) {
    stop("Number of age categories for study deaths and study population do not match")
  } else if(length(population) != length(refDeathRates)) {
    stop("Number of age categories for study deaths/population and reference death rates do not match")
  }
  expDeaths <- population * refDeathRates
  SMR <- sum(deaths) / sum(expDeaths)
  IAR.0 <- SMR * refMortality
  if(missing(perPopulation)) {
    perPopulation <- 1000
    IAR.1 <- IAR.0 * perPopulation
  } else {
    IAR.1 <- IAR.0 * perPopulation
  }
  if(missing(decimal)) {
    IAR <- round(IAR.1, 2)
  } else {
    IAR <- round(IAR.1, decimal)
  }
  return(paste(paste(paste("Indirect age-adjusted mortality rate (per", perPopulation), "):", sep = ""), IAR))
}
