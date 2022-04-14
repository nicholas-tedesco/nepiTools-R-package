#' @title directAA
#'
#' @description Calculates age-adjusted mortality rate using the direct method
#'
#' @param deaths Vector of death counts for each age category
#' @param population Vector of population counts for each age category
#' @param refPopulation Vector of standard population counts for each age category
#' @param perPopulation Optional input for "per population" that applies to rate (default: per 1000 members of population)
#' @param decimal Optional input for rounding rate to certain decimal (default: two decimals)
#'
#' @return Character string including age-adjusted mortality rate, method of adjustment (direct), and per population
#'
#' @example
#' sampleDeaths <- c(39745, 26372, 37125, 33679, 64386)
#' samplePop <- c(17724000, 5390000, 1210000, 364000, 199000)
#' sampleRef <- c(21151000, 28473000, 19298000, 5864000, 2530000)
#' directAA(sampleDeaths, samplePop, sampleRef)
#'
#' @export
#'

directAA <- function(deaths, population, refPopulation, perPopulation, decimal) {
  stopifnot(is.vector(deaths))
  stopifnot(is.vector(population))
  stopifnot(is.vector(refPopulation))
  if(length(deaths) != length(population)) {
    stop("Number of age categories for study deaths and study population do not match")
  } else if(length(deaths) != length(refPopulation) | length(population) != length(refPopulation)) {
    stop("Number of age categories for study deaths/population and reference data do not match")
  }
  rates <- deaths / population
  ref.deaths <- rates * refPopulation
  DAR.0 <- sum(ref.deaths) / sum(refPopulation)
  if(missing(perPopulation)) {
    perPopulation <- 1000
    DAR.1 <- DAR.0 * perPopulation
  } else {
    DAR.1 <- DAR.0 * perPopulation
  }
  if(missing(decimal)) {
    DAR <- round(DAR.1, 2)
  } else {
    DAR <- round(DAR.1, decimal)
  }
  age_specific <- deaths / population
  list(DAR = paste(paste(paste("Direct age-adjusted mortality rate (per", perPopulation), "):", sep = ""), DAR), AS = age_specific)
}
