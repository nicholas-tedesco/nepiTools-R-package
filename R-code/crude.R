#' @title crude
#'
#' @description Calculates crude mortality rate
#'
#' @param deaths Vector of death counts for each age category
#' @param population Vector of population counts for each age category
#' @param perPopulation Optional input for "per population" that applies to rate (default: per 1000 members of population)
#' @param decimal Optional input for rounding rate to certain decimal (default: two decimals)
#'
#' @return Character string including crude mortality rate and per population
#'
#' @example
#' sampleDeaths <- c(39745, 26372, 37125, 33679, 64386)
#' samplePop <- c(17724000, 5390000, 1210000, 364000, 199000)
#' crude(sampleDeaths, samplePop)
#'
#' @export
#'

crude <- function(deaths, population, perPopulation, decimal) {
  stopifnot(is.vector(deaths))
  stopifnot(is.vector(population))
  if(length(deaths) != length(population)) {
    stop("Number of age categories for study deaths and study population do not match")
  }
  CMR.0 <- sum(deaths) / sum(population)
  if(missing(perPopulation)) {
    perPopulation <- 1000
    CMR.1 <- CMR.0 * perPopulation
  } else {
    CMR.1 <- CMR.0 * perPopulation
  }
  if(missing(decimal)) {
    CMR <- round(CMR.1, 2)
  } else {
    CMR <- round(CMR.1, decimal)
  }
  return(paste(paste(paste("Crude mortality rate (per", perPopulation), "):", sep = ""), CMR))
}
