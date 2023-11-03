#' @title life_table
#'
#' @description Calculates and outputs life table
#'
#' @param age Vector of age categories
#' @param population Vector of population counts for each age category
#' @param deaths Vector of death counts for each age category
#'
#' @return Data frame of life table consisting of the following columns: age, population, deaths, cpd: conditional probability of death, cps: conditional probability of survival, survival_prop: survival proportion, person_years, total_years: total person years, life_expectancy
#'
#' @example
#' samAges <- c(0, 1, 2, 3, 4, 5)
#' samPop <- c(100, 100, 100, 100, 100, 100)
#' samDeaths <- c(20, 30, 50, 120, 170, 210)
#' life_table(samAges, samPop, samDeaths)
#'
#' @export
#'

life_table <- function(age, population, deaths) {
  stopifnot(is.vector(age))
  stopifnot(is.vector(population))
  stopifnot(is.vector(deaths))
  if(length(age) != length(population)) {
    stop("Number of age categories for ages and population do not match")
  } else if(length(age) != length(deaths)) {
    stop("Number of age categories for ages and deaths do not match")
  } else if(length(population) != length(deaths)) {
    stop("Number of age categories for population and deaths do not match")
  }
  df <- data.frame("age" = age, "population" = population, "deaths" = deaths)
  df$cpd <- df$deaths / (df$population + df$deaths)
  df$cps <- 1 - df$cpd
  df$survival <- 100000
  for(i in 2:length(df$survival)) {
    df$survival[i] <- df$cps[i-1] * df$survival[i-1]
  }
  df$survival_prop <- df$survival / 100000
  for(i in 1:length(df$survival)) {
    df$person_years[i] <- (df$survival[i] + df$survival[i+1]) / 2
  }
  df$person_years[length(df$person_years)] <- df$survival[length(df$survival)]
  df$total_years <- sum(df$person_years)
  year_dif <- 0
  for(i in 2:length(df$person_years)) {
    year_dif <- year_dif + df$person_years[i-1]
    df$total_years[i] <- sum(df$person_years) - year_dif
  }
  df$life_expectancy <- df$total_years / df$survival
  return(df)
}
