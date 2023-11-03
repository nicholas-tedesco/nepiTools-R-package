#' @title median_survival
#'
#' @description Calculates median survival time
#'
#' @param lifeTable Input must be data frame with columns ages and survival_prop
#'
#' @return Median survival time (in units of age groups that data is input in)
#'
#' @export
#'
#' @import dplyr

median_survival <- function(lifeTable) {
  if(!is.data.frame(lifeTable)) {
    stop("Input must be data frame with columns 'age' and 'survival_prop'")
  }
  df = lifeTable$age[min(which(lifeTable$survival_prop <= 0.5))]
  return(df)
}
