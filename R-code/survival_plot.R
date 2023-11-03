#' @title survival_plot
#'
#' @description Outputs plot of survival probability versus age
#'
#' @param lifeTable Input must be data frame with columns ages and survival_prop
#'
#' @return step plot of survival probability versus age
#'
#' @export
#'
#' @import ggplot2

survival_plot <- function(lifeTable) {
  if(!is.data.frame(lifeTable)) {
    stop("Input must be data frame with columns 'age' and 'survival_prop'")
  }
  lifeTable$counter <- 1
  for(i in 2:length(lifeTable$age)) {
    lifeTable$counter[i] <- lifeTable$counter[i-1] + 1
  }
  if(length(lifeTable$counter) <= 20) {
    plot <- (ggplot(data = lifeTable, aes(x = counter, y = survival_prop))
           + geom_step() + theme_linedraw()
           + labs(x = "Age", y = "Survival", title = "Life Table Estimate of Survival by Age")
           + scale_x_continuous(breaks = lifeTable$counter, labels = lifeTable$age)
           + scale_y_continuous(labels = scales::percent, limits = c(0, 1))
           + theme(plot.title = element_text(hjust = 0.5)))
  } else {
    plot <- (ggplot(data = lifeTable, aes(x = counter, y = survival_prop))
             + geom_step() + theme_linedraw()
             + labs(x = "Age", y = "Survival", title = "Life Table Estimate of Survival by Age")
             + scale_x_continuous(n.breaks = 10)
             + scale_y_continuous(labels = scales::percent, limits = c(0, 1))
             + theme(plot.title = element_text(hjust = 0.5)))
  }
  return(plot)
}
