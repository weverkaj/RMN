#' @title Functional cover summary plot
#'
#' @description For veg data, summarizes and plots mean cover of functional groups for a property
#'
#' @param lpi A dataframe object of lpi data from a veg survey
#' @param transect Ranch for which to make plot
#' @param type "absolute" or "relative" cover
#' @param surveyyear The year for which to calculate cover
#' @param xlab,ylab Axis labels
#'
#' @return A ggplot of functional cover with error bars
#'
#' @examples data = functional.cover.plot(lpi, type = "absolute")
#'
#' @export functional.cover.table
#'
#'
#'
#'


functional.cover.plot = function(lpi,
                                 type = "absolute",
                                 transect,
                                 invasives = FALSE,
                                 surveyyear = max(levels(as.factor(lpi$year))),
                                 xlab = "Functional Group",
                                 ylab = "Percent Cover"){
  library(ggplot2)

  cov = functional.cover.table(lpi, type = type, transect = transect, surveyyear = surveyyear, invasives = invasives)
  cov$NumIndices = NULL
  cov = melt(cov, id = c("pointyear", "Point.Id", "year"))
  names(cov)<-c("pointyear", "Point.Id", "year", "Type", "Cover")
  d = cov

  d$year = as.factor(d$year)
  d$Type = as.factor(d$Type)

  d2 = subset(d, subset = year == max(levels(year)))

  cover_plot = ggplot(d2, aes(x = Type, y = Cover)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab(xlab) +
    ylab(ylab)

  return(cover_plot)

}
