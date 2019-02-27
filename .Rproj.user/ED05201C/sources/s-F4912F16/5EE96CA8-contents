#' @title Functional cover summary plot
#'
#' @description For veg data, summarizes and plots mean cover of functional groups for a property
#'
#' @param lpi A dataframe object of lpi data from a veg survey\
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
                                 xlab = "Functional Group",
                                 ylab = "Percent Cover"){

  library(ggplot2)

  abs = functional.cover.table(lpi, type = type)
  abs$NumIndices = NULL
  abs = melt(abs, id = c("pointyear", "Point.Id", "year"))
  names(abs)<-c("pointyear", "Point.Id", "year", "Type", "Cover")
  d = abs

  d$year = as.factor(d$year)
  d$Type = as.factor(d$Type)

  d2 = subset(d, subset = year == max(levels(year)))
  d1 = subset(d, subset = year == min(levels(year)))

  coveryear = merge(d1, d2, by = c("Point.Id", "Type"))
  coveryear$change = coveryear$Cover.y - coveryear$Cover.x


  a = aggregate(coveryear, by = list(coveryear$Type), FUN = "mean")
  a = subset(a, select = c("Group.1", "Cover.y"))
  colnames(a) = c("Functional_Group", "Percent_Cover")
  b = aggregate(coveryear, by = list(coveryear$Type), FUN = "se")
  b = subset(b, select = c("Group.1", "Cover.y"))
  colnames(b) = c("Functional_Group", "Standard_error")

  current_cover = merge(a, b, by = "Functional_Group")

  cover_plot = ggplot(current_cover, aes(x = Functional_Group, y = Percent_Cover)) +
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_errorbar(ymin = (current_cover$Percent_Cover - current_cover$Standard_error),
                  ymax = (current_cover$Percent_Cover + current_cover$Standard_error),
                  width = 0.5) +
    ylim(0, (max(current_cover$Percent_Cover) +
               max(current_cover$Standard_error))) +
    xlab(xlab) +
    ylab(ylab)

  return(cover_plot)

}
