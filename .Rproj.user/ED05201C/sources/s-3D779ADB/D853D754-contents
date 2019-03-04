#' @title Functional cover change summary plot
#'
#' @description For veg data, summarizes change in mean cover of functional groups for a property
#'
#' @param lpi A dataframe object of lpi data from a veg survey
#'
#' @return A ggplot of functional cover change with error bars
#'
#' @examples data = functional.cover.plot(lpi, type = "absolute")
#'
#' @export functional.cover.table
#'
#'
#'
#'


functional.cover.change.plot = function(lpi,
                                        type = "absolute",
                                        xlab = "Functional Group",
                                        ylab = paste("Cover Change",
                                                     " ",
                                                     min(levels(abs$year)),
                                                     "-",
                                                     max(levels(abs$year)),
                                                     sep = ""))
  {

  library(ggplot2)

  abs = functional.cover.table(lpi, type = type, surveyyear = levels(as.factor(lpi$year)))
  abs$NumIndices = NULL
  abs = melt(abs, id = c("pointyear", "Point.Id", "year"))
  names(abs)<-c("pointyear", "Point.Id", "year", "Type", "Cover")

  abs$year = as.factor(abs$year)
  abs$Type = as.factor(abs$Type)

  d2 = subset(abs, subset = year == max(levels(year)))
  d1 = subset(abs, subset = year == min(levels(year)))

  coveryear = merge(d1, d2, by = c("Point.Id", "Type"))
  coveryear$change = coveryear$Cover.y - coveryear$Cover.x


  x = aggregate(coveryear, by = list(coveryear$Type), FUN = "mean")
  x = subset(x, select = c(Group.1, change))
  y = aggregate(coveryear, by = list(coveryear$Type), FUN = "se")
  y = subset(y, select = c(Group.1, change))
  colnames(x) = c("Functional_Group", "Percent_Change")
  colnames(y) = c("Functional_Group", "Standard_error")

  coverchange = merge(x, y, by = "Functional_Group")


  c = ggplot(coverchange, aes(x = Functional_Group, y = Percent_Change)) +
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_errorbar(ymin = coverchange$Percent_Change - coverchange$Standard_error,
                  ymax = coverchange$Percent_Change + coverchange$Standard_error, width = 0.5) +
    ylim((min(coverchange$Percent_Change) - max(coverchange$Standard_error)),
         (max(coverchange$Percent_Change) + max(coverchange$Standard_error))) +
    xlab(label = xlab) +
    ylab(label = ylab)

  return(c)


}
