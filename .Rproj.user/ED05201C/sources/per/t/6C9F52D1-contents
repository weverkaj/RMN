#' @title Functional cover change summary plot
#'
#' @description For veg data, summarizes change in mean cover of functional groups for a property
#'
#' @param lpi A dataframe object of lpi data from a veg survey
#' @param transect Ranch for which to make plot
#' @param invasives Boolean whether to include invasives as a functional group
#' @param surveyyear The years for which to make the plot
#' @param type "absolute" or "relative" cover
#' @param xlab,ylab Axis Labels
#'
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
                                        transect,
                                        invasives = FALSE,
                                        surveyyear = levels(as.factor(lpi$year)),
                                        type = "absolute",
                                        xlab = "Functional Group",
                                        ylab = paste("Cover Change",
                                                      " ",
                                                      min(levels(surveyyear)),
                                                      "-",
                                                      max(levels(surveyyear)),
                                                      sep = "")){

  library(ggplot2)
  library(reshape2)


  coveryear = functional.cover.change.table(lpi = lpi,
                                            transect = transect,
                                            type = type,
                                            invasives = invasives,
                                            surveyyear = surveyyear,
                                            casted = FALSE
  )


  x = aggregate(coveryear, by = list(coveryear$Type), FUN = "mean")
  x = subset(x, select = c(Group.1, change))
  y = aggregate(coveryear, by = list(coveryear$Type), FUN = "se")
  y = subset(y, select = c(Group.1, change))
  colnames(x) = c("Functional_Group", "Percent_Change")
  colnames(y) = c("Functional_Group", "Standard_error")

  coverchange = merge(x, y, by = "Functional_Group")


  c = ggplot(coveryear, aes(x = Type, y = change)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab(label = xlab) +
    ylab(label = ylab) +
    geom_hline(yintercept = 0, linetype = 'dotted')

  return(c)


}
