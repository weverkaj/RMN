#' @title Functional cover change summary plot
#'
#' @description For veg data, summarizes change in mean cover of functional groups for a property
#'
#' @param lpi A dataframe object of lpi data from a veg survey
#' @param transect Ranch for which to make plot
#' @param invasives Boolean whether to include invasives as a functional group
#' @param surveyyear The years for which to make the plot
#' @param background = TRUE whether to display "background" data on plot not from selected ranch
#' @param type "absolute" or "relative" cover
#' @param xlab,ylab Axis Labels
#' @param legendnames specifies how points are named on the legend
#' @param legendtitle Character string of legend title
#' @param boxcolors vector of colors to display data on the boxplot
#'
#'
#' @return A ggplot of functional cover change with error bars
#'
#' @examples data = functional.cover.plot(lpi, type = "absolute")
#'
#' @export functional.cover.change.plot
#'
#'
#'
#'


functional.cover.change.plot = function(lpi,
               transect,
               invasives = FALSE,
               surveyyear = levels(as.factor(lpi$year)),
               background = TRUE,
               type = "absolute",
               xlab = "Functional Group",
               ylab = paste("Cover Change",
                            " ",
                            min(levels(as.factor(surveyyear))),
                            "-",
                            max(levels(as.factor(surveyyear))),
                            sep = ""),
               legendtitle = "Ranch",
               legendnames = c(paste(transect, collapse = " "), "Others"),
               boxcolors = c("black","gray")
){

  library(ggplot2)
  library(reshape2)


  coveryear = functional.cover.change.table(lpi = lpi,
                                            transect = levels(lpi$Transect.Name),
                                            type = type,
                                            invasives = invasives,
                                            surveyyear = surveyyear,
                                            casted = FALSE
  )
  masked = coveryear
  if(!background){masked = subset(masked, subset = Transect.x %in% transect)}

  masked$Transect.x = as.character(replace(as.character(masked$Transect.x),
                                           masked$Transect.x != transect, values = "zzzz"))
  masked$Transect.x = as.character(replace(as.character(masked$Transect.x),
                                           masked$Transect.x == transect, values = "aaaa"))
  if(!("aaaa" %in% masked$Transect.x)){stop("This ranch does not contain multiple surveys in the years selected")}


  # x = aggregate(masked, by = list(masked$Transect.x, coveryear$Type), FUN = "mean")
  # x = subset(x, select = c(Group.1, Group.2, change))
  # y = aggregate(coveryear, by = list(masked$Transect.x, coveryear$Type), FUN = "se")
  # y = subset(y, select = c(Group.1, Group.2, change))
  # colnames(x) = c("Transect", "Functional_Group", "Percent_Change")
  # colnames(y) = c("Transect", "Functional_Group", "Standard_error")
  #
  # coverchange = merge(x, y, by = c("Transect", "Functional_Group"))


  c = ggplot(masked, aes(x = Type, y = change, color = Transect.x)) +
    geom_boxplot() +
    scale_color_manual(name = legendtitle, values = boxcolors, labels = legendnames) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab(label = xlab) +
    ylab(label = ylab) +
    geom_hline(yintercept = 0, linetype = 'dotted')

  return(c)


}
