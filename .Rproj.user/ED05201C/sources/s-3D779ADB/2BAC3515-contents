#' @title Carbon Plot
#'
#' @description Plots soil carbon as scatterplot. Points from ranch identified are black, and all others are grayed out. Points are labeled automatically unless specified with labels = FALSE
#'
#' @param data from output of soil.final.cleanup()
#' @param transect Character string of selected ranch code
#' @param labels = TRUE determines whether points from the chosen ranch are labeled on the plot
#' @param pointcolors = c("black", "gray") a vector that specifies colors of points on the plot
#' @param legend specifies whether to display a legend
#' @param legendnames specifies how points are named on the legend
#' @param box.padding numeric adjusts spacing of labels on the plot
#' @param xlab,ylab character strings for axis lables
#'
#'
#' @return plot of soil carbon data
#'
#' @examples carbon.plot(soil, "ranchname")
#'
#' @export carbon.plot
#'
#'
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


carbon.plot<-function(data, transect, year, labels = TRUE, pointcolors = c("black", "gray"),
                      legend = FALSE, legendnames = c(transect, "Others"), box.padding = 0.5, xlab = "% Carbon 0-10 cm", ylab = "% Carbon 10-40 cm"){

  library(ggplot2)
  library(ggrepel)
  library(dplyr)

  data = subset(data, data$YEAR %in% year)
  masked = data
  masked$Transect = as.character(replace(as.character(masked$Transect), masked$Transect != transect, values = "zzzz"))

  masked2 = masked[!is.na(masked$Carbon.0.10.cm) & !is.na(masked$Carbon.10.40.cm),]


  masked_soil = arrange(masked2, desc(Transect))



  if(isTRUE(labels)){
    labs = as.character(masked_soil$Point)
  } else {
    labs = NA
  }

  ggplot(masked_soil, aes(x = Carbon.10.40.cm, y = Carbon.0.10.cm, color = Transect)) +
    geom_point() +
    scale_color_manual(values = pointcolors, labels = legendnames, guide = legend) +
    theme_bw() +
    xlab(xlab) + ylab(ylab) +
    geom_label_repel(aes(label = ifelse(Transect == transect, labs, NA)), box.padding = box.padding)

}
