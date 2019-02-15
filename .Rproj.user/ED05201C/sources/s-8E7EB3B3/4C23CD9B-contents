#' @title Carbon Plot
#'
#' @description Plots soil carbon as scatterplot. Points from ranch identified are black, and all others are grayed out. Points are labeled automatically unless specified with labels = FALSE
#'
#' @param data
#' @param transect
#' @param labels
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


carbon.plot<-function(data,transect, labels = TRUE){

  library(ggplot2)
  library(ggrepel)

  masked = data
  masked$Transect = as.character(replace(as.character(soil$Transect), soil$Transect != transect, values = "zzzz"))
  masked = masked %>% arrange(desc(Transect))

  if(isTRUE(labels)){
    l = geom_label_repel(aes(label = ifelse(Transect == transect, as.character(masked$Point), NA)), box.padding = 0.5)
  } else {
    l = geom_label_repel(aes(label = ifelse(Transect == transect, NA, NA)), box.padding = 0.5)
  }

  ggplot(masked %>% arrange(desc(Transect)), aes(x = Carbon.10.40.survey, y = Carbon.0.10.survey, color = Transect)) +
    geom_point() +
    scale_color_manual(values = c("black", "gray"), labels = c(transect, "Others"), guide = FALSE) +
    theme_bw() +
    xlab("% Carbon 0-10 cm") + ylab("% Carbon 10-40 cm") +
    l



}
