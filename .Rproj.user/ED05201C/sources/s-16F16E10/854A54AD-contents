#' @title Carbon Plot
#'
#' @description Plots soil carbon
#'
#' @param data
#' @param transect
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


carbon.plot<-function(data,transect){

  data1 = subset(data,data$Transect==transect)
  data2 = subset(data,data$Transect!=transect)
  #data2 = data2[!is.na(data2$survey2),]

  plot(data$Carbon.10.40.survey2, data$Carbon.0.10.survey2, cex=0,
       ylab = "% organic carbon 0-10 cm", xlab="% organic carbon 10-40 cm", main=paste("Soil carbon - ",transect))

  points(data2$Carbon.10.40.survey2, data2$Carbon.0.10.survey2, pch=16, col="gray")
  points(data1$Carbon.10.40.survey2, data1$Carbon.0.10.survey2, pch=16, col="black")


}
