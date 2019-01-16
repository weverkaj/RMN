#' @title Make a table to summarize data
#'
#' @description From output of prepare.soil.triangle() creates a soil texture plot and compares a ranch to other ranches in the dataset
#'
#' @param data
#' @param transect
#'
#' @return Shows soil data in a nice-looking table
#'
#' @examples data.sum(soil, "ranchname")
#'
#'
#' @export data.sum
#'
#' @return data summary table
#'


data.sum<-function(data,transect){
  #data<-soil
  #transect="BOPO"
  data1<-subset(data,data$Transect==transect)
  data2<-subset(data1,select=c("Point","Carbon.0.10.2018", "Carbon.10.40.2018", "Bulk.Density.2018", "Infilt1.2018","Carbon010change","Carbon1040change","Bulk.density.change"))
  names(data2) = c("Point","Carbon\n2018\n0-10cm", "Carbon\n2018\n10-40cm", "Bulk\nDensity\n2018", "Infiltration\n2018","Carbon\nChange\n0-10cm","Carbon\nChange\n10-40cm","Bulk\nDensity\nchange")
  data2[,2:8] = round(data2[,2:8], 2)
  grid.table(data2,theme= ttheme_default(base_size=10))
}
