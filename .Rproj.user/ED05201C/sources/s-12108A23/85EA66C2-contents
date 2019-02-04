#' @title Prepare soil data to make figures
#'
#' @description For a dataframe output from function pair.data(), produces a table that shows 2018 data and amount of change.
#' @description This table is designed to be the source data for our functions that make figures
#'
#' @param paired_data
#'
#' @return Data frame with soil data
#'
#' @examples soil.final.cleanup(soildata)
#'
#' @export soil.final.cleanup
#'
#'
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

soil.final.cleanup = function(paired_data){

  soil<-subset(paired_data,select=c("Point","Transect","Carbon.0.10.survey2", "Carbon.10.40.survey2","Bulk.Density.survey2","Infilt1.survey2","Clay.10.40.cm.survey2", "Sand.10.40.cm.survey2", "Silt.10.40.cm.survey2","carbon.0.10.per_delta","carbon.10.40.per_delta","Infilt.per_delta","Bulk.Density.per_delta"))
  names(soil)<-c("Point","Transect","Carbon.0.10.survey2","Carbon.10.40.survey2","Bulk.Density.survey2","Infilt1.survey2","Clay.10.40.cm.survey2", "Sand.10.40.cm.survey2", "Silt.10.40.cm.survey2","Carbon010change","Carbon1040change","Infiltration.change","Bulk.density.change")
  soil$Location<-str_sub(soil$Point, -2)
  soil$CLAY<-soil$Clay.10.40.cm.survey2
  soil$SILT<-soil$Silt.10.40.cm.survey2
  soil$SAND<-soil$Sand.10.40.cm.survey2

  return(soil)

}
