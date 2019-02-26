#' @title Create a table of cover for functional groups at each point
#'
#' @description For veg data, summarizes cover of each functional group at each point.
#'
#' @param lpi A dataframe object of lpi data from a veg survey
#' @param type Type of cover to calculate - either "absolute" or "relative"
#' @param invasives Boolean that specifies whether to count invasive species as a separate functional group
#'
#' @return A dataframe summary of cover of functional groups
#'
#' @examples data = functional.cover.table(lpi, type = "absolute")
#'
#' @export functional.cover.table
#'
#'
#'


functional.cover.table = function(longlpi, type = "absolute", invasives = FALSE){

  library(reshape2)
  CAPlants = RMN:::CAPlantsv2
  Invasives = RMN:::Invasivesv1



  CAPlantsI<-CAPlants
  levels(CAPlantsI$FunGrp) <- c(levels(CAPlantsI$FunGrp), "Invasives")

  CAPlantsI$FunGrp[CAPlantsI$Accepted.Symbol %in% Invasives$USDA.code]<-"Invasives"



  longlpi$FunGrp = NULL
  if(isTRUE(invasives)){
    longlpi$FunGrp<-CAPlantsI$FunGrp[match(longlpi$Spp,CAPlantsI$Accepted.Symbol)]
  } else if(isFALSE(invasives)){
    longlpi$FunGrp = CAPlants$FunGrp[match(longlpi$Spp,CAPlants$Accepted.Symbol)]
  } else {stop("Argument for invasives not recognized")}


  Fun.Sum = aggregate(longlpi$Tally, by = list(longlpi$pointyear, longlpi$Point.Id, longlpi$year, longlpi$FunGrp), sum)
  names(Fun.Sum) = c("pointyear", "Point.Id", "year", "FunGrp", "Count")

  Fun.Sum2 = dcast(Fun.Sum, pointyear ~ FunGrp, value.var = c("Count"), sum)
  Fun.Sum2 = merge(Fun.Sum2, a, by = "pointyear")
  pointyears = as.data.frame(Fun.Sum[,1:3])
  pointyears = unique(pointyears)




  Fun.Sum3 = Fun.Sum2
  if(type == "absolute"){
    Fun.Sum3[,2:(ncol(Fun.Sum3) - 1)] = Fun.Sum3[,2:(ncol(Fun.Sum3) - 1)]/Fun.Sum3[,ncol(Fun.Sum3)] * 100
  } else if(type == "relative"){
    Fun.Sum3[,2:(ncol(Fun.Sum3) - 1)] = Fun.Sum3[,2:(ncol(Fun.Sum3) - 1)]/rowSums(Fun.Sum3[,2:(ncol(Fun.Sum3) - 1)], na.rm = TRUE) * 100
  } else {stop("Argument for type not recognized")}

  Fun.Sum3 = merge(pointyears, Fun.Sum3, by = "pointyear", all = FALSE)


  return(Fun.Sum3)

}
