#' @title Create a table of cover for each species at each point
#'
#' @description For veg data, summarizes cover of each species at each point.
#'
#' @param lpi A dataframe object of lpi data from a veg survey
#' @param type Denotes type of cover to calculate"absolute" or "relative"
#'
#' @return A dataframe summary of cover of species
#'
#' @examples data = cover.summary(checklist, lpi)
#'
#' @export species.cover.table
#'
#'

species.cover.table = function(lpi, type = "absolute"){
  lpi$Tally = 1

  a = aggregate(lpi$Tally, list(lpi$pointyear), sum)
  names(a) = c("pointyear", "NumIndices")

  lpi.trim = subset(lpi, select=c("pointyear", "year", "Point.Id", "Canopy1", "Canopy2", "Canopy3",
                                  "Top.Layer","Lower1","Lower2", "Lower3", "Lower4","Lower5","Lower6","Lower7",
                                  "Lower8","Lower9","Lower10","Soil.Surface"))

  longlpi = melt(lpi.trim, id=c("pointyear", "Point.Id", "year"))
  # Note, this step may give a warning, but it's
  names(longlpi) = c("pointyear", "Point.Id", "year", "Layer", "Spp")

  longlpi = subset(longlpi, select=c("pointyear", "Point.Id", "year", "Spp"))
  longlpi$Tally = 1

  newlpi = dcast(longlpi, pointyear~Spp,value.var=c("Tally"), sum)
  newlpi = merge(newlpi, a, by="pointyear")
  pointyear = newlpi$pointyear
  firstdrops = c("Var.2", "NA", "NOPLANT", "M", "L", "EM", "AM", "R", "WL", "S")
  newlpi = newlpi[,!(names(newlpi) %in% firstdrops)]
  if(type == "absolute"){
    newlpi.relative = (newlpi[,3:ncol(newlpi)-1]/newlpi$NumIndices) *100
  } else if(type == "relative"){
    newlpi.relative = newlpi[,3:ncol(newlpi)-1]/rowSums(newlpi[,3:ncol(newlpi)-1]) * 100
  } else{ stop("Argument for type not recognized") }

  newlpi.relative = cbind(pointyear, newlpi.relative)

  ## Almost there, just some tidying

  drops = c("Var.2", "NA", "NumIndices", "NOPLANT", "UNKNWN", "M", "L", "EM", "AM", "R", "WL", "S")
  newlpi.relative = newlpi.relative[,!(names(newlpi.relative) %in% drops)]
  newlpi = newlpi[,!(names(newlpi) %in% drops)]

  return(newlpi.relative)



}
