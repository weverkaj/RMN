#' @title Remove first row if empty
#'
#' @description For veg data, summarizes and plots mean cover of functional groups for a property
#'
#' @param df A dataframe object
#'
#' @return A ggplot of functional cover with error bars
#'
#' @examples data = functional.cover.plot(lpi, type = "absolute")
#'
#' @export remove_first
#'
#'
#'
#'



remove_first<-function(df){
  if(df[1,1]=="")
  {df<-df[-1,]
  rownames(df)<-1:nrow(df)
  return(df)
  } else { df<-df}}
