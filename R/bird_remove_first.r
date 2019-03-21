#' @title Remove first row of data.frame if empty
#'
#' @description A reoccuring problem when importing data frames into rstudio is a blank first row that is added to the data frame. This function looks at the.
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
