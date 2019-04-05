#' @title Tidy Column Names of data frame
#'
#' @description For any data frame, changes column names if they are not unique or tidy.
#'
#' @param df A data frame object
#'
#' @return A data frame with tidy names
#'
#' @examples data = all_Tidynames(df)
#'
#' @export all_Tidynames
#'
#'
#'
#'
all_Tidynames<-function(df){
  tidy.name.vector <- make.names(names(df), unique=TRUE)
  names(df)<-tidy.name.vector
  return(df)
}
