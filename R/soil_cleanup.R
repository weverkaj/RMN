#' @title Add date, year, and survey ID columns
#'
#' @description For soil data, adds columns with date formatted as a date object in R, year as a numeric object, and Survey as a character object that inludes the point name and the survey year
#' @description Also adds columns that display BD and Infiltration diameter, height, and volume, and one that converts infiltration times to minutes shown as a base-10 decimal numeric
#' @description Interprets date formate automatically with read.date()
#'
#' @param data
#' @param timeformat
#' @param inf_diameter
#' @param bd_diameter
#' @param volume
#' @param bd_height
#' @param date_string
#'
#'
#' @return data frame with added columns
#'
#' @examples data = add_datetime(data)
#' @examples x = add_datetime(soildata)
#'
#' @export add.soilcolumns
#' @export read.date
#'
#'
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'




add.soilcolumns = function(data, timeformat = "mm/dd/yyyy", inf_diameter = 15.2, bd_diameter = 5.2, volume = 450, bd_height = 7.5){

  data1 = data

  #interpret date, isolate year as column
  data1$DATE = read.date(as.character(data1$Date))
  data1$YEAR = year(data1$DATE)
  data1$SURVEY = as.factor(paste(data1$Point.Name,"-",data1$YEAR,sep=""))


  ## add some simple data about our measurements
  data1$Ring.Infiltrometer.Diameter<-inf_diameter
  data1$Bulk.Density.Diameter<-bd_diameter
  data1$Water.Volume<-volume
  data1$Bulk.Density.Height<-bd_height
  data1$Total.Volume<-pi*((data1$Bulk.Density.Diameter/2)^2)*data1$Bulk.Density.Height
  data1$Bulk.Density<-(data1$Bulk.Density.Dry.Wt/(data1$Total.Volume-data1$Bulk.Density.Rock.Vol))
  data1[,"Bulk.Density"][data1[,"Bulk.Density"] <= 0] <- NA

  #convert water infiltration time to minutes in base-ten decimal
  data1[,"Water.Infiltration.Time.1"][data1[,as.character("Water.Infiltration.Time.1")] == ""] <- "00:00:00"
  data1$ISec1<-NULL
  data1$ISec1<-toSeconds(as.character(data1$Water.Infiltration.Time.1))
  data1$ISec1<-data1$ISec1/60



  return(data1)

}


read.date = function(date_string){

  if(grepl("/", date_string, )){
    splitter = "/"
  } else if(grepl( "-", date_string)){
    splitter = "-"
  } else {stop("Date format not recognized")}



  date = as.character(date_string)
  x = strsplit(date, splitter)
  x = unlist(x)


  if(nchar(x[1]) == 2){
    date_format = paste("%m", "%d", "%Y", sep = splitter)
  } else if(nchar(x[1]) == 4){
    date_format = paste("%Y", "%m", "%d", sep = splitter)
  } else {stop("Date format not recognized")}

  the_date = as.Date(date_string, format = date_format)

  return(the_date)

}
