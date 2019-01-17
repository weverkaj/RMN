#' @title Extrapolate Infiltration Time
#'
#' @description For infiltration tests that were stopped before completely finishing, this funtion uses the remaining depth (cm) and the run time (minutes) of the test to extrapolate the time of a complete infiltration.
#'
#' @param height
#' @param time
#'
#'
#' @return Extrapolated time of Infiltration
#'
#' @examples extrapolate_inf(1.5, 50)
#'
#' @export extrapolate_inf
#'
#'
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

extrapolate_inf = function(height, time = 45){
  inf = (2.5 - height)
  num = time * 450
  vol = (pi*57.76)*(2.5-height)
  time = num/vol
  minutes = trunc(time)
  seconds = (time - minutes) * 60
  return(paste(as.character(round(minutes)), ":", as.character(round(seconds)), sep = ""))}
