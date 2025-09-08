#' @title Calculate wind direction
#' @description helper function to calculate wind direction
#'
#' @param u horizontal (u) wind speed 
#' @param v vertical (v) wind speed
#' @return Wind direction in degrees clockwise
#' @export

windDir <-function(u,v){
  (270-atan2(u,v)*180/pi)%%360 
}