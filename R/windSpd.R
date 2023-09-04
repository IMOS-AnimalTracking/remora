#' @title Calculate wind speed
#' @description helper function to calculate wind speed
#'
#' @param u horizontal (u) wind speed 
#' @param v vertical (v) wind speed
#' @return Wind speed in meters per second
#' @keywords internal

windSpd <-function(u,v){
  sqrt(u^2+v^2)
}