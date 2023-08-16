##' @title Pull the Latest IMOS Moorings Dataset from the Server 
##' @description Uses a Web link to send a Web Feature Service (WFS) query directly to AODN geoserver
##' @param sensorType Character string containing name of mooring sensor to query. Can be "temperature" or "velocity"
##' 
##' @return dataframe containing the locations of IMOS moorings as csv tibble
##' 
##' @importFrom readr read_csv
##' @export

mooringTable <- function(sensorType = "temperature"){
  
  sensorType <- match.arg(sensorType, choices = c("temperature","velocity","salinity","oxygen"))
  if(!sensorType %in% c("temperature","velocity","salinity","oxygen")) 
    stop('Only "temperature", "velocity", "salinity" or "oxygen" can be provided as a valid sensor type')
  
  XY_url <- "http://geoserver-123.aodn.org.au/geoserver/imos/ows?service=WFS&version=1.1.0&request=GetFeature&typeName=imos:moorings_hourly_timeseries_map&outputFormat=csv"
    suppressMessages(
      XYdata <- read_csv(XY_url) 
    )
    XYdata2 <- XYdata[grepl(sensorType, XYdata$standard_names, fixed=TRUE),]
    return(XYdata2)
}

