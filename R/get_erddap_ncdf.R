get_erddap_ncdf <- function(url, file, layer, df, date_col = "datecollected", lat_col = "latitude", lon_col = "longitude") {
  library(lubridate)
  
  #calculate the study extent. 
  minLat <- min(df[lat_col])
  maxLat <- max(df[lat_col])
  minLon <- min(df[lon_col])
  maxLon <- max(df[lon_col])
  
  #Calculate the date extent for our call to griddap. (Make sure to use double square brackets so we get a vector and not a tibble.)
  dates <- parse_date_time(df[[date_col]], orders = c('ymd', 'ymd HMS'), tz = "UTC")
  minDate <- min(dates)
  maxDate <- max(dates)
  
  grid <- griddap(
   file,
   latitude = c(minLat, maxLat),
   longitude = c(minLon, maxLon),
   time = c(minDate, minDate %m+% months(12)),
   fields = layer,
   url = url
  )
  
  return(grid)
}