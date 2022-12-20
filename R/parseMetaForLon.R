#Since different data sources can have different longitude formats (-180 to 180 vs 0 to 360), we have to determine which we want ahead of time.
#That way, we can correctly modify our dataframe so that when we get data, we can pass the correct longitude to the URL. 
#We can get this info from the metadata. 
#This function takes a dataframe containing a longitude column and a data source URL. It then uses the URL to grab the appropriate metadata, inferring
#a path using the getMetadataUrl function. It then checks the longitude boundaries in the dataframe and, if necessary mutates them to match the data source's
#expected format.

parseMetaForLon <- function(dataframe, url) {
  library(tidyverse)
  #Start by deriving the necessary URL.
  metaUrl <- getMetadataUrl(url)
  
  message(metaUrl)
  
  #Now load the metadata
  meta <- read_csv(metaUrl) %>% filter(`Variable Name` == "longitude" & `Attribute Name` == "actual_range")
  
  #And get the range of the longitude.
  lonRange = meta$Value[1]
  
  #Annoyingly, at this point our longitude range is a character string, rather than a set of int values. While we could do a string comparison, I feel
  #better making them ints first and then doing numeric comparison, since ultimately we're working with numeric data.
  lons = str_split(lonRange, ', ', simplify=TRUE)
  
  print(as.double(lons[1,1]))
  #If the low range of our tibble is 0, we know we're in 0-360, not -180-180. Since our data will always- as guaranteed by Jon- be -180 to 180, we modify
  #The data if we see it going to the appropriate range.
  if(as.double(lons[1,1]) == 0.0) {
    dataframe <- mutate(dataframe,
                        longitude_original = longitude,
                        longitude = (longitude + 360) %% 360)
  }
  
  #Now we either return the unmodified dataframe or the modified one, depending on whether or not we modified it.
  return(dataframe) 
}
