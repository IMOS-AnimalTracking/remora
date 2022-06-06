#Generalized version of parseMetaForLon, which takes a variable name and a URL, and then infers the metadata URL for
#the ERDDAP server and gets the metadata associated with the provided variable. If the variable is just 'none', then it
#returns a dataframe containing all of the metadata. 

parseMetaForVar <- function(url, var="None") {
  #Start by deriving the necessary URL.
  metaUrl <- getMetadataUrl(url)
  
  message("Getting metadata from: ", metaUrl)
  
  #Now load the metadata
  meta <- read_csv(metaUrl)
  
  #If we've got a var name passed, we filter out only those things from the metadata associated with a particular
  #variable. 
  if(var != "None") {
    meta = filter(meta, `Variable Name` == var)
  }
  
  #Either way, at the end we return Meta. 
  return(meta) 
}
