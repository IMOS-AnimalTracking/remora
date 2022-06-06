#Helper function to, given a URL, grab the requisite metadata. 

getMetadataUrl <- function(url, type='fgdc') {
  #Start by stripping any query string off the URL. 
  url = sub("\\?.+", "", url)
  
  #Now break up the URL so we can reassemble it later.
  splits = str_split(url, '/', simplify=TRUE)
  
  #Slightly ugly, but since we can't always depend on the user to give us a URL with a protocol on it, we're just going to blank these out
  #And then re-add them below ourselves. Ideally, this will give us a vector that has our URL, plus all our paths and ultimately the filename. We'll use
  #these to derive a metadata url. 
  parts = splits[!splits %in% c('http:', 'https:', "")]
  
  #Start out, this gives us just the URL. 
  metaUrl <- paste("https://", parts[1], '/', sep="")
  
  #Making this an if check so we can expand it with other things that aren't erddap in the future.
  if("erddap" %in% parts)
  {
    metaUrl <- paste(metaUrl, "erddap/info/", sep="")
  }
  
  #Now that we have all that, we can infer the rest of the URL from the filepath. It's the last element of the URL. 
  file <- tail(parts, n=1)
  
  #Strip off the file type extension.
  file <- tools::file_path_sans_ext(file)
  
  #Bolt on the filename, the type, and .xml for the path. 
  metaUrl <- paste(metaUrl, file, "/index.csv", sep="")
  
  #Now, ideally, we have a nice path to return from which we can download the metadata. 
  return(metaUrl)
}