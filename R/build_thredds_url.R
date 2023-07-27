##' @title Build an object containing URL and date information.
##'
##' @description This function creates an object containing relevate date and URL information for 
##' pulling environmental variables from an arbitrary server. 
##'
##' @param df The dataframe from which date data will be derived.
##' @param datetime The column in df that contains date/time information.
##' @param full_timeperiod ...
##' @param url The base URL of the appropriate THREDDS server.
##' @param path The path to the file on the THREDDS server, not including the file name. (This lets us expand the functionality to
##' iterate over multiple folders).
##' @param file The name of the file on the THREDDS server. 
##' Might need to expand this to include dates if the need arises.
##' @param var ...
##' @param verbose turn on/off progress messages to console
##'
##' @details Built to be a small-scale utility function to grab an arbitrary file from an abritrary THREDDS server. May expand to allow
##' iteration over multiple files.
##'
##' @return A URL (or tibble thereof) indicating files to be pulled. 
##'
##'
##' @importFrom dplyr '%>%' slice left_join transmute mutate filter select case_when
##' @importFrom magrittr '%$%'
##' @importFrom xml2 read_html as_list
##' @importFrom purrr map_dfr
##' @importFrom tibble tibble
##'
##' @keywords internal

build_thredds_url <- function(df,
                              datetime = "datecollected",
                              full_timeperiod = FALSE,
                              url = "",
                              path = "",
                              file = "",
                              var = "",
                              verbose = TRUE) {
  
  
  #Paste together the pieces of the URL.
  url = paste(url, path, file, sep="")
  
  ## Define the date range for the environmental data. 
  unique_dates <- 
    df %>%
    mutate(date = as.Date(!!as.name(datetime))) %>%
    distinct(date) %>%
    pull(date) 
  
  date_range <- range(unique_dates)
  
  ## Generate the dates for which we want to get environmental variables. If full_timeperiod is true, we get every date from the entire
  ## time period, otherwise we only extract environmental data against the days detections were present. 
  if(full_timeperiod){
    if(verbose){
      message("Request's date range will include all days between ", 
              date_range[1], " and ", date_range[2], " (", 
              difftime(date_range[2], date_range[1], units = "days"), " days)") 
    }
    dates <- seq(date_range[1], date_range[2], by = 1)
    
  } else {
    if(verbose){
      message("Request's date range includes only days where detections took place between ", 
              date_range[1], " and ", date_range[2], " (", 
              length(unique_dates), " days)")
    }
    dates <- unique_dates
  }
  
  #Build the object containing dates, the URL to hit, and the variable we want to extract from it. 
  request <- tibble(date=dates, url_name=url, layer=var) 
  
  return(request)
}
