##' @title Build a URL to pull environmental data from an arbitrary THREDDS server. 
##'
##' @description Function to pull an arbitrary file from a given THREDDS server. 
##'
##' @param url The base URL of the appropriate THREDDS server.
##' @param path The path to the file on the THREDDS server, not including the file name. (This lets us expand the functionality to
##' iterate over multiple folders).
##' @param file The name of the file on the THREDDS server. 
##' Might need to expand this to include dates if the need arises.
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

build_thredds_url <- function(dates = "", url = "", path = "", file = "", var = ""){
  url = paste(url, path, file, sep="")
  
  url_df <- tibble(date=dates, url_name=url, layer=var) 
  
  return(url_df)
}
