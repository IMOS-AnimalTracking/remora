##' @title Build urls to pull data environmental data from IMOS THREDDS server
##'
##' @description Internal function to check, define and create urls for the .pull_var() subroutine
##'
##' @param dates detection data source in data frame with at the minimum a X, Y and date time field
##' @param var_name variable needed options include available options ('rs_sst', 'rs_sst_interpolated', 
##' 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current'). For 'bathy' and 'dist_to_land', .pull_var() downloads 
##' them directly without the need for this subroutine.
##' @param verbose should function provide details of what operation is being conducted. Set to `FALSE` to keep it quiet
##'
##' @details Internal function to build URLs for specific environmental variables and dates from IMOS Thredds server (http://thredds.aodn.org.au/)
##'
##' @return a tibble with urls from where to download environmental data and layer names (where applicable) 
##'
##'
##' @importFrom dplyr '%>%' slice left_join transmute mutate filter select case_when
##' @importFrom magrittr '%$%'
##' @importFrom xml2 read_html as_list
##' @importFrom purrr map_dfr
##' @importFrom tibble tibble
##'
##' @keywords internal

.build_urls <- function(dates, var_name, verbose = TRUE){
  
  ## Check arguments
  if(!var_name %in% c('rs_sst', 'rs_sst_interpolated', 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current')){
    stop("Environmental variable not recognised, options include:\n'rs_sst', 'rs_sst_interpolated', 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current'")}
  
  ## calculate date range in dataset
  date_range <- range(dates)
  
  
  ## Check and refine dates where IMOS data is available, and set up components of url creation
  
  ## Daily SST interpolated dataset (9km resolution)
  ## 
  if(var_name %in% "rs_sst_interpolated"){
    ## check if IMOS remote sensing data covers detection data range 
    ## RAMSSA: 2006-06-12 - present
    if(date_range[1] < as.Date("2006-06-12")){
      warning("IMOS interpolated sst data is currently only available from 2006-06-12 onwards,\ndetections prior to this date will not have envrionmental data associated")}
    sub_dates <-  dates[dates > as.Date("2006-06-12")]
    fdates <- sub_dates %>% format("%Y%m%d")
    
    ## define start and mid url, and define end of THREDDS based on variable name
    ## example :"http://thredds.aodn.org.au/thredds/dodsC/IMOS/SRS/SST/ghrsst/L4/RAMSSA/2006/20060612120000-ABOM-L4_GHRSST-SSTfnd-RAMSSA_09km-AUS-v02.0-fv01.0.nc"
    #start_url <- "http://thredds.aodn.org.au/thredds/dodsC/IMOS/SRS/SST/ghrsst/L4/RAMSSA/"
    start_url <- "http://thredds.aodn.org.au/thredds/fileServer/IMOS/SRS/SST/ghrsst/L4/RAMSSA/"
    end_url <- "120000-ABOM-L4_GHRSST-SSTfnd-RAMSSA_09km-AUS-v02.0-fv01.0.nc"
    layer <- "analysed_sst"
    
  } 

  ## Daily SST 'raw' dataset (~2km resolution)
  ## 
  if(var_name %in% "rs_sst"){
    ## check if IMOS remote sensing data covers detection data range 
    ## GHRSST AVHRR dataset: 1992-03-21 - present
    if(date_range[1] < as.Date("1992-03-21")){
      warning("IMOS environmental data is currently only available from 1992-03-21 onwards,\ndetections prior to this date will not have envrionmental data associated")}
    sub_dates <- dates[dates > as.Date("1992-03-21")]
    fdates <- sub_dates %>% format("%Y%m%d")
    
    ## define start and mid url, and define end of THREDDS based on variable name
    ## example :"http://thredds.aodn.org.au/thredds/dodsC/IMOS/SRS/SST/ghrsst/L3S-1d/dn/2013/20130501092000-ABOM-L3S_GHRSST-SSTfnd-AVHRR_D-1d_dn.nc"
    start_url <- "http://thredds.aodn.org.au/thredds/fileServer/IMOS/SRS/SST/ghrsst/L3S-1d/dn/"
    end_url <- "092000-ABOM-L3S_GHRSST-SSTfnd-AVHRR_D-1d_dn.nc"
    layer <- "sea_surface_temperature"
  }
  
  
  ## Ocean colour datasets (~1km resolution)
  ## 
  if(var_name %in% c("rs_chl", "rs_turbidity", "rs_npp")){
    ## check if IMOS remote sensing data covers detection data range 
    ## ocean color (Aqua Modis): 2002-07-04 - present
    if(date_range[1] < as.Date("2002-07-04")){
      warning("IMOS environmental data is currently only available from 2002-07-04 onwards,\ndetections prior to this date will not have envrionmental data associated")}
    sub_dates <- dates[dates > as.Date("2002-07-04")]
    fdates <- sub_dates %>% format("%Y%m%d")
    
    ## define start and mid url, and define end of THREDDS based on variable name
    ## example :"http://thredds.aodn.org.au/thredds/dodsC/IMOS/SRS/OC/gridded/aqua/P1D/2013/05/A.P1D.20130501T053000Z.aust.chl_oc3.nc"
    start_url <- "http://thredds.aodn.org.au/thredds/fileServer/IMOS/SRS/OC/gridded/aqua/P1D/"
    mid_url <- "A.P1D."
    layer <- ""
    
    # if(var_name %in% "rs_sst"){end_url <- "T053000Z.aust.sst.nc"}
    if(var_name %in% "rs_chl"){end_url <- "T053000Z.aust.chl_oc3.nc"}
    if(var_name %in% "rs_turbidity"){end_url <- "T053000Z.aust.K_490.nc"}
    if(var_name %in% "rs_npp"){end_url <- "T053000Z.aust.npp_vgpm_eppley_oc3.nc"}
  }
  
  
  ## Weekly Salinity composite
  ## 
  if(var_name %in% "rs_salinity"){
    ## check if IMOS remote sensing data covers detection data range for weekly (7day composite product)
    ## salinity: 2011-08-25 - 2015-06-07
    if(!(date_range[2] <= as.Date("2011-08-27") || date_range[1] >= as.Date("2015-06-10"))){
      warning("IMOS weekly salinity data is currently only available between 2011-08-27 and 2015-06-10,\ndetections outside this period will not have salinity data associated")
      sub_dates <-  dates[dates > as.Date("2011-08-27") & dates < as.Date("2015-06-10")]
      # fdates <- sub_dates %>% format("%Y%m%d")
    } else {
      stop("IMOS weekly salinity data does not overlap with your detection data\n[currently only available between 2011-08-27 and 2015-06-10]")
    }
    
    catalog <-
      tibble(date = sub_dates, 
                     fdates = format(date, "%Y%m%d"),
                     year = format(date, "%Y"),
                     base_url = paste0("http://thredds.aodn.org.au/thredds/catalog/IMOS/SRS/SSS/aquarius/L3/7day/", year, "/"),
                     start_url =  paste0("http://thredds.aodn.org.au/thredds/fileServer/IMOS/SRS/SSS/aquarius/L3/7day/", year, "/"))
    
    if(verbose){
      message("Finding weekly IMOS salinity data...")
    }
    
    find_url <- function(m){
      base <- unique(m$base_url)[1]
      url_list <-
        paste0(base, "catalog.html") %>% 
        read_html() %>%
        as_list() %$%
        html %$%
        body %$%
        table %>% 
        map_dfr(function(x){if(is.null(x$td$a$tt[[1]])) return(NULL)
          tibble(end_url = x$td$a$tt[[1]],
                         fromdate = as.Date(substr(end_url, start = 2, stop = 9), "%Y%m%d"),
                         todate = as.Date(substr(end_url, start = 11, stop = 18), "%Y%m%d"),
                         type = substr(end_url, start = 30, stop = 30))}) %>% 
        filter(type %in% "_") %>% 
        select(-type) %>% 
        slice(-1) %>% 
        mutate(a = "a")
      
      out_join <-
        m %>% 
        mutate(a = "a") %>% 
        left_join(url_list, by = "a") %>% 
        select(-a) %>% 
        filter(date >= fromdate & date <= todate)

      return(out_join)
    }
    
    find_df <-
      catalog %>%
      split(., .$year) %>%
      map_dfr( ~ find_url(.x), .progress = T) 
    
    url_df <-
      find_df %>% 
      transmute(date = date,
                       url_name = paste0(start_url, end_url),
                       layer = "SSS")
  }
  
  
  
  ## Daily Ocean Current
  ## 
  if(var_name %in% "rs_current"){
    ## check if IMOS ocean current data covers detection data range 
    ## Ocean current: 1993-01-01 - present
    ## example : "http://thredds.aodn.org.au/thredds/catalog/IMOS/OceanCurrent/GSLA/DM/"
    if(date_range[1] < as.Date("1993-01-01")){
      warning("IMOS ocean current data is currently only available from 1993-01-01 onwards,\ndetections prior to this date will not have current data associated")
    } 
    sub_dates <-  dates[dates > as.Date("1993-01-01")]
    
    ## IDJ - 19/05/2023: directory name on thredds server has changed from: http://thredds.aodn.org.au/thredds/catalog/IMOS/OceanCurrent/GSLA/DM00/ 
    ##                      to http://thredds.aodn.org.au/thredds/catalog/IMOS/OceanCurrent/GSLA/DM/
    catalog <-
      tibble(date = sub_dates, 
                     fdates = format(date, "%Y%m%d"),
                     year = format(date, "%Y"),
                     base_url = paste0("http://thredds.aodn.org.au/thredds/catalog/IMOS/OceanCurrent/GSLA/DM/", year, "/"),
                     start_url =  paste0("http://thredds.aodn.org.au/thredds/fileServer/IMOS/OceanCurrent/GSLA/DM/", year, "/"))
    
    if(verbose){
      message("Finding IMOS Ocean Current data...")
    }
    
    find_url <- function(m){
      base <- unique(m$base_url)[1]
      url_list <-
       paste0(base, "catalog.html") %>% 
       read_html() %>%
       as_list() %$%
       html %$%
       body %$%
       table %>% 
       map_dfr(function(x){if(is.null(x$td$a$tt[[1]])) return(NULL)
         tibble(end_url = x$td$a$tt[[1]],
                        fdates = substr(end_url, start = 22, stop = 29),
                        date = as.Date(fdates, "%Y%m%d"))}) %>% 
       slice(-1)
     
      out_join <-
        m %>% 
        left_join(url_list, by = c("date", "fdates"))
      return(out_join)
    }
    
    find_df <-
      catalog %>%
      split(., .$year) %>%
      map_dfr( ~ find_url(.x), .progress = T) 
    
    url_df <-
      find_df %>% 
      transmute(date = date,
                       url_name = paste0(start_url, end_url),
                       layer = case_when(!is.na(end_url) ~ 1))
    
  }
  
  
  
  
  ## build urls from which to download environmental data (current and salinity have different formats)
  if(!var_name %in% c("rs_current", "rs_salinity")){
    if(var_name %in% c("rs_sst_interpolated", "rs_sst")){
      url_name <- paste0(start_url, 
                         substr(fdates, start = 1, stop = 4), "/",
                         fdates, end_url)
    } else {
      url_name <- paste0(start_url, 
                         substr(fdates, start = 1, stop = 4), "/",
                         substr(fdates, start = 5, stop = 6), "/",
                         mid_url,
                         fdates, end_url) 
    }
    
    url_df <- tibble(date = sub_dates, url_name, layer) 
  }
  return(url_df)
  
}






