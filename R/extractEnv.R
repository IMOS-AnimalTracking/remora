##' @title Extract and append remote sensing environmental data to detection data
##'
##' @description Accesses and download environmental data from the IMOS THREDDS 
##' server and append variables to detection data based on date of detection
##'
##' @param df detection data source in data frame with at the minimum a X, Y and
##' date time field
##' @param X name of column with X coordinate or longitude (EPSG 4326)
##' @param Y name of column with Y coordinate or latitude (EPSG 4326)
##' @param datetime name of column with date time stamp (Coordinated Universal 
##' Time; UTC)
##' @param env_var variable needed options include ('rs_sst', 'rs_sst_interpolated', 
##' 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'bathy', 'dist_to_land', 
##' 'rs_current')
##' @param folder_name name of folder within 'imos.cache' where downloaded rasters 
##' should be saved. default NULL produces automatic folder names based on study extent
##' @param verbose should function provide details of what operation is being conducted. 
##' Set to `FALSE` to keep it quiet
##' @param cache_layers should the extracted environmental data be cached within
##' the working directory? if FALSE stored in temporary folder and discarded 
##' after environmental extraction
##' @param crop_layers should the extracted environmental data be cropped to 
##' within the study site
##' @param full_timeperiod should environmental variables extracted for each day
##' across full monitoring period, time and memory consuming for long projects
##' @param fill_gaps should the function use a spatial buffer to estimate 
##' environmental variables for detections where there is missing data. Default 
##' is `FALSE` to save computational time.
##' @param buffer radius of buffer (in m) around each detection from which 
##' environmental variables should be extracted from. A median value of pixels 
##' that fall within the buffer will be used if `fill_gaps = TRUE`. If `NULL` a 
##' buffer will be chosen based on the resolution of environmental layer. A 
##' numeric value (in m) can be used here to customise buffer radius.
##' @param nrt should Near Real-Time current data be used if Delayed-Mode current 
##' data is missing. Default is `FALSE`, in which case NA's are appended to current
##' variables for years (currently, all years after 2020) when current data are 
##' missing. Note that Near Real-Time data are subject to less quality control 
##' than Delayed-Mode data.
##' @param output_format File type for cached environmental layers. See 
##' \code{\link[raster]{writeFormats}}. The default format is 'raster'.
##' @param .parallel should the function be run in parallel 
##' @param .ncores number of cores to use if set to parallel. If none provided, 
##' uses \code{\link[parallel]{detectCores}} to determine number.
##'
##' @details The `extractEnv` function allows the user to access, download and 
##' append a range of environmental variables to each detection within a telemetry
##' data set. We advocate for users to first undertake a quality control step using
##' the \code{\link{runQC}} function before further analysis, however the 
##' functionality to append environmental data will work on any dataset that has
##' at the minimum spatial coordinates (i.e., latitude, longitude; in EPSG 4326)
##' and a timestamp (in UTC) for each detection event. Quality controlled 
##' environmental variables housed in the IMOS Thredds server will be extracted 
##' for each specific coordinate at the specific timestamp where available. A 
##' summary table of the full range of environmental variables currently 
##' available can be accessed using the \code{\link{imos_variables}} function.
##' 
##'
##' @return a dataframe with the environmental variable appended as an extra 
##' column based on date of each detection
##'
##' @examples
##' ## Input example detection dataset that have run through the quality control
##' ##   workflow (see 'runQC' function)
##' 
##' library(tidyverse)
##' data("TownsvilleReefQC")
##' 
##' ## simplify & subset data for example speed-up
##' qc_data <- 
##'   TownsvilleReefQC %>% 
##'   unnest(cols = c(QC)) %>% 
##'   ungroup() %>% 
##'   filter(Detection_QC %in% c(1,2)) %>%
##'   filter(filename == unique(filename)[1]) %>%
##'   slice(5:8)
##' 
##' ## Extract daily interpolated sea surface temperature
##' ## cache_layers & fill_gaps args set to FALSE for speed
##' data_with_sst <- 
##'   extractEnv(df = qc_data,
##'               X = "receiver_deployment_longitude", 
##'               Y = "receiver_deployment_latitude", 
##'               datetime = "detection_datetime", 
##'               env_var = "rs_sst_interpolated",
##'               cache_layers = FALSE,
##'               crop_layers = TRUE,
##'               full_timeperiod = FALSE,
##'               fill_gaps = TRUE,
##'               folder_name = "test",
##'               .parallel = FALSE)
##'
##' @importFrom dplyr '%>%' mutate distinct pull left_join select
##' @importFrom terra ext
##' @importFrom lubridate date 
##'
##' @export
##'

extractEnv <-
  function(df,
           X = "longitude",
           Y = "latitude",
           datetime = "detection_timestamp",
           env_var,
           folder_name = NULL,
           verbose = TRUE,
           cache_layers = TRUE,
           crop_layers = TRUE,
           full_timeperiod = FALSE,
           fill_gaps = FALSE,
           buffer = NULL,
           nrt = FALSE,
           output_format = "raster",
           .parallel = TRUE,
           .ncores = NULL) {
    
  
  ## Initial checks of parameters
  if(!X %in% colnames(df)){stop("Cannot find X coordinate in dataset, provide column name where variable can be found")}
  if(!Y %in% colnames(df)){stop("Cannot find Y coordinate in dataset, provide column name where variable can be found")}
  if(!datetime %in% colnames(df)){stop("Cannot find date timestamp column in dataset, provide column name where variable can be found")}
  if(!env_var %in% c('rs_sst', 'rs_sst_interpolated', 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current', 'bathy', 'dist_to_land')){
    stop("Environmental variable not recognised, options include:\n'rs_sst', 'rs_sst_interpolated', 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current', 'bathy', 'dist_to_land'")}
  if(length(env_var) > 1){stop("This function currently only supports extracting a single variable at a time. Please only select one of:\n'rs_sst', 'rs_sst_interpolated', 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current', 'bathy', 'dist_to_land'")}
  
  ## Turn of un-needed parallelising or gap filling if extracting 'bathy', 'dist_to_land'
  if(env_var %in% c("bathy", "dist_to_land")){
    .parallel = FALSE
    fill_gaps = FALSE}
  
  if(env_var %in% "rs_current"){.parallel = FALSE}
  
  ## define date range
  unique_dates <- 
    df %>%
    mutate(date = date(!!as.name(datetime))) %>%
    distinct(date) %>%
    pull(date) 
  
  date_range <- range(unique_dates)
  
  ## define spatial extent and extend by 40%
  study_extent <- ext(c(min(df[[X]]), max(df[[X]]), min(df[[Y]]), max(df[[Y]]))) * 1.4
  
  ## define unique positions (for quicker environmental variable extraction)
  unique_positions <-
    ungroup(df) %>% 
    mutate(date = date(!!as.name(datetime))) %>%
    distinct(!!as.name(X), !!as.name(Y), date) %>% 
    dplyr::select(!!as.name(X), !!as.name(Y), date)
  
  ## define dates of detection and date range and catalog all dates between start and end if .full_timeperiod = TRUE
  if(full_timeperiod){
    if(verbose){
      message("Extracting environmental data for each day between ", 
              date_range[1], " and ", date_range[2], " (", 
              difftime(date_range[2], date_range[1], units = "days"), " days)",
              "\nThis may take a little while...") 
    }
    dates <- seq(date_range[1], date_range[2], by = 1)
  } else {
    if(verbose){
      message("Extracting environmental data only on days detections were present; between ", 
              date_range[1], " and ", date_range[2], " (", 
              length(unique_dates), " days)",
              "\nThis may take a little while...")
    }
    dates <- unique_dates
  }
  
  # Pull environmental netcdf from THREDDS server
  if(verbose){
    message("Accessing and downloading IMOS environmental variable: ", env_var)
  }
  
  if(.parallel){
      try(
        suppressWarnings(
          env_stack <- .pull_env(
            dates = dates,
            study_extent = study_extent,
            var_name = env_var,
            .cache = cache_layers,
            folder_name = folder_name,
            .crop = crop_layers,
            .nrt = nrt,
            .output_format = output_format,
            verbose = verbose,
            .parallel = .parallel,
            .ncores = .ncores
          )), 
        silent = FALSE)
  } else {
    try(
      suppressWarnings(
        env_stack <- .pull_env(
          dates = dates,
          study_extent = study_extent,
          var_name = env_var,
          .cache = cache_layers,
          folder_name = folder_name,
          .crop = crop_layers,
          .nrt = nrt,
          .output_format = output_format,
          verbose = verbose,
          .parallel = .parallel,
          .ncores = .ncores
        )),
      silent = FALSE) 
  }
  
  if(cache_layers & verbose){
    message("\nDownloaded layers are cached in the `imos.cache` folder in your working directory")
  }

  if(!is.null(env_stack)) {
    ## Extract environmental variable from env_stack
    if(verbose){
      message("Extracting and appending environmental data")
    }
    
    env_data <- .extract_var(unique_positions, env_stack, env_var, .fill_gaps = fill_gaps, .buffer = buffer, verbose = verbose)
  
  
  ## Combine environmental data with input detection data
  output <- df %>% 
    mutate(date = date(!!as.name(datetime))) %>%
    left_join(env_data, by = c(X, Y, "date"))
  
  
  ## Calculate additional variables for current data (current direction and velocity)
  if(env_var %in% "rs_current"){
    output <- output %>% 
      mutate(rs_current_velocity = sqrt(rs_vcur^2 + rs_ucur^2),
             rs_current_bearing = atan2(rs_ucur,rs_vcur)*(180/pi))
    
    ## Adjust bearing to 0 - 360 degrees clockwise
    output <- 
      output %>% 
      mutate(rs_current_bearing = 
               case_when(rs_current_bearing < 0 ~ rs_current_bearing + 360, 
                         TRUE ~ rs_current_bearing))
  }
  
  output$date <- NULL
  if(max(date_range) >= as.Date("2021-01-01") & nrt) {
    message("Near Real-Time Ocean Current data appended in place of unavailable Delayed-Mode data")
  }
  return(output)
  
  } else {
    ## if no viable urls found then return input data
    message("Returning original input data")
    return(df)
  }
}



