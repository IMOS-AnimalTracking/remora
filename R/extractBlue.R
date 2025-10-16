##' @title Extract and append Bluelink Reanalysis (BRAN) environmental data to detection data
##'
##' @description Accesses and download environmental data from the Bluelink (CSIRO)
##' server and append variables to detection data based on date of detection
##'
##' @param df detection data source in data frame with at the minimum a X, Y and
##' date time field
##' @param X name of column with X coordinate or longitude (EPSG 4326)
##' @param Y name of column with Y coordinate or latitude (EPSG 4326)
##' @param datetime name of column with date time stamp (Coordinated Universal 
##' Time; UTC)
##' @param env_var variable needed from Bluelink. Options include ('BRAN_temp', 'BRAN_salt', 
##' BRAN_ssh', 'BRAN_mld', 'BRAN_cur', 'BRAN_wind').
##' @param extract_depth Bluelink data is 3D, so data can be obtained either at the water surface or at depth. Please
##' provide the depth of interest (between 0 and 4,509 m) as numeric and the function will automatically obtain the data at 
##' the nearest available layer. By default the data will be extracted at the water surface. 
##' @param var_name name for the column including the extracted environmental data. Can be usefull if the user wants to 
##' download the same environmental data at different depths. If not specified, it will be chosen based on the env_var and
##' extract_depth arguments.
##' @param verbose should function provide details of what operation is being conducted. 
##' Set to `FALSE` to keep it quiet
##' @param env_buffer distance (in decimal degrees) to expand the study area beyond the coordinates to extract environmental data. Default value is 1°.
##' @param full_timeperiod should environmental variables be extracted for each day
##' across full monitoring period? This option is time and memory consuming for long projects. If this option is selected, 
##' the returned dataset will be standardized for the days with/without detections across all stations (station_name column) where animals were 
##' detected. For more details please see the package vignettes.
##' @param station_name if full_timeperiod = TRUE, please provide the column that identifies the name of the 
##' acoustic stations
##' @param fill_gaps should the function use a spatial buffer to estimate 
##' environmental variables for detections where there is missing data. Default 
##' is `FALSE` to save computational time.
##' @param buffer radius of buffer (in m) around each detection from which 
##' environmental variables should be extracted from. A median value of pixels 
##' that fall within the buffer will be used if `fill_gaps = TRUE`. If `NULL` a 
##' buffer will be chosen based on the resolution of environmental layer. A 
##' numeric value (in m) can be used here to customise buffer radius.
##' @param export_step should the processed dataset be exported to file? This is particularly usefull for large 
##' datasets, to avoid function failure due to issues with internet connexion. The rows with missing data will be 
##' exported as NAs, and only these will be completed if the function is rerun providing the exported dataset as input (df) 
##' @param export_path path and name of to export the dataset with appended environmnetal data
##' @param .parallel should the function be run in parallel 
##' @param .ncores number of cores to use if set to parallel. If none provided, 
##' uses \code{\link[parallel]{detectCores}} to determine number.
##'
##' @details The `extractBlue` function allows the user to download, process and 
##' append a range of 3D environmental variables (between the water surface to 4,509 m depth) 
##' to each detection within a telemetry data set. 
##' We advocate for users to first undertake a quality control step using
##' the \code{\link{runQC}} function before further analysis, however the 
##' functionality to append environmental data will work on any dataset that has
##' at the minimum spatial coordinates (i.e., latitude, longitude; in EPSG 4326)
##' and a timestamp (in UTC) for each detection event. Quality controlled 
##' environmental variables housed in the Bluelink (BRAN) CSIRO server will be extracted 
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
##' ## simplify & subset data for speed
##' qc_data <- 
##'   TownsvilleReefQC %>% 
##'   unnest(cols = c(QC)) %>% 
##'   ungroup() %>% 
##'   filter(Detection_QC %in% c(1,2)) %>%
##'   filter(filename == unique(filename)[1]) %>%
##'   slice(1:20)
##' 
##' ## Extract daily interpolated sea surface temperature
##' data_with_temp <- 
##'    extractBlue(df = qc_data,
##'                X = "receiver_deployment_longitude", 
##'                Y = "receiver_deployment_latitude", 
##'                datetime = "detection_datetime", 
##'                env_var = "BRAN_temp",
##'                extract_depth = 0,
##'                verbose = TRUE)
##'
##' @importFrom foreach %dopar% foreach
##' @importFrom parallel detectCores makeCluster stopCluster
##' @importFrom doParallel registerDoParallel
##' @importFrom geosphere distm
##' @importFrom stringr str_split_fixed
##' @importFrom utils write.csv
##' @importFrom tibble as_tibble
##'
##' @export

extractBlue <- function(df, 
                        X, 
                        Y, 
                        datetime, 
                        env_var, 
                        extract_depth = 0, 
                        var_name = paste(env_var, extract_depth, sep = "_"), 
                        env_buffer = 1,
                        full_timeperiod = FALSE, 
                        station_name = NULL,
                        fill_gaps = FALSE,
                        buffer = NULL, 
                        export_step = FALSE, 
                        export_path = "Processed_data",
                        .parallel = FALSE, 
                        .ncores = NULL, 
                        verbose = TRUE) {
  
  # Initial checks of parameters
  if(!X %in% colnames(df)){stop("Cannot find X coordinate in dataset, provide column name where variable can be found")}
  if(!Y %in% colnames(df)){stop("Cannot find Y coordinate in dataset, provide column name where variable can be found")}
  if(!datetime %in% colnames(df)){stop("Cannot find datetime in dataset, provide column name where variable can be found")}
  if(!datetime %in% colnames(df)){stop("Cannot find date timestamp column in dataset, provide column name where variable can be found")}
  if(!env_var %in% c('BRAN_temp', 'BRAN_salt', 'BRAN_ssh', 'BRAN_mld', 'BRAN_cur', 'BRAN_wcur', 'BRAN_wind')){
    stop("Environmental variable not recognised, options include:\n'BRAN_temp', 'BRAN_salt', 'BRAN_ssh', 'BRAN_mld', 'BRAN_cur', 'BRAN_wcur', 'BRAN_wind'")}
  if(length(env_var) > 1){stop("This function currently only supports extracting a single variable at a time. Please only select one of:\n'ocean_temp', 'ocean_salt', 'ocean_u', 'ocean_v', 'ocean_w', 'ocean_eta_t', 'ocean_mld', 'air_wind")}
  if(is.character(extract_depth)) {
    stop("Please provide the extract_depth argument as numeric")
  }
  if(full_timeperiod) {
    if(is.null(station_name))
      stop("Please provide column with station names in the 'station_name' argument.")
  }
  if (fill_gaps & is.null(buffer)) {
    stop("Please provide a 'buffer' size to fill gaps.")
    
  }
  
  # Define spatial and temporal extent of data fetch:
  data_details <- ext_find(
    .df = df, 
    .X = X,
    .Y = Y,
    .datetime = datetime,
    .full_timeperiod = full_timeperiod,
    verbose = verbose
  )
  
  # Create URLs to open data remotely
  var_urls <- remote_urls(input = data_details,
                          var_name = env_var,
                          verbose = TRUE)
  
  # Open data remotely 
  if(verbose){
    if (.parallel) {
      message("Accessing and extracting Bluelink data in parallel: ", env_var)
    } else {
      message("Accessing and extracting Bluelink data: ", env_var)
    }
  }
  df_env <- remote_open(input = var_urls,
                        depth = extract_depth,
                        data_details = data_details,
                        .fill_gaps = fill_gaps,
                        .buffer = buffer,
                        env_buffer = env_buffer,
                        .parallel = .parallel,
                        .ncores = .ncores,
                        var_name = env_var)
  
  # Match detections 
  df_output <- locs_match(aux_df = df,
                          X = X, 
                          Y = Y,
                          datetime = datetime, 
                          aux_env = df_env,
                          full_timeperiod = full_timeperiod, 
                          station_name = station_name,
                          var_name = env_var)  
  
  # Export
  return(df_output) 
  
}