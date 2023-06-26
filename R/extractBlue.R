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
##' @param env_var variable needed from Bluelink. Options include ('ocean_temp', 'ocean_salt', 
##' ocean_u', 'ocean_v', 'ocean_w', 'ocean_eta_t')
##' @param extract_depth Bluelink data is 3D, so data can be obtained either at the water surface or at depth. Please
##' provide the depth of interest (between 0 and 4,509 m) as numeric and the function will automatically obtain the data at 
##' the nearest available layer. By default the data will be extracted at the water surface. 
##' @param var_name name for the column including the extracted environmental data. Can be usefull if the user wants to 
##' download the same environmental data at different depths. If not specified, it will be chosen based on the env_var and
##' extract_depth arguments.
##' @param folder_name name of folder within the working directory where the downloaded and processed 
##' netCDF files should be saved. Default (NULL) produces automatic folder names based on study extent and deletes 
##' processed files after processing. 
##' @param verbose should function provide details of what operation is being conducted. 
##' Set to `FALSE` to keep it quiet
##' @param cache_layers should the downloaded and processed environmental data be cached within
##' the working directory? If FALSE (default), the Bluelink data will be stored in a temporary folder 
##' and discarded after environmental extraction
##' @param fill_gaps should the function use a spatial buffer to estimate 
##' environmental variables for detections where there is missing data. Default 
##' is `FALSE` to save computational time.
##' @param buffer radius of buffer (in m) around each detection from which 
##' environmental variables should be extracted from. A median value of pixels 
##' that fall within the buffer will be used if `fill_gaps = TRUE`. If `NULL` a 
##' buffer will be chosen based on the resolution of environmental layer. A 
##' numeric value (in m) can be used here to customise buffer radius.
##' @param full_timeperiod should environmental variables be extracted for each day
##' across full monitoring period? This option is time and memory consuming for long projects. If this option is selected, 
##' the returned dataset will be standardized for the days with/without detections across all stations (station_name column) where animals were 
##' detected. For more details please see the package vignettes.
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
##' ## cache_layers & fill_gaps args set to FALSE for speed
##' data_with_temp <- 
##'    extractBlue(df = qc_data,
##'                X = "receiver_deployment_longitude", 
##'                Y = "receiver_deployment_latitude", 
##'                datetime = "detection_datetime", 
##'                env_var = "ocean_temp",
##'                extract_depth = 0,
##'                cache_layers = FALSE,
##'                folder_name = "Bluelink",
##'                verbose = TRUE,
##'                fill_gaps = FALSE)
##'
##' @importFrom dplyr '%>%' mutate distinct pull left_join select
##' @importFrom raster raster extent
##' @importFrom lubridate date 
##' @importFrom progressr with_progress
##'
##' @export
##'
 
extractBlue <- function(df, X = "longitude", Y = "latitude", datetime = "detection_timestamp", env_var, extract_depth = 0, 
  var_name = paste(env_var, extract_depth, sep = "_"), folder_name = "Bluelink", verbose = TRUE, cache_layers = FALSE, 
  full_timeperiod = FALSE, fill_gaps = FALSE, buffer = 10000) {
  
  ## Initial checks of parameters
  if(!X %in% colnames(df)){stop("Cannot find X coordinate in dataset, provide column name where variable can be found")}
  if(!Y %in% colnames(df)){stop("Cannot find Y coordinate in dataset, provide column name where variable can be found")}
  if(!datetime %in% colnames(df)){stop("Cannot find date timestamp column in dataset, provide column name where variable can be found")}
  if(!env_var %in% c('ocean_temp', 'ocean_salt', 'ocean_u', 'ocean_v', 'ocean_w', 'ocean_eta_t', 'ocean_mld')){
    stop("Environmental variable not recognised, options include:\n'ocean_temp', 'ocean_salt', 'ocean_u', 'ocean_v', 'ocean_w', 'ocean_eta_t', 'ocean_mld'")}
  if(length(env_var) > 1){stop("This function currently only supports extracting a single variable at a time. Please only select one of:\n'ocean_temp', 'ocean_salt', 'ocean_u', 'ocean_v', 'ocean_w', 'ocean_eta_t', 'ocean_mld'")}
  if(is.character(extract_depth)) {
    stop("Please provide the extract_depth argument as numeric")
  }

  # Create temporaty derectory to save netCDF files (if it doesn't exist)
  if(dir.exists(folder_name) == FALSE)
    dir.create(folder_name) 

  # Find time interval of interest to download data (years and months)
  year.month <- unique(substr(as.data.frame(df[,which(names(df) == datetime)])[,1], 1, 7))
  year.month <- str_split(year.month, pattern = "-")

  ### Download environmental data
  if (verbose) {
    message(paste("Downloading Bluelink (BRAN) data:", env_var,
      "\nThis step can take a while depending on the duration of your study and internet connection"))
    pb <-  txtProgressBar(min = 0, max = length(year.month), initial = 0, style = 3, width = 60)
  }
  for (i in 1:length(year.month)) {
    year <- year.month[[i]][1]
    month <- year.month[[i]][2]
    options(timeout = 1000000000) # Increase timeout for slow internet connections
    download.file(paste0("https://dapds00.nci.org.au/thredds/fileServer/gb6/BRAN/BRAN2020/daily/", env_var, "_", year, "_", month, ".nc"), 
      destfile = paste0(folder_name, "/", env_var, "_", year, "_", month, ".nc"), mode = 'wb', quiet = TRUE)
    if (verbose)
      setTxtProgressBar(pb, i)     
  }
  if (verbose)
    close(pb) 
  
  ### Process environmental data
  bran.depths <- c(2.5, 7.5, 12.5, 17.51539, 22.66702, 28.16938, 34.21801, 40.95498, 48.45498, 56.71801, 65.66938, 
    75.16702, 85.01539, 95, 105, 115, 125, 135, 145, 155, 165, 175, 185, 195, 205.1899, 217.0545, 233.1943, 255.8842, 
    286.609, 325.8842, 373.1943, 427.0545, 485.1899, 545.5111, 610.4156, 685.9268, 775.9268, 880.4156, 995.5111, 
    1115.313, 1238.354, 1368.157, 1507.734, 1658.157, 1818.354, 1985.313, 2165.18, 2431.101, 2894.842, 3603.101, 4509.18)
  bran.layer <- which.min(abs(bran.depths - extract_depth)) # Find layer of interest
  tot.dates <- unique(as.Date(df$detection_datetime))
  if (full_timeperiod) 
    tot.dates <- seq(min(tot.dates), max(tot.dates), 1)
  
  # Load and combine netCDFs (only days of interest)
  aux.names <- list.files(folder_name, pattern = ".nc")
  if (verbose) {
    message(paste("Extracting data at", extract_depth, "m"))
    pb <-  txtProgressBar(min = 0, max = length(aux.names), initial = 0, style = 3, width = 60)
  }
  for (i in 1:length(aux.names)) {
    # Load netCDF
    aux.nc <- raster::brick(paste(folder_name, aux.names[i], sep = "/"), 
      level = bran.layer) # Load depth of interest 
    # Auxiliar object to find dates in the file
    aux.dates <- seq(
      as.Date(paste(
        substr(stringr::str_remove(aux.names[i], pattern = env_var), 2, 5), 
        substr(stringr::str_remove(aux.names[i], pattern = env_var), 7, 8),
        1, sep = "-")),
      as.Date(paste(
        substr(stringr::str_remove(aux.names[i], pattern = env_var), 2, 5), 
        substr(stringr::str_remove(aux.names[i], pattern = env_var), 7, 8),
        1, sep = "-")) + (length(names(aux.nc)) - 1), 
      1
      ) 
    # Match days of data to tracking data
    index <- which(aux.dates %in% tot.dates)
    aux.nc <- aux.nc[[index]]
    names(aux.nc) <- aux.dates[index]
    # Crop files to study area
    aux.nc <- raster::crop(aux.nc, y = raster::extent(
      min(df$receiver_recovery_longitude) - 1,
      max(df$receiver_recovery_longitude) + 1,
      min(df$receiver_recovery_latitude) - 1,
      max(df$receiver_recovery_latitude) + 1)
      )
    # Compile final netCDF 
    if (i == 1) {
      nc.save <- aux.nc
    } else {
      nc.save <- raster::stack(nc.save, aux.nc)
    }
    if (verbose)
      setTxtProgressBar(pb, i)     
  }
  if (verbose)
    close(pb) 

  ### Extract environmental data from detection data (daily run)
  df$Var <- NA
  if (full_timeperiod) {
    aux.stations <- unique(df$station_name)
    df.aux <- tibble(station_name = aux.stations)
    df.aux <- expand.grid(station_name = df.aux$station_name, 
      detection_datetime = tot.dates)
    df.aux$receiver_recovery_latitude <- df$receiver_recovery_latitude[match(df.aux$station_name, df$station_name)]
    df.aux$receiver_recovery_longitude <- df$receiver_recovery_longitude[match(df.aux$station_name, df$station_name)]
    
      # Find days with detections per station
      df.aux$Detection <- 0
      df$aux <- paste(df$station_name, as.Date(df$detection_datetime), sep = "_")
      df.aux$aux <- paste(df.aux$station_name, df.aux$detection_datetime, sep = "_")
      df.aux$Detection[df.aux$aux %in% df$aux] <- 1
      df.aux <- df.aux[,-6]
    df.aux$Var <- NA
    df <- tibble::tibble(df.aux)
  } 
  if (verbose) {
    message("Extracting daily environmnetal data")
    pb <-  txtProgressBar(min = 0, max = length(tot.dates), initial = 0, style = 3, width = 60)
  }
  for (i in 1:length(tot.dates)) {
    index <- which(as.Date(df$detection_datetime) == tot.dates[i])
    aux.locs <- df[index, c("receiver_recovery_longitude", "receiver_recovery_latitude")]
    env.vals <- base::round(raster::extract(x = nc.save[[i]], y = aux.locs), 3)
    # Fill gaps for missing data
    if (fill_gaps) {
      index2 <- which(is.na(env.vals))
      if (length(index2) > 0) {
        env.vals2 <- base::round(
          suppressWarnings(as.numeric(
            raster::extract(x = nc.save[[i]], y = aux.locs[index2,], buffer = buffer, fun = mean, na.rm = TRUE))),
          3)
        env.vals[index2] <- env.vals2
      }
    }
    df$Var[index] <- env.vals
    if (verbose)
      setTxtProgressBar(pb, i)     
  }
  if (verbose)
    close(pb) 
  names(df)[length(names(df))] <- var_name

  ### Save data download and processed?
  # Remove raw Bluelink files (too large - global res)
  nc.names <- list.files(folder_name, pattern = ".nc")
  nc.names <- paste(folder_name, nc.names, sep = "/")
  unlink(nc.names)
  # Export netCDF to file
  if (cache_layers) {
    suppressWarnings(raster::writeRaster(x = nc.save, 
        filename = paste0(folder_name, "/Bluelink_", env_var, "_", extract_depth), 
        format = "CDF"))
  } else {
    nc.dir <- list.dirs(folder_name)
    unlink(nc.dir, recursive = TRUE)
  }

  return(df)
}