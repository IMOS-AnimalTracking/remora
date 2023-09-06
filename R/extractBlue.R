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
##' ocean_u', 'ocean_v', 'ocean_w', 'ocean_eta_t', 'air_wind')
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
##' and discarded after environmental extraction. Otherwise, it will be saved in the "cached" folder
##' within folder_name. 
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
##' @param station_name if full_timeperiod = TRUE, please provide the column that identifies the name of the 
##' acoustic stations
##' @param export_step should the processed dataset be exported to file? This is particularly usefull for large 
##' datasets, to avoid function failure due to issues with internet connexion. The rows with missing data will be 
##' exported as NAs, and only these will be completed if the function is rerun providing the exported dataset as input (df) 
##' @param export_path path and name of to export the dataset with appended environmnetal data
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
##'                folder_name = "Bluelink",
##'                verbose = TRUE,
##'                fill_gaps = FALSE)
##'
##' @importFrom dplyr '%>%' mutate distinct pull left_join select
##' @importFrom lubridate date 
##' @importFrom progressr with_progress
##'
##' @export
##'
 
extractBlue <- function(df, X, Y, datetime, env_var, extract_depth = 0, 
  var_name = paste(env_var, extract_depth, sep = "_"), folder_name = "Bluelink", 
  verbose = TRUE, cache_layers = FALSE, full_timeperiod = FALSE, station_name = NULL, 
  fill_gaps = FALSE, buffer = 10000, 
  export_step = TRUE, export_path = "Processed_data") {
  
  # Initial checks of parameters
  if (full_timeperiod & export_step){stop("'export_step' option is currently not available when full_timeperiod = TRUE. Please set export_step = FALSE.")}
  if(!X %in% colnames(df)){stop("Cannot find X coordinate in dataset, provide column name where variable can be found")}
  if(!Y %in% colnames(df)){stop("Cannot find Y coordinate in dataset, provide column name where variable can be found")}
  if(!datetime %in% colnames(df)){stop("Cannot find datetime in dataset, provide column name where variable can be found")}
  if(!datetime %in% colnames(df)){stop("Cannot find date timestamp column in dataset, provide column name where variable can be found")}
  if(!env_var %in% c('ocean_temp', 'ocean_salt', 'ocean_u', 'ocean_v', 'ocean_w', 'ocean_eta_t', 'ocean_mld', 'air_wind')){
    stop("Environmental variable not recognised, options include:\n'ocean_temp', 'ocean_salt', 'ocean_u', 'ocean_v', 'ocean_w', 'ocean_eta_t', 'ocean_mld', 'air_wind'")}
  if(length(env_var) > 1){stop("This function currently only supports extracting a single variable at a time. Please only select one of:\n'ocean_temp', 'ocean_salt', 'ocean_u', 'ocean_v', 'ocean_w', 'ocean_eta_t', 'ocean_mld', 'air_wind")}
  if(is.character(extract_depth)) {
    stop("Please provide the extract_depth argument as numeric")
  }
  # Check if data needs to be download for full timeperiod or not
  df <- as.data.frame(df)
  if (full_timeperiod) {
    if (is.null(station_name)) {
      stop("Please provide column with station names in the 'station_name' argument.")
    } else {
      # Create empty dataset to be filled
      stations <- unique(df[ ,station_name])
      all.dates <- as.Date(range(unique(df[,datetime])), tz = "UTC")
      all.dates <- seq(all.dates[1], all.dates[2], 1)
      df.all <- expand.grid(all.dates, stations)
      names(df.all) <- c("date", station_name)
      df.all$lon <- df[,X][match(df.all[,station_name], df[,station_name])]
      df.all$lat <- df[,Y][match(df.all[,station_name], df[,station_name])]
      df.all$Var <- NA 
      names(df.all)[length(names(df.all))] <- var_name
        # For data download!
        df.all$aux.date <- substr(df.all$date, 1, 7) 
        dates <- unique(df.all$aux.date)
      if (dir.exists(folder_name) == FALSE) 
        dir.create(folder_name)
    }
  } else {
    # Check for previous function runs
    if (dir.exists(folder_name) == FALSE) 
      dir.create(folder_name)
    df$aux.date <- substr(df[,which(names(df) == datetime)], 1, 7)
    if (env_var == "air_wind") {
      if (c("wind_dir") %in% names(df)) {
        message("Wind data was found. Continuing data download.")
        dates <- unique(df$aux.date[is.na(df[which(names(df) == "wind_dir")])])  
      } else {
        dates <- unique(df$aux.date)
        df$wind_dir <- NA
        df$wind_spe <- NA
      }
    } else {
      if (var_name %in% names(df)) {
        message(paste("The", var_name, "variable was found. Continuing data download."))
        dates <- unique(df$aux.date[is.na(df[which(names(df) == var_name)])])  
      } else {
        dates <- unique(df$aux.date)
        df$Var <- NA
        names(df)[length(names(df))] <- var_name
      }
    }    
  }
  # Download and extract variable:
  if (verbose) {
    if (env_var == "air_wind") {
      message(paste("Downloading and processing wind data:",
        min(dates), "|", max(dates)))
    } else {
      message(paste("Downloading and processing", var_name, "data:",
        min(dates), "|", max(dates)))
    }
    pb <-  txtProgressBar(min = 0, max = length(dates), initial = 0, style = 3, width = 60)
  }
  # Change variables names for download
  if (env_var == "air_wind") {
    download.var <- "atm_flux_diag"
  } else {
    download.var <- env_var
  }
  # Processing begins
  for (i in 1:length(dates)) {
    # Download BRAN data
    options(timeout = 1000000000) # Increase timeout for slow internet connections
    aux.date <- dates[i]
    dataDownload(type = "day", 
      year = as.numeric(substr(aux.date, 1, 4)), 
      month = as.numeric(substr(aux.date, 6, 7)), 
      dir = folder_name, 
      varname = download.var,
      quiet = TRUE)
    # Load BRAN data
    nc.bran <- terra::rast(
        paste0(folder_name, "/", 
        download.var, "_", substr(aux.date, 1, 4), "_", substr(aux.date, 6, 7), ".nc")) 
    if (env_var == "air_wind") {
      index <- which(substr(names(nc.bran), 1, 5) %in% 
        c("u_atm", "v_atm"))
      nc.bran <- nc.bran[[index]]
    }
    # Auxiliar object to find layers of interest 
    if (env_var == "ocean_temp")
      aux.names <- str_remove(names(nc.bran), pattern = "temp_st_ocean=")
    if (env_var == "ocean_salt")
      aux.names <- str_remove(names(nc.bran), pattern = "salt_st_ocean=")
    if (env_var == "ocean_v")
      aux.names <- str_remove(names(nc.bran), pattern = "v_st_ocean=")
    if (env_var == "ocean_u")
      aux.names <- str_remove(names(nc.bran), pattern = "u_st_ocean=")
    if (env_var == "ocean_eta_t")
      aux.names <- str_remove(names(nc.bran), pattern = "eta_t_")
    if (env_var == "ocean_mld")
      aux.names <- str_remove(names(nc.bran), pattern = "mld_")
    if (env_var == "air_wind") {
      aux.names <- str_replace(names(nc.bran), pattern = "u_atm", 
        replacement = "uatm")
      aux.names <- str_replace(aux.names, pattern = "v_atm", 
        replacement = "vatm")
    }
    aux.names <- str_remove(aux.names, pattern = "Time=")
    aux.names <- str_split(aux.names, pattern = "_")          
    if (env_var %in% c("ocean_temp", "ocean_salt", "ocean_v", "ocean_u", "ocean_w")) {
      aux.depth <- NULL
    } else {
      aux.depth <- 0
    }
    if (env_var == "air_wind") {
      aux.time <- NULL
      aux.var <- NULL
      for (size.depth in 1:length(aux.names)) {
        aux.time <- c(aux.time, aux.names[[size.depth]][2]) 
        aux.var <- c(aux.var, aux.names[[size.depth]][1])
      }
      aux.names <- data.frame(Var = aux.var, Depth = as.numeric(aux.depth) * -1, Time = as.numeric(aux.time))
      aux.names$Time <- as.Date("1979-01-01", tz = "UTC") + aux.names$Time 
    } else {
      aux.time <- NULL
      for (size.depth in 1:length(aux.names)) {
        if (env_var %in% c("ocean_temp", "ocean_salt", "ocean_v", "ocean_u", "ocean_w")) {
          aux.depth <- c(aux.depth, aux.names[[size.depth]][1])
          aux.time <- c(aux.time, aux.names[[size.depth]][2]) 
        } else {
          aux.time <- c(aux.time, aux.names[[size.depth]][1]) 
        }
      }
      aux.names <- data.frame(Depth = as.numeric(aux.depth) * -1, Time = as.numeric(aux.time))
      aux.names$Time <- as.Date("1979-01-01", tz = "UTC") + aux.names$Time
    }
    # Process data!
    if (full_timeperiod) { # Full timeperiod
      index.day <- which(df.all$aux.date == dates[i])
      for (ii in 1:length(index.day)) {
        aux.day <- aux.names[which(as.character(aux.names$Time) == as.character(as.Date(df.all[index.day[ii], "date"], tz = "UTC"))),]
        index.depth <- which.min(abs(aux.day$Depth - extract_depth))
        aux.day <- aux.day[index.depth,]
        if (nrow(aux.day) > 0) {
          # Extract BRAN values
          index.layer <- which(aux.names$Depth == aux.day$Depth & aux.names$Time == aux.day$Time)
          aux.bran <- nc.bran[[index.layer]]
          aux.bran <- invisible(terra::rotate(aux.bran))
          aux.val <- terra::extract(
            x = aux.bran, 
            y = df.all[index.day[ii], c("lon", "lat")])[1,2]
          # Export processed data if requested by user
          if (cache_layers == TRUE) {
            aux.area <- terra::rast()
            terra::ext(aux.area) <- c(min(df[,X]), 
              max(df[,X]), 
              min(df[,Y]),
              max(df[,Y]))
            aux.cache <- terra::crop(x = aux.bran,
              y = aux.area)
            terra::writeCDF(aux.cache, 
              filename = paste0(folder_name, "/cached/", var_name, "_", aux.day$Time[1], ".nc"))
          }
          # Use buffer to fill NAs 
          if (is.na(aux.val) & fill_gaps) {
            # Create point object and apply buffer
            pos_sf <- 
            df.all[index.day[ii], c("lon", "lat")] %>% 
            sf::st_as_sf(coords = c(1,2), crs = 4326, remove = FALSE)
            pos_sf <- sf::st_buffer(pos_sf, buffer)
            # Extract data from buffered points
            aux.val <- terra::extract(x = aux.bran, y = pos_sf)
            names(aux.val)[2] <- "Buffer"
            aux.val <- aux.val %>%
              group_by(ID) %>%
              summarise(Buffer_mean = mean(Buffer, na.rm = TRUE))
            aux.val <- aux.val$Buffer_mean
            aux.day$Var <- aux.val
          } else {
            aux.day$Var <- aux.val
          }
        }
        if (env_var %in% c('ocean_temp', 'ocean_salt', 'ocean_u', 'ocean_v', "ocean_eta_t", "ocean_mld"))
          aux.day$Var <- round(aux.day$Var, 4)
        df.all[index.day[ii], which(names(df.all) == var_name)] <- aux.day$Var
        gc()
      } 
    } else {  # Only tracking locations
      index.day <- which(df$aux.date == dates[i])
      for (ii in 1:length(index.day)) {
        if (env_var == "air_wind") {
          aux.day <- aux.names[which(as.character(aux.names$Time) == as.character(as.Date(df[index.day[ii], which(names(df) == datetime)], tz = "UTC"))),]
        } else {
          aux.day <- aux.names[which(as.character(aux.names$Time) == as.character(as.Date(df[index.day[ii], which(names(df) == datetime)], tz = "UTC"))),]
          index.depth <- which.min(abs(aux.day$Depth - extract_depth))
          aux.day <- aux.day[index.depth,]
        }     
        if (nrow(aux.day) > 0) {
          # Extract BRAN values
          if (env_var == "air_wind") {
            index.u <- which(aux.names$Var == "uatm" & aux.names$Depth == aux.day$Depth & aux.names$Time == aux.day$Time)
            index.v <- which(aux.names$Var == "vatm" & aux.names$Depth == aux.day$Depth & aux.names$Time == aux.day$Time)
            bran.u <- nc.bran[[index.u]]
            bran.u <- invisible(terra::rotate(bran.u))
            bran.v <- nc.bran[[index.v]]
            bran.v <- invisible(terra::rotate(bran.v))
            aux.u <- terra::extract(
              x = bran.u, 
              y = df[index.day[ii], c(X, Y)])[1,2]
            aux.v <- terra::extract(
              x = bran.v, 
              y = df[index.day[ii], c(X, Y)])[1,2]
            aux.dir <- round(windDir(u = aux.u, v = aux.v), 2)
            aux.spe <- round(windSpd(u = aux.u, v = aux.v), 2)
            aux.val <- c(aux.dir, aux.spe)
          } else {
            index.layer <- which(aux.names$Depth == aux.day$Depth & aux.names$Time == aux.day$Time)
            aux.bran <- nc.bran[[index.layer]]
            aux.bran <- invisible(terra::rotate(aux.bran))
            aux.val <- terra::extract(
              x = aux.bran, 
              y = df[index.day[ii], c(X, Y)])[1,2]
          }
          # Export processed netCDF if requested by user
          if (cache_layers == TRUE) {
            if (dir.exists(paste(folder_name, "cached", sep = "/")) == FALSE) 
              dir.create(paste(folder_name, "cached", sep = "/"))
            aux.area <- terra::rast()
            terra::ext(aux.area) <- c(min(df[,X]), 
              max(df[,X]), 
              min(df[,Y]),
              max(df[,Y]))
            if (env_var == "air_wind") {
              aux.u <- terra::crop(x = bran.u,
                y = aux.area)
              aux.v <- terra::crop(x = bran.v,
                y = aux.area)
              terra::writeCDF(aux.u, 
                filename = paste0(folder_name, "/cached/", "uwind_", aux.day$Time[1], ".nc"),
                overwrite = TRUE)
              terra::writeCDF(aux.v, 
                filename = paste0(folder_name, "/cached/", "vwind_", aux.day$Time[1], ".nc"),
                overwrite = TRUE)
            } else {
              aux.cache <- terra::crop(x = aux.bran,
                y = aux.area)
              terra::writeCDF(aux.cache, 
                filename = paste0(folder_name, "/cached/", var_name, "_", aux.day$Time[1], ".nc"),
                overwrite = TRUE)
            }            
          }
          # Use buffer to fill NAs 
          if (is.na(aux.val[1]) & fill_gaps) {
            # Create point object and apply buffer
            pos_sf <- 
            df[index.day[ii], c(X, Y)] %>% 
            sf::st_as_sf(coords = c(1,2), crs = 4326, remove = FALSE)
            pos_sf <- sf::st_buffer(pos_sf, buffer)
            # Extract data from buffered points
            aux.val <- terra::extract(x = aux.bran, y = pos_sf)
            names(aux.val)[2] <- "Buffer"
            aux.val <- aux.val %>%
              group_by(ID) %>%
              summarise(Buffer_mean = mean(Buffer, na.rm = TRUE))
            aux.val <- aux.val$Buffer_mean
            aux.day$Var <- aux.val
          } else {
            aux.day$Var <- aux.val
          }
        }
        if (env_var %in% c('ocean_temp', 'ocean_salt', 'ocean_u', 'ocean_v', "ocean_eta_t", "ocean_mld"))
          aux.day$Var <- round(aux.day$Var, 3)
        if (env_var == "air_wind") {
          df[index.day[ii], which(names(df) == "wind_dir")] <- aux.dir
          df[index.day[ii], which(names(df) == "wind_spe")] <- aux.spe
        } else {
          df[index.day[ii], which(names(df) == var_name)] <- aux.day$Var
        }      
        gc()
      } 
    }
    if (export_step) # Export extracted data 
      write.csv(df[,-which(names(df) == "aux.date")], paste0(export_path, ".csv"), row.names = FALSE)
    # Delete BRAN raw file
    nc.names <- list.files(folder_name, pattern = ".nc")
    nc.names <- paste(folder_name, nc.names, sep = "/")
    invisible(file.remove(nc.names))
    if (verbose)
      setTxtProgressBar(pb, i)     
  }
  if (verbose)
    close(pb)
  # Match detections and standardized dataset for full_timeperiod runs
  if (full_timeperiod) {
    df.all$aux.date <- paste(df.all[,1], df.all[,2], sep = "_")
    df$aux.date <- paste(as.Date(df[,which(names(df) == datetime)], tz = "UTC"), 
      df[,station_name], sep = "_")
    index <- which(df.all$aux.date %in% df$aux.date)
    df.all$Detection <- 0
    df.all$Detection[index] <- 1 # Days and locations with detections!
    df.all <- df.all[,-which(names(df.all) == "aux.date")]
  }
  # Delete temporary directory
  if (cache_layers == FALSE) 
    invisible(unlink(folder_name, recursive = TRUE))
  # Export results
  if (full_timeperiod) {
    return(df.all)
  } else {
    return(df)
  } 
}