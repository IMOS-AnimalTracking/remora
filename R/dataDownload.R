##' @title Download Bluelink environmental data 
##' @description helper function to download netCDF files, not called by user
##'
##' @param type type of environmental data to download, "day" or "month" resolutions currently accepted. 
##' @param year year of interest to download data for. Must be provided
##' @param month month of interest to download data for. If not provided, all months with available data will be downloaded.
##' @param varname Bluelink variable of interest to be downloaded.
##' @param quiet Should the files be downloaded in silence?
##' @return Download netCDF files from Bluelink for internal processing. 
##' 
##' @importFrom utils download.file
##' @keywords internal

dataDownload <- function(type, year, month = NULL, dir, varname, quiet = TRUE) {
  if (type %in% c("day", "month") == FALSE) {
    stop("Please provide type as 'day' or 'month'.")
  }
  if (is.null(year)) {
    stop("Please provide a year of interest to download data.")
  }  
  if(!varname %in% c('ocean_temp', 'ocean_salt', 'ocean_u', 'ocean_v', 'ocean_w', 'ocean_eta_t', 'ocean_mld', 'atm_flux_diag')){
      stop("Environmental variable not recognised, options include:\n'ocean_temp', 'ocean_salt', 'ocean_u', 'ocean_v', 'ocean_w', 'ocean_eta_t', 'ocean_mld', 'atm_flux_diag'")}
  # Change variable name to download correct netCDF file from BRAN
  if (varname %in% c('air_wind')) {
    varname <- "atm_flux_diag"
  }
  # Increase timeout for large file downloads and slow internet connexions
  options(timeout = 1000000000) 
  # Daily resolution
  if (type == "day") {
    if (year < 2023) {
      if (is.null(month)) {
        month <- 1:12
        for (i in 1:length(month)) {
          if (month[i] < 10) {
            download.file(paste0("https://dapds00.nci.org.au/thredds/fileServer/gb6/BRAN/BRAN2020/daily/", varname, "_", year, "_0", month[i], ".nc"), 
              destfile = paste0(dir, "/", varname, "_", year, "_0", month[i], ".nc"), mode = 'wb', quiet = quiet)
            gc()
          } else {
            download.file(paste0("https://dapds00.nci.org.au/thredds/fileServer/gb6/BRAN/BRAN2020/daily/", varname, "_", year, "_", month[i], ".nc"), 
              destfile = paste0(dir, "/", varname, "_", year, "_", month[i], ".nc"), mode = 'wb', quiet = quiet)
            gc()
          }
        }
      } else {
        for (i in 1:length(month)) {
          if (month[i] < 10) {
            download.file(paste0("https://dapds00.nci.org.au/thredds/fileServer/gb6/BRAN/BRAN2020/daily/", varname, "_", year, "_0", month[i], ".nc"), 
              destfile = paste0(dir, "/", varname, "_", year, "_0", month[i], ".nc"), mode = 'wb', quiet = quiet)
            gc()
          } else {
            download.file(paste0("https://dapds00.nci.org.au/thredds/fileServer/gb6/BRAN/BRAN2020/daily/", varname, "_", year, "_", month[i], ".nc"), 
              destfile = paste0(dir, "/", varname, "_", year, "_", month[i], ".nc"), mode = 'wb', quiet = quiet)
            gc()
          }
        }
      }      
    } else {
      if (is.null(month)) {
        month <- 1:6 # 2023 data goes to June!
        for (i in 1:length(month)) {
          if (month[i] < 10) {
            download.file(paste0("https://dapds00.nci.org.au/thredds/fileServer/gb6/BRAN/BRAN2020/daily/", varname, "_", year, "_0", month[i], ".nc"), 
              destfile = paste0(dir, "/", varname, "_", year, "_0", month[i], ".nc"), mode = 'wb', quiet = quiet)
            gc()
          } else {
            download.file(paste0("https://dapds00.nci.org.au/thredds/fileServer/gb6/BRAN/BRAN2020/daily/", varname, "_", year, "_", month[i], ".nc"), 
              destfile = paste0(dir, "/", varname, "_", year, "_", month[i], ".nc"), mode = 'wb', quiet = quiet)
            gc()
          }
        }
      } else {
        for (i in 1:length(month)) {
          if (month[i] < 10) {
            download.file(paste0("https://dapds00.nci.org.au/thredds/fileServer/gb6/BRAN/BRAN2020/daily/", varname, "_", year, "_0", month[i], ".nc"), 
              destfile = paste0(dir, "/", varname, "_", year, "_0", month[i], ".nc"), mode = 'wb', quiet = quiet)
            gc()
          } else {
            download.file(paste0("https://dapds00.nci.org.au/thredds/fileServer/gb6/BRAN/BRAN2020/daily/", varname, "_", year, "_", month[i], ".nc"), 
              destfile = paste0(dir, "/", varname, "_", year, "_", month[i], ".nc"), mode = 'wb', quiet = quiet)
            gc()
          }
        }
      }
    }
  }
  # Monthly resolution
  if (type == "month") {
    if (is.null(month)) {
      month <- 1:12
      for (i in 1:length(month)) {
        if (month[i] < 10) {
          download.file(paste0("https://dapds00.nci.org.au/thredds/fileServer/gb6/BRAN/BRAN2020/month/", varname, "_mth_", year, "_0", month[i], ".nc"), 
            destfile = paste0(dir, "/", varname, "_", year, "_0", month[i], ".nc"), mode = 'wb', quiet = quiet)
          gc()
        } else {
          download.file(paste0("https://dapds00.nci.org.au/thredds/fileServer/gb6/BRAN/BRAN2020/month/", varname, "_mth_", year, "_", month[i], ".nc"), 
            destfile = paste0(dir, "/", varname, "_", year, "_", month[i], ".nc"), mode = 'wb', quiet = quiet)
          gc()
        }
      }
    } else {
      for (i in 1:length(month)) {
        if (month[i] < 10) {
          download.file(paste0("https://dapds00.nci.org.au/thredds/fileServer/gb6/BRAN/BRAN2020/month/", varname, "_mth_", year, "_0", month[i], ".nc"), 
            destfile = paste0(dir, "/", varname, "_", year, "_0", month[i], ".nc"), mode = 'wb', quiet = quiet)
          gc()
        } else {
          download.file(paste0("https://dapds00.nci.org.au/thredds/fileServer/gb6/BRAN/BRAN2020/month/", varname, "_mth_", year, "_", month[i], ".nc"), 
            destfile = paste0(dir, "/", varname, "_", year, "_", month[i], ".nc"), mode = 'wb', quiet = quiet)
          gc()
        }
      }
    }
  }   
}   