##' @title Remotely open BRAN netCDF files 
##'
##' @description Open netCDF files from BRAN and converts them into data frame format.
##'
##' @param year Year of interest for data download
##' @param month Month of interest for data download
##' @param var Variable of interest for data download
##' @param depth Depth of interest for data download 
##' @param lon.min Minimum longitude for data download
##' @param lon.max Maximum longitude for data download
##' @param lat.min Minimum latitude for data download
##' @param lat.max Maximum latitude for data download
##'
##' @details Returns a dataframe with environmental data
##' 
##' @importFrom dplyr %>%
##' 
##' @export
 
remoteNCDF <- function(year, month, var, depth, lon.min, lon.max, lat.min, lat.max) {
  options(dplyr.summarise.inform = FALSE)
  # Create filename
  if (month < 10) {
    filename = paste0("https://thredds.nci.org.au/thredds/dodsC/gb6/BRAN/BRAN2020/daily/", var, "_", year, "_0", month, ".nc")
  } else {
    filename = paste0("https://thredds.nci.org.au/thredds/dodsC/gb6/BRAN/BRAN2020/daily/", var, "_", year, "_", month, ".nc")
  }
  # Open file remotely
  tryCatch(
    { nc <- suppressMessages(tidync::tidync(filename))},
      error = function(msg){
        error(paste("BRAN", var, "data not found for year =", year, "and month =", month, "!"))
      }
  )
  # Process data
  if (var %in% c("ocean_temp", "ocean_salt", "ocean_u", "ocean_v")) {
    # Find nearest depth layer of interest
    bran_depth <- nc %>% 
      tidync::activate("st_ocean") %>% 
      tidync::hyper_tibble()
    bran_depth$st_ocean <- bran_depth$st_ocean * -1
    if (depth > 0)
      depth <- depth * -1
    depth_layer <- which.min(abs(bran_depth$st_ocean - depth))
    # Subset data for only depth layer of interest
    nc <- nc %>% 
      tidync::hyper_filter(st_ocean = index == depth_layer)
  }
  # Subset data for geographical area of interest and export
  if (var %in% c("ocean_u", "ocean_v", "atm_flux_diag")) {
    if (var %in% c("ocean_u", "ocean_v")) {
      df.nc <- nc %>% 
      tidync::hyper_filter(xu_ocean = xu_ocean > lon.min & xu_ocean < lon.max,
        yu_ocean = yu_ocean > lat.min & yu_ocean < lat.max) %>%
      tidync::hyper_tibble()
    } else {
      df.nc <- nc %>% 
      tidync::hyper_filter(lon = lon > lon.min & lon < lon.max,
        lat = lat > lat.min & lat < lat.max) %>%
      tidync::hyper_tibble()
      df.nc <- df.nc[, c("u_atm", "v_atm", "lon", "lat", "Time")]
    }
  } else {
    df.nc <- nc %>% 
    tidync::hyper_filter(xt_ocean = xt_ocean > lon.min & xt_ocean < lon.max,
      yt_ocean = yt_ocean > lat.min & yt_ocean < lat.max) %>%
    tidync::hyper_tibble()
  }
  # Convert time variable
  tunit <- ncmeta::nc_atts(filename, "Time") %>% dplyr::filter(name == "units")
  tunit <- as.character(tunit$value)
  time_parts <- RNetCDF::utcal.nc(tunit, df.nc$Time)
  df.nc$Time <- ISOdatetime(time_parts[,"year"], 
            time_parts[,"month"], 
            time_parts[,"day"], 
            time_parts[,"hour"], 
            time_parts[,"minute"], 
            time_parts[,"second"])
  if (var == "atm_flux_diag") 
    names(df.nc) <- c("u", "v", "x", "y", "Time")
  if (var %in% c("ocean_eta_t", "ocean_mld"))
    names(df.nc) <- c(var, "x", "y", "Time")
  if (var %in% c("ocean_temp", "ocean_salt", "ocean_u", "ocean_v"))
    names(df.nc) <- c(var, "x", "y", "depth", "Time")   
  if (var == "ocean_w") {
    df.nc <- df.nc[-which(df.nc$sw_ocean > 200),] # Use only layers < 200 m
    names(df.nc) <- c(var, "x", "y", "depth", "Time")   
    # Calculate variable
    depths <- sort(unique(df.nc$depth))
    heights <- 5
    for (i in 2:length(depths)) {
      heights <- c(heights, depths[i] - depths[i - 1])
    }
    aux.depths <- data.frame(depth = depths, height = heights)
    df.nc$height <- aux.depths$height[match(df.nc$depth, aux.depths$depth)]
    df.nc$Each <- df.nc$ocean_w * df.nc$height
    df.nc <- df.nc %>%
      dplyr::mutate(loc = paste(x, y, sep = "_")) %>%
      dplyr::group_by(loc, x, y, Time) %>%
      dplyr::summarise(sum_cur = sum(Each), 
        sum_height = sum(height))
    df.nc$loc <- df.nc$sum_cur / df.nc$sum_height
    df.nc$depth <- 5
    df.nc <- df.nc[,c(1,2,3,7,4)]
    names(df.nc) <- c(var, "x", "y", "depth", "Time")   
  }
  # Export
  return(df.nc)
}