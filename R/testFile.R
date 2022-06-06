library(tibble)
library(raster)
library(ncdf4)

dates = c(as.Date("2021-01-01"), as.Date("2021-01-08"))
## build urls based on dates and variable names
built_urls <- build_thredds_url(dates, url="https://www.neracoos.org/thredds/", path="dodsC/WW3/",
                                file="EastCoast.nc",var="dir")

## error log
error_log <- built_urls %>% filter(is.na(layer))

## extract urls with data
urls <- built_urls %>% filter(!is.na(layer))

urls <- built_urls
#raster(urls$url_name[1], varname=urls$layer[1])

rast <- raster(urls$url_name[1], varname="dir")
plot(rast)

urls$url_name

  ## Looped version
  ## run through urls to download, crop and stack environmental variables
  
  ## establish a log to store all erroneous urls
  error_log <- tibble(date = as.Date(NULL), url_name = NULL, layer = NULL)
  
  for(i in 1:nrow(urls)){
    if(i %in% 1){
      pb <- txtProgressBar(max = nrow(urls), style = 3)
      tryCatch({
        out_brick <-
          try(raster(urls$url_name[i], varname = urls$layer[i]) %>%
                {if (.crop) crop(.,study_extent) else .} , silent=TRUE)
        
        if(var_name %in% c("rs_sst_interpolated", "rs_sst")){
          names(out_brick) <- substr(out_brick@z[[1]], start = 1, stop = 10)
          ## Convert to deg C
          out_brick <- out_brick - 273.15
          
        } else {
          names(out_brick) <- as.character(out_brick@z[[1]]) 
        }
      }, error=function(e) {
        error_log <<- bind_rows(error_log, urls[i,])
      })
      
    } else {
      tryCatch({
        out_layer <- 
          try(raster(urls$url_name[i], varname = urls$layer[i]) %>%
                {if (.crop) crop(.,study_extent) else .}, silent=TRUE)
        
        if(var_name %in% c("rs_sst_interpolated", "rs_sst")){
          names(out_layer) <- substr(out_layer@z[[1]], start = 1, stop = 10)
          ## Convert to deg C
          out_layer <- out_layer - 273.15
          
        } else {
          names(out_layer) <- as.character(out_layer@z[[1]])
        }
        
        out_brick <- addLayer(out_brick, out_layer) 
        
      }, 
      error = function(e) {
        error_log <<- bind_rows(error_log, urls[i,])
      }
      )
    }
    setTxtProgressBar(pb, i)
  }
  
  ## provide log of error prone urls
  if(nrow(error_log) > 0){
    message("Environmental data were not found for ", n_distinct(error_log$date)," dates")
    message("Error log with missing data saved in the working directory")
    write_csv(error_log %>% mutate(variable = var_name), paste0(var_name, "_errorlog.txt"))}