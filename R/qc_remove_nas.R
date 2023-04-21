##' @title QC Remove NAs
##'
##' @description Small function to remove NA values from a dataframe before the rest of the checks are run on it. 
##' 
##' @param data data formatted by either `remora::get_data` or `remora::get_data_arbitrary`
##' 
##' @details ...
##' 
##' @return returns the dataframe with the NA values removed. 
##' 
##' @keywords internal
##' 

qc_remove_nas <- function(data){
  
  ## Initial tests to identify & correct obvious errors in data
  ## first check for NA's in detection_datetime & remove and flag in logfile
  rn <- which(is.na(data$detection_datetime))
  if(length(rn) > 0) {
    lapply(1:length(rn), function(i) {
      write(paste0(data$filename[1],
                   ":  ", length(rn), " NA's found in detection_datetime; records removed from QC'd output"),
            file = logfile,
            append = TRUE)
    })
    ## remove records with NA's in the above variables so QC can proceed
    data <- data[-rn,]
  }

  ## check for NA's in (receiver_deployment) longitude/latitude & remove and flag in logfile
  rn <- which(is.na(data$longitude) | is.na(data$latitude))
  if(length(rn) > 0) {
    lapply(1:length(rn), function(i) {
      write(paste0(data$filename[1],
                   ":  ", length(rn), " NA's found in receiver_deployment_longitude &/or latitude; records removed from QC'd output"),
            file = logfile,
            append = TRUE)
    })
    ## remove records with NA's in the above variables so QC can proceed
    data <- data[-rn,]
  }
  return(data)
}