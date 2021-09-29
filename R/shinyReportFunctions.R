## Reporting Functions for the statistics embeded in the transmitter
## and receiver reports. It is assumed that the user will use the IMOS-ATF Web interface data

##' @title Choose a file interactively
##' @description Provides a platform-independent way to interactively select files
##' 
##' @param caption caption to display in choose file window. If method defaults to base::file.choose then
##' caption is printed to the console (doesn't work reliably due to shiny server issues)
##' @export

choose_file <- function(caption) {
  if (exists('utils::choose.files')) {
    ## Windows
    ff <- choose.files(default = getwd(), caption = caption) 
  } else if (requireNamespace("tcltk", quietly = TRUE)) {
    ## MacOS, Linux if tcltk package installed
    ff <- tcltk::tk_choose.files(default = getwd(), caption = caption)
  } else {
    ## Any platform, using base::file.choose (least preferred option)
    cat("\n", caption, "...\n", file = stderr(), sep = "")
    
    ff <- file.choose()
  }
  ## tcltk::tk_choose.files returns two file paths, 1 for shinyReport & 1 for selected data file
  ## take the last element of ff to deal with this
  return(ff[length(ff)])
}

##' @title Number of total transmitters
##' @description not to be called by user
##' @param data detections data
##' @export
n_tags <- function(data){
  length(unique(data$transmitter_id))
}

##' @title Number of total species
##' @description not to be called by user
##' @param data detections data
##' @export
n_species <- function(data){
  length(unique(data$species_common_name))
}


##' @title Number of total detections per transmitter
##' @description not to be called by user
##' @param data detections data
##' @export
n_detections <- function(data){
  nrow(data)
}

##' @title Detections per species
##' @description not to be called by user
##' @param data detections data
##' @return a tibble
##' @importFrom dplyr group_by summarise n %>%
##' @export
det_per_species <- function(data){
  data %>%
  group_by(species_common_name) %>%
  summarise(n.detections = n(), n.tags = length(unique(transmitter_id)))
  }

##' @title Number of detections per tag, tag deployment coordinates and datetime
##' @description not to be called by user
##' @param data detections data
##' @return a tibble
##' @importFrom dplyr group_by summarise n %>%
##' @export
det_per_tag_location <- function(data)
                                 {
  data %>%
  group_by(transmitter_id) %>%
  summarise(transmitter_deployment_longitude = transmitter_deployment_longitude[1],
            transmitter_deployment_latitude = transmitter_deployment_latitude[1],
            transmitter_deployment_datetime = 
              (ifelse("transmitter_deployment_datetime_new" %in% colnames(data), 
                      transmitter_deployment_datetime_new[1], 
                      transmitter_deployment_datetime[1])),
            species_common_name = species_common_name[1],
            n.detections = n())}

##' @title Number of detections per transmitter
##' @description not to be called by user
##' @param data detections data
##' @return a tibble
##' @importFrom dplyr group_by summarise n %>%
##' @export
det_per_tag <- function(data){ 
  data %>%
  group_by(transmitter_id) %>%
  summarise(n.detections = n(), species_common_name = species_common_name[1])}

##' @title Receiver location summary 
##' @description The data needs to have the detections merged to the receiver metadata. 
##' Not to be called by user
##' @param data merged detections data and receiver metadata
##' @return a tibble
##' @importFrom dplyr group_by summarise n %>%
##' @export
det_per_receiver_location <- function(data){
  data %>%
    group_by(receiver_name) %>%
    summarise(receiver_deployment_longitude=receiver_deployment_longitude[1],
              receiver_deployment_latitude=receiver_deployment_latitude[1],
              receiver_deployment_datetime = receiver_deployment_datetime[1],
              installation_name=installation_name[1],
              station_name=station_name[1],
              n.species=length(unique(species_common_name)),
              n.transmitters= length(unique(transmitter_id)),
              n.detections=n())
}

##' @title Number of total receivers
##' @description not to be called by user
##' @param data merged detections data and receiver metadata
##' @export
n_receivers <- function(data){
  length(unique(data$receiver_name))
}

##' @title Detections per station
##' @description not to be called by user
##' @param data merged detections data and receiver metadata
##' @return a tibble
##' @importFrom dplyr group_by summarise n %>%
##' @export
det_per_station <- function(data){
  data %>%
  group_by(station_name) %>%
    summarise(n.species=length(unique(species_common_name)), 
              n.transmitters= length(unique(transmitter_id)), 
              n.detections=n(),
              installation_name=installation_name[1],
              longitude=receiver_deployment_longitude[1],
              latitude=receiver_deployment_latitude[1],
             .groups = "drop")
}





##' @title Detections per station per species
##' @description not to be called by user
##' @param data merged detections data and receiver metadata
##' @return a tibble
##' @importFrom dplyr group_by summarise n %>%
##' @export
det_species_station <- function(data){
  data %>%
    group_by(station_name, species_common_name) %>%
    summarise(n.species=n(), n.tags=length(unique(transmitter_id)), .groups="drop")
}


##' @title Station Efficiency Index
##' @description not to be called by user
##' @param data merged detections data and receiver metadata
##' @param date1 start date for SEI calculation
##' @param date2 end date for SEI calculation
##' @return a tibble
##' @importFrom dplyr group_by summarise %>% mutate
##' @export
sei <- function(data, date1, date2){
  detections_in_range <- data[which((data$detection_datetime>=date1) & (data$detection_datetime<=date2)), ] 
  #Number of tags detected across all stations
  n.tags.all <- length(unique(detections_in_range$transmitter_id[!is.na(detections_in_range$transmitter_id)]))
  #Number of species detected across all stations
  n.species.all <- length(unique(detections_in_range$species_common_name[!is.na(detections_in_range$species_common_name)]))
  #Number of unique days with detections across all stations
  detections_date_range_all <- length(unique(detections_in_range$date[!is.na(detections_in_range$date)]))
  #Number of days the array was active *In this case, array = installation
  date_range_all <- date2-date1
  date_range_all <- as.numeric(date_range_all, units="days")
  
  #Edit receiver recovery and deployment date based on the selected dates by the user.
  
  data <- data %>%
    mutate(receiver_deployment_datetime = 
             ifelse(receiver_deployment_datetime < date1, 
                    date1, 
                    receiver_deployment_datetime)) %>%
    mutate(receiver_recovery_datetime = 
             ifelse(receiver_recovery_datetime > date2, 
                    date2, 
                    receiver_recovery_datetime))
  
  det.per.station<- detections_in_range %>%
    group_by(station_name) %>%
    #Number of species detected on the receiver
    summarise(n.species = length(unique(species_common_name)),
              #Number of tags detected across all receivers
              n.tags = length(unique(transmitter_id)),
              #Number of days the receiver was active
              date_range = as.numeric(max(receiver_recovery_datetime,na.rm=TRUE) - 
                                        min(receiver_deployment_datetime, na.rm=TRUE),
                                    units="days"),
              #Number of unique dates with detections on the receiver
              detections_date_range = length(unique(date[!is.na(date)])),
              n.tags.all = n.tags.all, 
              n.species.all = n.species.all, 
              detections_date_range_all = detections_date_range_all,
              date_range_all = date_range_all,
              station_name = station_name[1],
              receiver_name = receiver_name[1],
              .groups = "drop")

  ## Station Efficiency Index
  det.per.station <- det.per.station %>%
    dplyr::mutate(index = (n.tags/n.tags.all) * (n.species/n.species.all) * 
                    (detections_date_range/detections_date_range_all) * 
                    (date_range/date_range_all))
  
  return(det.per.station)
}