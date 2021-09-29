##' @title Depth-time plot 
##' @description Creates a depth-time plot from a sensor dataset from a specified IMOS mooring, with an option to overlay species detection records from tracking dataset from closest receiver 
##' @param moorData Dataframe containing the sensor data from a single IMOS mooring.
##' @param moorName Character string specifying mooring name using IMOS ID code.
##' @param dateStart Optional character string of start date of date range of mooring data to plot, as "YYYY-MM-DD"
##' @param dateEnd Optional character string of end date of date range of mooring data to plot, as "YYYY-MM-DD"
##' @param varName Character string of mooring sensor variable of interest. Can be "temperature", "ucur" for meridional (u) velocity, "vcur" for zonal (v) velocity
##' @param trackingData Dataframe containing tag detections data to plot. Takes output from getDistance() function as input. Set as NULL for a base plot with no detections.
##' @param speciesID Character string describing species or tag identifier.
##' @param IDtype Character string describing type of species or tag identifier. Can be CAAB code, tag ID, common name or scientific name. Must match identifier in detections dataset.
##' @param detStart Optional character string of start date of date range of detections data to overlay on depth-time plot.
##' @param detEnd Optional character string of end date of date range of detections data to overlay on depth-time plot.
##' @importFrom lubridate date
##' @importFrom dplyr mutate %>% filter group_by summarise distinct 
##' @importFrom ggplot2 ggplot geom_tile aes scale_y_reverse ylab xlab scale_fill_distiller 
##' @importFrom ggplot2 geom_vline 
##' @importFrom viridis scale_fill_viridis
##' @export

plotDT <- function(moorData, 
                   moorName,
                   dateStart=NULL, 
                   dateEnd=NULL, 
                   varName = "temperature",
                   trackingData=NULL,
                   speciesID,
                   IDtype = "CAAB",
                   detStart=NULL,
                   detEnd=NULL){ 
  
  varName <- match.arg(varName, choices = c("temperature", "vcur", "ucur", "psal"))
  IDtype <- match.arg(IDtype, choices = c("CAAB","tag_id","species_common_name", "species_scientific_name"))
  
  if(!varName %in% c("temperature","vcur","ucur","psal")) 
    stop('Only "temperature", "vcur", "ucur" or "psal" can be provided as a valid varName')

  moorData <- moorData %>% 
    mutate(mooring.date = date(moor_timestamp), 
           depth_bin = round((moor_depth/5))*5) 
  if (!is.null(dateStart) | !is.null(dateEnd)){
    dateStart <- date(dateStart)
    dateEnd <- date(dateEnd)
    moorData <- moorData %>% 
      dplyr::filter(mooring.date>=dateStart & mooring.date<=dateEnd)
  }
  moorData <- moorData %>% 
    dplyr::filter(moor_site_code == moorName)
  if (varName == "temperature"){  
    datplot <- moorData %>% 
      group_by(mooring.date,depth_bin) %>% 
      suppressMessages(
        summarise(moor_sea_temp = median(moor_sea_temp)) 
      )
    p1 <- datplot %>% 
      ggplot() +
      geom_tile(aes(x=mooring.date, y=depth_bin, fill=moor_sea_temp)) +
      scale_y_reverse() +
      scale_fill_viridis(name = "Temperature (°C)") +
      #scale_fill_distiller() +
      ylab("Depth (m)") +
      xlab("Date")
  } else if (varName == "ucur") {
    datplot <- moorData %>% 
      dplyr::filter(!moor_ucur=="NaN") %>% 
      group_by(mooring.date,depth_bin) %>%
      suppressMessages(
        summarise(moor_ucur = median(moor_ucur))
      )
    limit <- max(abs(datplot$moor_ucur)) * c(-1, 1)
    p1 <- datplot %>% 
      ggplot() +
      geom_tile(aes(x=mooring.date, y=depth_bin, fill=moor_ucur)) +
      scale_y_reverse() +
      scale_fill_distiller(type="div", palette="RdBu", 
                           limit=limit, name = "Eastward (u) velocity (m/s)") +
      ylab("depth (m)") +
      xlab("date")
  } else if (varName == "vcur") {
    datplot <- moorData %>% 
      dplyr::filter(!moor_vcur=="NaN") %>%
      group_by(mooring.date,depth_bin) %>%
      suppressMessages(
        summarise(moor_vcur = median(moor_vcur))
      )
    limit <- max(abs(datplot$moor_vcur)) * c(-1, 1)
    p1 <- datplot %>% 
      ggplot() +
      geom_tile(aes(x=mooring.date, y=depth_bin, fill=moor_vcur)) +
      scale_y_reverse() +
      scale_fill_distiller(type="div", palette="RdBu", 
                           limit=limit, name = "Northward (v) velocity (m/s)") +
      ylab("Depth (m)") +
      xlab("Date")
  }  else if (varName == "psal") {
    datplot <- moorData %>% 
      dplyr::filter(!moor_psal=="NaN") %>%
      group_by(mooring.date,depth_bin) %>%
      suppressMessages(
        summarise(moor_psal = median(moor_psal))
      )
  #  limit <- max(abs(datplot$moor_psal)) * c(-1, 1)
    p1 <- datplot %>% 
      ggplot() +
      geom_tile(aes(x=mooring.date, y=depth_bin, fill=moor_psal)) +
      scale_y_reverse() +
      scale_fill_distiller(type="div", palette="RdBu", #limit=limit, 
                           name = "Practical salinity") +
      ylab("Depth (m)") +
      xlab("Date")
  }

  
  if(!is.null(trackingData)){
    trackingData <- trackingData %>% 
      dplyr::filter(moor_site_code == moorName) %>%
      mutate(detection_date = date(detection_datetime))
    if (!is.null(detStart) | !is.null(detEnd)){
      detIn <- trackingData %>% 
        dplyr::filter(detection_date>=detStart & detection_date<=detEnd)
    }
    detIn <- trackingData %>% 
      dplyr::filter(moor_site_code == moorName)
    if (IDtype == "CAAB"){
      detIn <- trackingData %>% 
        dplyr::filter(CAAB_species_id==speciesID)
    } else if (IDtype == "tag_id") {
      detIn <- trackingData %>% 
        dplyr::filter(tag_id==speciesID)
    } else if (IDtype == "species_common_name") {
      detIn <- trackingData %>% 
        dplyr::filter(species_common_name==speciesID)
    } else if (IDtype == "species_scientific_name") {
      detIn <- trackingData %>% 
        dplyr::filter(species_scientific_name==speciesID)
    }
    detUnique <- detIn %>% 
      distinct(detection_date, .keep_all=TRUE)
    
    if (is.null(addDetections)==FALSE){
      p1 +
        geom_vline(data=detUnique, aes(xintercept = date(detection_date)),
                   colour="black", size=0.3, alpha=0.4)
    }
  } else {
    p1
  }
}




