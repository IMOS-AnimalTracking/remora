#' @title Map IMOS tag metadata to an OTN-like format
#' 
#' @description In the same way that otn_imos_column_map takes OTN data and massages it into an IMOS-like format for REMORA, 
#' this function and its ilk take IMOS data (in this case, tag metadata and animal measurements data) and massage it into an OTN-like format, for the
#' purposes of reporting and more general applicability within the OTN suite of programs.
#'
#' @param tag_dataframe A dataframe containing IMOS-formatted tag metadata. 
#' @param animal_measurements_dataframe A dataframe containing IMOS-formatted animal measurements data. 
#'
#' @return A single dataframe containing the tag and measurement data combined into an OTN-like format. 
#' 
#' @importFrom dplyr '%>%' mutate rename left_join
#' @importFrom tidyr unite separate
#' @export
#'
imos_to_otn_tags <- function(tag_dataframe, animal_measurements_dataframe) {
  
  #First do the manipulation on the Tag dataframe. 
    tag_return <- tag_dataframe %>%
      dplyr::select(
        #Columns we can get from the file.
        transmitter_type,
        transmitter_serial_number,
        transmitter_id,
        placement,
        transmitter_estimated_battery_life,
        species_common_name,
        species_scientific_name,
        animal_sex,
        transmitter_deployment_locality,
        transmitter_deployment_latitude,
        transmitter_deployment_longitude,
        transmitter_deployment_datetime,
        transmitter_recovery_datetime,
        transmitter_deployment_comments
      ) %>%
      mutate(
        #Columns we need to change, or add as NAs.
        TAG_TYPE = "ACOUSTIC",
        TAG_MANUFACTURER = "VEMCO",
        TAGGER = NA,
        TAG_OWNER_PI = NA,
        TAG_OWNER_ORGANIZATION = NA,
        WILD_OR_HATCHERY = NA,
        STOCK = NA,
        LIFE_STAGE = NA,
        AGE = NA,
        AGE_UNITS = NA,
      ) %>%
      rename(
        #Columns we now have that need to be renamed. 
        TAG_MODEL = transmitter_type,
        TAG_SERIAL_NUMBER = transmitter_serial_number,
        TAG_IMPLANT_TYPE = placement,
        EST_TAG_LIFE = transmitter_estimated_battery_life,
        COMMON_NAME_E = species_common_name,
        SCIENTIFIC_NAME = species_scientific_name,
        SEX = animal_sex,
        RELEASE_LOCATION = transmitter_deployment_locality,
        RELEASE_LATITUDE = transmitter_deployment_latitude,
        RELEASE_LONGITUDE = transmitter_deployment_longitude,
        UTC_RELEASE_DATE_TIME = transmitter_deployment_datetime,
        HARVEST_DATE = transmitter_recovery_datetime,
        ) 
    
    #Now do the manipulation on the animal measurements dataframe. 
    animal_return <- animal_measurements_dataframe %>%
      dplyr::select(
        transmitter_id,
        measurement_value,
        measurement_type,
        measurement_unit,
        comments
      ) %>%
      mutate(
        #Set up Null columns since the above aren't 1:1 maps of the needed columns
        LENGTH = NA,
        WEIGHT = NA,
        LENGTH_TYPE = NA,
      )
    
    #Convert anything in cm to m
    animal_return$measurement_value[animal_return$measurement_unit == 'cm'] <- 
      animal_return$measurement_value/100
    
    #Convert anything in mm to m
    animal_return$measurement_value[animal_return$measurement_unit == 'mm'] <- 
      animal_return$measurement_value/1000
    
    #Convert anything in in to m
    animal_return$measurement_value[animal_return$measurement_unit == 'in'] <- 
      animal_return$measurement_value/39.37
    
    #Convert anything in ft to m
    animal_return$measurement_value[animal_return$measurement_unit == 'ft'] <- 
      animal_return$measurement_value/3.281
    
    #Wherever the measurement type contains 'length', put the measurement type
    #directly into LENGTH_TYPE... 
    animal_return$LENGTH_TYPE[grepl("LENGTH", toupper(animal_return$measurement_type))] <-
      animal_return$measurement_type
    
    #and the value directly into LENGTH (m).
    animal_return$LENGTH[grepl("LENGTH", toupper(animal_return$measurement_type))] <-
      animal_return$measurement_value
    
    #Whereas if it contains WEIGHT, put that into the weight column.
    animal_return$WEIGHT[grepl("WEIGHT", toupper(animal_return$measurement_type))] <-
      animal_return$measurement_value
    
    #We'll drop the columns we no longer need
    keeps <- c("transmitter_id", "LENGTH", "WEIGHT", "LENGTH_TYPE", "comments")
    
    #Now we join the data on transmitter ID...
    tag_return <- left_join(tag_return, animal_return[keeps], by="transmitter_id") %>%
      #We'll also split up transmitter_id now that we no longer need it in one column. 
      separate(
        col = transmitter_id,
        into = c("code_space_1", "code_space_2", "TAG_ID_CODE")
      ) %>%
      unite(
        "TAG_CODE_SPACE",
        code_space_1, code_space_2,
        sep = "-"
      ) %>%
      unite(
      #We'll also unite the cooments fields. 
      "COMMENTS",
      comments, transmitter_deployment_comments,
      sep = ";"
    ) %>% 
      rename(
        #Finally we'll rename length and weight to have the units. They were annoying
        #to index before so I just figured I'd leave them without the units then add those
        #in.
        `LENGTH (m)` = LENGTH,
        `WEIGHT (kg)` = WEIGHT
      )
    
    return(tag_return)
}