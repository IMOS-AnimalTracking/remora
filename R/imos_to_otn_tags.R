imos_to_otn_tags <- function(tag_dataframe) {
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
        #measurement_value,
        #measurement_type,
        #measurement_unit,
        animal_sex,
        transmitter_deployment_locality,
        transmitter_deployment_latitude,
        transmitter_deployment_longitude,
        transmitter_deployment_datetime,
        transmitter_recovery_datetime,
        #comments,
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
        `WEIGHT (kg)` = NA,
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
        COMMENTS = transmitter_deployment_comments
      ) %>%
      separate(
        col = transmitter_id,
        into = c("code_space_1", "code_space_2", "TAG_ID_CODE")
      ) %>%
      unite(
        "TAG_CODE_SPACE",
        code_space_1, code_space_2,
        sep = "-"
      ) #%>%
      #unite(
      #  "COMMENTS",
      #  comments, transmitter_deployment_comments,
      #  sep = ";"
      #)
    
    return(tag_return)
}