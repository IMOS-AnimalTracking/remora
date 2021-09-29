##' @title Extract number of tag detections that fall within the duration of sensor coverage at each mooring
##' @description Extract number of tag detections that fall within the duration of sensor coverage at each mooring
##' @param x The \code{trackingData} dataframe with the nearest  \code{moor_site_code} to each acoustic detection
##' @return a grouped tibble describing proportion of temporal overlap between mooring coverage and detections dataset
##' 
##' @importFrom dplyr group_by summarize
##' 
##' @export

getOverlap <- function(x){
  x %>% 
    group_by(moor_site_code,
             moor_coverage_start,
             moor_coverage_end
    ) %>%
    summarize(Count = n(),
              Poverlap = sum(is.coverage)/Count)
}