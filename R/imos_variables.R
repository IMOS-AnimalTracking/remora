##' @title Table of all available IMOS variables accessible from `remora` package
##'
##' @description Summary of available IMOS environmental variable to append to detection data.
##'  
##' @param  variable (optional) name of a specific variable you are interested in. Default of `NULL` provides details for all available datasets.
##'
##' @details This function helps users identify what quality controlled environmental variables 
##' can be accessed and appended to their detection data using the \code{\link{extractEnv}} of \code{\link{mooringTable}} functions. 
##'
##' @return a formatted table with details of available variables accessible through the `remora` package
##'
##' @examples
##' ## Identify all available variables
##' imos_variables()
##' 
##' ## If there is a specific variable you are interested in
##' imos_variables(variable = "rs_sst_interpolated")
##'
##' @importFrom dplyr '%>%' 
##' @importFrom readr read_csv cols
##' @importFrom knitr kable
##' @importFrom kableExtra kable_paper column_spec
##' 
##' @export
##'

imos_variables <- function(variable = NULL){
  
  if(!is.null(variable)){
    if(!variable %in% c('rs_sst', 'rs_sst_interpolated', 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current', 'bathy', 'dist_to_land')){
      stop("Environmental variable not recognised, options include:\n'rs_sst', 'rs_sst_interpolated', 'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current', 'bathy', 'dist_to_land'
Or leave as 'NULL' to see all variables available")} 
  }
  
  data("imos_variables_table", envir = environment())
  
  var_tab <- 
    imos_variables_table %>% 
    {if (!is.null(variable)) subset(., Variable %in% {{variable}}) else .}
  
  knitr::kable(var_tab, format = "html") %>%
    kableExtra::kable_paper(full_width = F) %>%
    kableExtra::column_spec(1, bold = T) %>%
    kableExtra::column_spec(6, width = "30em")
}
