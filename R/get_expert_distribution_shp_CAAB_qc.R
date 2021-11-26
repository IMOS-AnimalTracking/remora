##' @title match species CAAB code with CSIRO expert distribution shapefiles
##'
##' @description Get expert distribution shapefile, if available, from CSIRO's
##' Geoserver. Full CAAB code list: 
##' http://www.marine.csiro.au/datacentre/caab/caab_dump_latest.xlsx
##'
##' @param CAAB_species_id CAAB id of species for which a distribution shapefile
##' is required
##' @param spe species scientific name
##' 
##' @details For a few species acoustically tagged no shapefile exists.
##'
##' @return shp is a SpatialPolygons object of the species' distribution
##'
##' @importFrom rgdal readOGR
##' @importFrom utils download.file
##' @importFrom zip unzip
##'
##' @keywords internal

get_expert_distribution_shp_CAAB <- function(CAAB_species_id, spe){

	## Get expert distribution shapefile,if exists, from CSIRO's Geoserver
  URL <- paste0(
    "https://www.cmar.csiro.au/geoserver/caab/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=caab%3ACAAB_FISHMAP&maxFeatures=200&outputFormat=SHAPE-ZIP&CQL_FILTER=SPCODE%3D%27",
    CAAB_species_id,
    "%27"
  )
  
  tmp <- tempdir()
  if (!file.exists(file.path(tmp, CAAB_species_id, "CAAB_FISHMAPPolygon.shp"))) {
    if (!file.exists(file.path(tmp, CAAB_species_id, "CAAB_FISHMAP.shp"))) {
      ## To circumvent download even if the specified CAAB_species_id not on Geoserver
      foo <- suppressWarnings(try(download.file(URL, file.path(tmp, paste0(CAAB_species_id, ".zip")), quiet = TRUE),
                                  silent = TRUE))
      if(!inherits(foo, "try-error")) {
        unzip(
          file.path(tmp, paste0(CAAB_species_id, ".zip")),
          exdir = file.path(tmp, CAAB_species_id),
          overwrite = TRUE
        )
        unlink(file.path(tmp, paste0(CAAB_species_id, ".zip")))
      }
    }
  }

	if(file.exists(file.path(tmp, CAAB_species_id, "CAAB_FISHMAPPolygon.shp"))){
	  shp <- suppressWarnings(readOGR(dsn = file.path(tmp,
	                                                  CAAB_species_id,
	                                                  "CAAB_FISHMAPPolygon.shp"),
	                                  verbose = F))
	} else {
	  ## need "_" in species name to find ALA_Shapefile
	  spe <- gsub(" ", "_", spe)
	  ## If unavailable from CSIRO's Geoserver, use historical shapefile from ALA
	  if(length(grep("&", spe)) == 0) {
	    ALA.f <-
	      list.files(file.path(system.file(package = "remora"), "ALA_Shapefile"))
	    if (length(ALA.f[grepl(spe, ALA.f)]) == 0) {
	      k <- NA
	    } else {
	      k <- ALA.f[grepl(spe, ALA.f)]
	    }
	  }
	  
	    if (!is.na(k)){
	      shp <- suppressWarnings(
	        readOGR(dsn = system.file(file.path("ALA_Shapefile", 
	                                            k, 
	                                            paste0(k, ".shp")
	        ),
	        package = "remora"),
	        k,
	        verbose = FALSE)
	      )
	    }
	  } 
	  
  return(shp)
}
