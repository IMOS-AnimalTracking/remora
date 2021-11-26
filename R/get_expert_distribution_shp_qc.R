##' @title match species CAAB code with CSIRO expert distribution shapefiles,
##' when available.
##'
##' @description Get expert distribution shapefile, if available, from CSIRO's
##' Geoserver. If not, ALA shapefiles (http://www.ala.org.au/) are mathced
##' against species names and used to validate whether detections occur within
##' each species' known range.
##'
##' @param CAAB_species_id CAAB id of species for which a distribution
##' shapefile is required
##' @param spe species_scientific_name for which a distribution shapefile
##' is required
##'
##' @details For a few species acoustically tagged no shapefile exists, for a
##' few others multiple shapefiles need to be merged into a single one. gBuffer
##' extends the boundary of the original shapefile.
##'
##' @return shp_b is a SpatialPolygons object of the species' distribution
##'
##' @importFrom sp spTransform CRS
##' @importFrom rgdal readOGR
##' @importFrom rgeos gBuffer gSimplify gUnion
##' @importFrom utils download.file
##' @importFrom zip unzip
##'
##' @keywords internal
##'

get_expert_distribution_shp <- function(CAAB_species_id, spe){

	## Get expert distribution shapefile, if exists, from CSIRO's Geoserver
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
		shp <- suppressWarnings(readOGR(dsn = file.path(tmp, CAAB_species_id, 'CAAB_FISHMAPPolygon.shp'), verbose = F))
		shp_b <- suppressWarnings(spTransform(shp, CRSobj = CRS("+proj=merc +ellps=GRS80")))
		shp_b <- gBuffer(shp_b, width = 500000) ## 500 km buffer area
		shp_b <- gSimplify(shp_b, tol = 0.01, topologyPreserve = TRUE)
		shp_b <- spTransform(shp_b, CRSobj = CRS("+proj=longlat +datum=WGS84"))

		## delete file after use - leave this commented for now as tempdir should be
		##  deleted by R at end of session
    ## unlink(file.path(tmp, CAAB_species_id, 'CAAB_FISHMAPPolygon.shp'))

		return(shp_b)

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
	  } else {
	    spe <- strsplit(spe, "_&_")[[1]]
	    k <- matrix(ncol = 1, nrow = length(spe))
	    for (j in 1:length(spe)) {
	      ALA.f <-
	        list.files(file.path(system.file(package = "remora"), "ALA_Shapefile"))
	      if (length(ALA.f[grepl(spe[j], ALA.f)]) == 0) {
	        k[j] <- NA
	      } else {
	        k[j] <- ALA.f[grepl(spe[j], ALA.f)]
	      }
	      
	    }
	  }
	  
		if (length(k) == 1){
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
			  
			shp_b <- suppressWarnings(
			    spTransform(shp, CRSobj = CRS("+proj=merc +ellps=GRS80"))
			    )
			shp_b <- gBuffer(shp_b, width = 500000) ## 500 km buffer area
			shp_b <- gSimplify(shp_b, tol = 0.01, topologyPreserve = TRUE)
			shp_b <- spTransform(shp_b, CRSobj = CRS("+proj=longlat +datum=WGS84"))
			return(shp_b)
			}
		}

	  if (length(k) > 1) {
	    if (!is.na(k[1]))
	      shp_1 <-suppressWarnings(
	        readOGR(dsn = system.file(file.path("ALA_Shapefile", k[1], 
	                                            paste0(k[1], ".shp")),
	                            package = "remora"),
	          k[1],
	          verbose = F)
	        )
	    
	    if (!is.na(k[2]))
	      shp_2 <- suppressWarnings(
	        readOGR(dsn = system.file(file.path("ALA_Shapefile", k[2], 
	                                            paste0(k[2], ".shp")),
	                            package = "remora"),
	          k[2],
	          verbose = F)
	        )
	    
	    if (is.na(k[1]) & !is.na(k[2]))
	      shp <- shp_2
	    else if (!is.na(k[1]) & is.na(k[2]))
	      shp <- shp_1

	    if (!is.na(k[1]) & !is.na(k[2])) {
	      shp_1 <- suppressWarnings(
	          spTransform(shp_1, CRSobj = CRS("+proj=merc +ellps=GRS80"))
	          )
	      shp_2 <- suppressWarnings(
	        spTransform(shp_2, CRSobj = CRS("+proj=merc +ellps=GRS80"))
	        )
	      shp_1 <- gBuffer(shp_1, width = 0, byid = TRUE)
	      shp_2 <- gBuffer(shp_2, width = 0, byid = TRUE)
	      shp <- gUnion(shp_1, shp_2)
	    }
	    shp <- spTransform(shp, CRSobj = CRS("+proj=longlat +datum=WGS84"))
	    shp_b <- suppressWarnings(
	      spTransform(shp, CRSobj = CRS("+proj=merc +ellps=GRS80"))
	      )
	    shp_b <- gBuffer(shp_b, width = 500000) ## 500 km buffer area
	    shp_b <- gSimplify(shp_b, tol = 0.01, topologyPreserve = TRUE)
	    shp_b <- spTransform(shp_b, CRSobj = CRS("+proj=longlat +datum=WGS84"))
	    if (exists("shp_1"))
	      rm(shp_1)
	    if (exists("shp_2"))
	      rm(shp_2)

			return(shp_b)
		}
	}
}
