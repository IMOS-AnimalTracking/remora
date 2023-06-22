##' @title find least-cost distance between two locations
##'
##' @description calculates least-cost distance between two receiver locations
##' if a land mass lies between them. Only applied on tags detected more than once
##'
##' @param position locations of deployment and subsequent detections as a
##' data.frame of longitude, latitude
##' @param inst installation names of detection (receiver) locations
##' @param raster a `terra` SpatRaster of telemetry region coastline
##' @param tr transition matrix for calculating least-cost distances (ie. produced
##' via transition, with geoCorrection applied)
##'
##' @details ...
##'
##' @return a 1-column matrix of shortest distances
##'
##' @importFrom terra extract crop ext crds
##' @importFrom sf st_as_sf st_distance st_coordinates
##' @importFrom gdistance costDistance
##' @importFrom dplyr lag '%>%'
##' @importFrom stats approx
##'
##' @keywords internal
##'

shortest_dist_sf <- function(position, inst, raster, tr){
  raster <- rast(raster)
  
  n <- nrow(position)
  
  pts2_o <- pts2 <- with(position, cbind(x = longitude[2:n],
                                         y = latitude[2:n]))
    
  pts1_o <- pts1 <- with(position, cbind(x = longitude[1:(n-1)],
                                         y = latitude[1:(n-1)]))

  inst1 <- lag(inst)
  inst2 <- inst

	mvmts <- unique(cbind(inst1, pts1, inst2, pts2))
	pts1 <- cbind(mvmts[, 2:3]) %>% apply(., 2, as.numeric)
	pts1.sf <- pts1 %>% 
	  as.data.frame() %>%
	  st_as_sf(coords = c("x","y"), crs = 4326)
	pts2 <- cbind(mvmts[, 5:6]) %>% apply(., 2, as.numeric)
	pts2.sf <- pts2 %>%
	  as.data.frame() %>%
	  st_as_sf(coords = c("x","y"), crs = 4326)
	xr <- c(min(position$longitude, na.rm = TRUE) - 1, max(position$longitude, na.rm = TRUE) + 1)
	yr <- c(min(position$latitude, na.rm = TRUE) - 1, max(position$latitude, na.rm = TRUE) + 1)

	dist <- matrix(ncol=1, nrow = nrow(pts1_o))
	dist_mvmts <- matrix(ncol = 1, nrow = nrow(mvmts))
	
	for (u in 1:nrow(dist_mvmts)){

		pts <- rbind(pts1[u, ], pts2[u, ])
		pts_o <- pts
		pts.sf <- rbind(pts1.sf[u,], pts2.sf[u,])
		pts_o.sf <- pts.sf

		##### If detection lat/long = subsequent detection lat/long then dist = 0 km
		if (length(which(pts1[u,] == pts2[u,])) == 2) {
		  dist_mvmts[u] <- 0
		} else {
		  ## If there's a point on land and the other offshore, then approximate river point by
		  ##   closest point on coastline, OR if there are two points on land belonging to
		  ##   two distinct installations
		  
		  ext.pts <- extract(raster, pts.sf)
		  
		  if (u == 1 & sum(is.na(ext.pts)) >= 1) {
		    Aust_sub <- try(crop(raster, 
		                         ext(min(pts[, 1]) - 2, 
		                             max(pts[, 1]) + 2, 
		                             min(pts[, 2]) - 2, 
		                             max(pts[, 2]) + 2)))
		    
		    if(inherits(Aust_sub, "try-error")) stop("detection locations outside extent of land raster")
		    
		    Aust_sub <- as.data.frame(crds(Aust_sub, na.rm = TRUE)) %>%
		      st_as_sf(coords = c("x","y"), crs = 4326)
#		    Aust_sub <- Aust_sub[Aust_sub[, 3] == 1, ]

		    if (sum(is.na(ext.pts)) == 1) {
		      ds <- st_distance(Aust_sub, 
		                        rep(pts.sf[is.na(ext.pts),], nrow(Aust_sub))) / 1000
		      dist_sub <- which.min(ds)
		        # which.min(geodist(
		        #   Aust_sub[, 2],
		        #   Aust_sub[, 1],
		        #   rep(pts[is.na(ext.pts) , 2], nrow(Aust_sub)),
		        #   rep(pts[is.na(ext.pts), 1], nrow(Aust_sub)),
		        #   units = 'km'
		        # ))
		      ## Find closest point on coast for river system
		      pts[is.na(ext.pts),] <- st_coordinates(Aust_sub[dist_sub, ])
		    } 
		    if (sum(is.na(ext.pts)) > 1) {
		      for (k in 1:2) {
		        ds <- st_distance(Aust_sub, 
		                          rep(pts.sf[k,], nrow(Aust_sub))) / 1000
		        dist_sub <- which.min(ds)
		        # dist_sub <-
		        #   which.min(geodist(
		        #     Aust_sub[, 2],
		        #     Aust_sub[, 1],
		        #     rep(pts[k, 2], nrow(Aust_sub)),
		        #     rep(pts[k, 1], nrow(Aust_sub)),
		        #     units = 'km'
		        #   ))
		        ## Find closest point on coast for river system
		        pts[k, ] <- st_coordinates(Aust_sub[dist_sub, ])
		      }
		    }
		  } else {

			  if (any(sum(is.na(ext.pts)) == 1,
			      all(sum(is.na(ext.pts)) == 2, inst1[u] != inst2[u]))) {
			    Aust_sub <- crop(raster, 
			                     extent(min(pts[, 1]) - 2, 
			                            max(pts[, 1]) + 2, 
			                            min(pts[, 2]) - 2, 
			                            max(pts[, 2]) + 2))
			    Aust_sub <- as.data.frame(crds(Aust_sub, na.rm = TRUE)) %>%
			      st_as_sf(coords = c("x","y"), crs = 4326)
			    
			    if (sum(is.na(ext.pts)) == 1) {
			      ds <- st_distance(Aust_sub, 
			                        rep(pts.sf[is.na(ext.pts),], nrow(Aust_sub))) / 1000
			      dist_sub <- which.min(ds)
			      # dist_sub <-
			      #   which.min(geodist(
			      #     Aust_sub[, 2],
			      #     Aust_sub[, 1],
			      #     rep(pts[is.na(ext.pts), 2], nrow(Aust_sub)),
			      #     rep(pts[is.na(ext.pts), 1], nrow(Aust_sub)),
			      #     units = 'km'
			      #   ))
			      ## Find closest point on coast for river system
			      pts[is.na(ext.pts), ] <- st_coordinates(Aust_sub[dist_sub, ])
			    }
			    if (sum(is.na(ext.pts)) > 1) {
			      for (k in 1:2) {
			        ds <- st_distance(Aust_sub, 
			                          rep(pts.sf[k,], nrow(Aust_sub))) / 1000
			        dist_sub <- which.min(ds)
			        # dist_sub <-
			        #   which.min(geodist(
			        #     Aust_sub[, 2],
			        #     Aust_sub[, 1],
			        #     rep(pts[k, 2], nrow(Aust_sub)),
			        #     rep(pts[k, 1], nrow(Aust_sub)),
			        #     units = 'km'
			        #   ))
			        ## Find closest point on coast for river system
			        pts[k, ] <- st_coordinates(Aust_sub[dist_sub, ])
			        
			      }
			    }
			  }
			}

			##### Linear interpolation between positions to determine the presence of a land mass
			if (all(sum(is.na(ext.pts)) < 2, sum(pts[1,] == pts[2,]) == 0)){
			  interp <- approx(c(pts[1,1], pts[2,1]), c(pts[1,2], pts[2,2]), n = 200)
			  interp <- data.frame(x = interp$x, y = interp$y) %>% 
			    st_as_sf(coords = c("x","y"), crs = 4326)
				int <- extract(raster, interp)
			} else int <- c(NA, NA)

			##### Determine how distances are calculated
			## None of the two positions were on land
			if (sum(pts == pts_o) == 4) {
			  dist_mvmts[u] <- ifelse(any(sum(int == 1) == length(int),
			                             sum(is.na(int)) == length(int),
			                             sum(is.na(ext.pts)) > 0), 
			                          as.numeric(st_distance(pts.sf[1,], 
			                                                 pts.sf[2,], 
			                                                 by_element = TRUE)) / 1000,
			                          #			  geodist(pts[1, 2], pts[1, 1], pts[2, 2], pts[2, 1], units = 'km'),
			                          costDistance(tr, pts[1, ], pts[2, ]) / 1000)

			}
			## One of the two positions was on land
			if (sum(pts == pts_o) == 2) {
			  d <- which(rowSums(pts) != rowSums(pts_o))
			  dist_mvmts[u] <-
			    ifelse(any(sum(int == 1) == length(int),
			        sum(is.na(int)) == length(int),
			        sum(is.na(ext.pts)) > 0),
			        as.numeric(st_distance(pts.sf[1,],
			                               pts.sf[2,],
			                               by_element = TRUE)) / 1000 + 
			          as.numeric(st_distance(pts_o.sf[d,],
			                                 pts.sf[d,],
			                                 by_element = TRUE)) / 1000,  
			    # geodist(pts[1, 2], pts[1, 1], pts[2, 2], pts[2, 1], units = 'km') +
			    #   geodist(pts_o[d, 2], pts_o[d, 1], pts[d, 2], pts[d, 1], units = 'km'),
			    costDistance(tr, pts[1, ], pts[2, ]) / 1000 +
			      as.numeric(st_distance(pts_o.sf[d,],
			                             pts.sf[d,],
			                             by_element = TRUE)) / 1000
#			      geodist(pts_o[d, 2], pts_o[d, 1], pts[d, 2], pts[d, 1], units = 'km')
			    )
			}
			
			## The two positions were on land and transformed points have different coordinates
			if (all(sum(pts == pts_o) == 0, sum(pts[1,] == pts[2,]) < 2)) {
			  dist_mvmts[u] <-
			    ifelse(any(sum(int == 1) == length(int),
			        sum(is.na(int)) == length(int),
			        sum(is.na(ext.pts)) > 0),
			        as.numeric(st_distance(pts.sf[1,],
			                               pts.sf[2,],
			                               by_element = TRUE)) / 1000 + 
			          as.numeric(st_distance(pts_o.sf[1,],
			                                 pts.sf[1,],
			                                 by_element = TRUE)) / 1000 +
			          as.numeric(st_distance(pts_o.sf[2,],
			                                 pts.sf[2,],
			                                 by_element = TRUE)) / 1000,
			    # geodist(pts[1, 2], pts[1, 1], pts[2, 2], pts[2, 1], units = 'km') +
			    #   geodist(pts_o[1, 2], pts_o[1, 1], pts[1, 2], pts[1, 1], units = 'km') +
			    #   geodist(pts_o[2, 2], pts_o[2, 1], pts[2, 2], pts[2, 1], units = 'km'),
			    costDistance(tr, pts[1,], pts[2,]) / 1000 +
			      as.numeric(st_distance(pts_o.sf[1,],
			                             pts.sf[1,],
			                             by_element = TRUE)) / 1000 + 
			      as.numeric(st_distance(pts_o.sf[2,],
			                             pts.sf[2,],
			                             by_element = TRUE)) / 1000
			      # geodist(pts_o[1, 2], pts_o[1, 1], pts[1, 2], pts[1, 1], units = 'km') +
			      # geodist(pts_o[2, 2], pts_o[2, 1], pts[2, 2], pts[2, 1], units = 'km')
			    )
			}
			
			## The two positions were on land and transformed points have same coordinates
			if (all(sum(pts == pts_o) == 0, sum(pts[1, ] == pts[2, ]) == 2)) {
			  dist_mvmts[u] <- as.numeric(st_distance(pts_o.sf[1,],
			                           pts_o.sf[2,],
			                           by_element = TRUE)) / 1000
			    # geodist(pts_o[1, 2], pts_o[1, 1], pts_o[2, 2], pts_o[2, 1], units = 'km')
			}
		}
		
		dist[all(pts1_o[, 1] == pts1[u, 1], 
		         pts1_o[, 2] == pts1[u, 2], 
		         pts2_o[, 1] == pts2[u, 1], 
		         pts2_o[, 2] == pts2[u, 2])] <- dist_mvmts[u]
	}

	return(dist)
}
