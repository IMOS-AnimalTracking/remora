##' @title find least-cost distance between two locations
##'
##' @description calculates least-cost distance between two receiver locations
##' if a land mass lies between them. Only applied on tags detected more than once
##'
##' @param position locations of deployment and subsequent detections as a
##' data.frame of longitude, latitude
##' @param inst installation names of detection (receiver) locations
##' @param rast raster of telemetry region coastline
##' @param tr transition matrix for calculating least-cost distances (ie. produced
##' via transition, with geoCorrection applied)
##'
##' @details ...
##'
##' @return a 1-column matrix of shortest distances
##'
##' @importFrom raster extract crop extent coordinates
##' @importFrom gdistance costDistance
##' @importFrom gmt geodist
##' @importFrom dplyr lag
##' @importFrom stats approx
##'
##' @keywords internal
##'

shortest_dist <- function(position, inst, rast, tr){
  
  #message("Shortest dist part 1")
  pts2_o <- pts2 <- cbind(x = position$longitude[2:nrow(position)],
                          y = position$latitude[2:nrow(position)])
  pts1_o <- pts1 <- cbind(x = position$longitude[1:(nrow(position)-1)],
                          y = position$latitude[1:(nrow(position) - 1)])
  inst1 <- lag(inst)
  inst2 <- inst

  #message("Shortest dist part 2")
	mvmts <- unique(cbind(inst1, pts1, inst2, pts2))
	pts1 <- cbind(as.numeric(mvmts[, 2]), as.numeric(mvmts[, 3]))
	pts2 <- cbind(as.numeric(mvmts[, 5]), as.numeric(mvmts[, 6]))
	xr <- c(min(position$longitude, na.rm = TRUE) - 1, max(position$longitude, na.rm = TRUE) + 1)
	yr <- c(min(position$latitude, na.rm = TRUE) - 1, max(position$latitude, na.rm = TRUE) + 1)

	dist <- matrix(ncol=1, nrow = nrow(pts1_o))
	dist_mvmts <- matrix(ncol = 1, nrow = nrow(mvmts))

	for (u in 1:nrow(dist_mvmts)){

		pts <- rbind(pts1[u, ], pts2[u, ])
		pts_o <- pts

		##### If detection lat/long = subsequent detection lat/long then dist = 0 km
		if (length(which(pts1[u,] == pts2[u,])) == 2) {
		  dist_mvmts[u] <- 0
		} else {
		  ## If there's a point on land and the other offshore approximate river point by
		  ##   closest point on coastline, OR if there are two points on land belonging to
		  ##   two distinct installations
		  #message(class(rast))
		  if (u == 1 & sum(is.na(extract(rast, pts))) >= 1) {
		    Aust_sub <-
		      try(crop(rast, extent(min(pts[, 1]) - 2, max(pts[, 1]) + 2, min(pts[, 2]) - 2, max(pts[, 2]) + 2)))
		    if(inherits(Aust_sub, "try-error")) stop("detection locations outside extent of land raster")
		    Aust_sub <- cbind(coordinates(Aust_sub), Aust_sub@data@values)
		    Aust_sub <- Aust_sub[Aust_sub[, 3] == 1,]

		    if (sum(is.na(extract(rast, pts))) == 1) {
		      dist_sub <-
		        which.min(geodist(
		          Aust_sub[, 2],
		          Aust_sub[, 1],
		          rep(pts[is.na(extract(rast, pts)) , 2], nrow(Aust_sub)),
		          rep(pts[is.na(extract(rast, pts)), 1], nrow(Aust_sub)),
		          units = 'km'
		        ))
		      message("Closest for river system.")
		      message(Aust_sub[dist_sub])
		      pts[is.na(extract(rast, pts)),] <- Aust_sub[dist_sub, 1:2] ## Find closest point on coast for river system
		    }
		    if (sum(is.na(extract(rast, pts))) > 1) {
		      for (k in 1:2) {
		        dist_sub <-
		          which.min(geodist(
		            Aust_sub[, 2],
		            Aust_sub[, 1],
		            rep(pts[k, 2], nrow(Aust_sub)),
		            rep(pts[k, 1], nrow(Aust_sub)),
		            units = 'km'
		          ))
		        pts[k, ] <- Aust_sub[dist_sub, 1:2] ## Find closest point on coast for river system
		      }
		    }
		  } else {


			  if (sum(is.na(extract(rast, pts))) == 1 |
			      (sum(is.na(extract(rast, pts))) == 2 & inst1[u] != inst2[u])) {
			    Aust_sub <-
			      crop(rast, extent(min(pts[, 1]) - 2, max(pts[, 1]) + 2, min(pts[, 2]) - 2, max(pts[, 2]) + 2))
			    Aust_sub <- cbind(coordinates(Aust_sub), Aust_sub@data@values)
			    Aust_sub <- Aust_sub[Aust_sub[, 3] == 1,]
			    if (sum(is.na(extract(rast, pts))) == 1) {
			      dist_sub <-
			        which.min(geodist(
			          Aust_sub[, 2],
			          Aust_sub[, 1],
			          rep(pts[is.na(extract(rast, pts)), 2], nrow(Aust_sub)),
			          rep(pts[is.na(extract(rast, pts)), 1], nrow(Aust_sub)),
			          units = 'km'
			        ))
			      message("Closest for river system.")
			      message(Aust_sub[dist_sub])
#			      browser()
			      pts[is.na(extract(rast, pts)), ] <- Aust_sub[dist_sub, 1:2] ## Find closest point on coast for river system
			    }
			    if (sum(is.na(extract(rast, pts))) > 1) {
			      for (k in 1:2) {
			        dist_sub <-
			          which.min(geodist(
			            Aust_sub[, 2],
			            Aust_sub[, 1],
			            rep(pts[k, 2], nrow(Aust_sub)),
			            rep(pts[k, 1], nrow(Aust_sub)),
			            units = 'km'
			          ))
			        pts[k, ] <- Aust_sub[dist_sub, 1:2]
			        ## Find closest point on coast for river system
			      }
			    }
			  }
			}

			##### Linear interpolation between positions to determine the presence of a land mass
			if (sum(is.na(extract(rast,pts))) < 2 & sum(pts[1,] == pts[2,]) == 0){
				interp <- cbind(approx(c(pts[1,1], pts[2,1]), c(pts[1,2], pts[2,2]), n = 200)$x,
				                approx(c(pts[1,1], pts[2,1]), c(pts[1,2], pts[2,2]), n = 200)$y)
				int <- extract(rast,interp)
			} else int <- c(NA, NA)

			##### Determine how distances are calculated
			## None of the two positions were on land
			if (sum(pts == pts_o) == 4) {
			  dist_mvmts[u] <- ifelse((
			    sum(int == 1) == length(int) |
			      sum(is.na(int)) == length(int) |
			      sum(is.na(extract(rast, pts))) > 0
			  ),
			  geodist(pts[1, 2], pts[1, 1], pts[2, 2], pts[2, 1], units = 'km'),
			  costDistance(tr, pts[1, ], pts[2, ]) / 1000
			  )
			}
			## One of the two positions was on land
			if (sum(pts == pts_o) == 2) {
			  d <- which(rowSums(pts) != rowSums(pts_o))
			  dist_mvmts[u] <-
			    ifelse((
			      sum(int == 1) == length(int) |
			        sum(is.na(int)) == length(int) |
			        sum(is.na(extract(rast, pts))) > 0
			    ),
			    geodist(pts[1, 2], pts[1, 1], pts[2, 2], pts[2, 1], units = 'km') +
			      geodist(pts_o[d, 2], pts_o[d, 1], pts[d, 2], pts[d, 1], units = 'km'),
			    costDistance(tr, pts[1, ], pts[2, ]) / 1000 +
			      geodist(pts_o[d, 2], pts_o[d, 1], pts[d, 2], pts[d, 1], units = 'km')
			    )
			}
			## The two positions were on land and transformed points have different coordinates
			if (sum(pts == pts_o) == 0 &
			    sum(pts[1,] == pts[2,]) < 2) {
			  dist_mvmts[u] <-
			    ifelse((
			      sum(int == 1) == length(int) |
			        sum(is.na(int)) == length(int) |
			        sum(is.na(extract(rast, pts))) > 0
			    ),
			    geodist(pts[1, 2], pts[1, 1], pts[2, 2], pts[2, 1], units = 'km') +
			      geodist(pts_o[1, 2], pts_o[1, 1], pts[1, 2], pts[1, 1], units = 'km') +
			      geodist(pts_o[2, 2], pts_o[2, 1], pts[2, 2], pts[2, 1], units = 'km'),
			    costDistance(tr, pts[1,], pts[2,]) / 1000 +
			      geodist(pts_o[1, 2], pts_o[1, 1], pts[1, 2], pts[1, 1], units = 'km') +
			      geodist(pts_o[2, 2], pts_o[2, 1], pts[2, 2], pts[2, 1], units = 'km')
			    )
			}
			## The two positions were on land and transformed points have same coordinates
			if (sum(pts == pts_o) == 0 & sum(pts[1, ] == pts[2, ]) == 2) {
			  dist_mvmts[u] <-
			    geodist(pts_o[1, 2], pts_o[1, 1], pts_o[2, 2], pts_o[2, 1], units = 'km')
			}
		  }
		dist[pts1_o[, 1] == pts1[u, 1] &
		       pts1_o[, 2] == pts1[u, 2] &
		       pts2_o[, 1] == pts2[u, 1] &
		       pts2_o[, 2] == pts2[u, 2]] <- dist_mvmts[u]
	}

	return(dist)
}
