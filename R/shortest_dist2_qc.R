##' @title find least-cost distance between two locations
##'
##' @description calculates least-cost distance between two receiver locations
##' if a land mass lies between them. Only applied on tags detected more than once
##'
##' @param position locations of deployment and subsequent detections as a
##' data.frame of longitude, latitude
##' @param inst installation names of detection (receiver) locations
##' @param raster raster of telemetry region coastline, if NULL then the default
##' `Aust_sr` SpatRaster is used
##' @param tr transition matrix for calculating least-cost distances (ie. produced
##' via transition, with geoCorrection applied)
##'
##' @details ...
##'
##' @return a 1-column matrix of shortest distances
##'
##' @importFrom terra rast extract crop ext crds values
##' @importFrom gdistance costDistance
##' @importFrom sf st_as_sf st_distance st_coordinates
##' @importFrom geodist geodist
##' @importFrom dplyr lag
##' @importFrom stats approx
##'
##' @keywords internal
##'

shortest_dist2 <- function(position, inst, raster = NULL, tr) {
  if (is.null(raster)) {
    raster <- rast(system.file("extdata/asr.tif", package = "remora"))
  }
  
  n <- nrow(position)
  
  pts2_o <- pts2 <- with(position, cbind(x = longitude[2:n],
                                         y = latitude[2:n]))
  
  pts1_o <- pts1 <- with(position, cbind(x = longitude[1:(n - 1)],
                                         y = latitude[1:(n - 1)]))
  
  
  inst1 <- lag(inst)
  inst2 <- inst
  
  mvmts <- unique(cbind(inst1, pts1, inst2, pts2))
  
  
  
  if(length(inst) > 1) {
    pts1 <- cbind(mvmts[, 2:3]) %>% apply(., 2, as.numeric)
    pts1.sf <- pts1 %>%
      as.data.frame() %>% 
      st_as_sf(coords = c("x", "y"), crs = 4326)
    
    pts2 <- cbind(mvmts[, 5:6]) %>% apply(., 2, as.numeric)
    pts2.sf <- pts2 %>%
      as.data.frame() %>%
      st_as_sf(coords = c("x", "y"), crs = 4326)
    
  } else if (length(inst) == 1) {
    pts1 <- t(cbind(mvmts[, 2:3]) %>% apply(., 2, as.numeric))
    dimnames(pts1) <- list(NULL, c("x","y"))
    pts1.sf <- pts1 %>%
      as.data.frame() %>% 
#      rename(x = V1, y = V2) %>%
      st_as_sf(coords = c("x", "y"), crs = 4326)

    pts2 <- t(cbind(mvmts[, 5:6]) %>% apply(., 2, as.numeric))
    dimnames(pts2) <- list(NULL, c("x","y"))
    pts2.sf <- pts2 %>%
      as.data.frame() %>%
#      rename(x = V1, y = V2) %>%
      st_as_sf(coords = c("x", "y"), crs = 4326)
  }

  xr <- c(min(position$longitude, na.rm = TRUE) - 1,
          max(position$longitude, na.rm = TRUE) + 1)
  yr <- c(min(position$latitude, na.rm = TRUE) - 1,
          max(position$latitude, na.rm = TRUE) + 1)
  
  dist <- matrix(ncol = 1, nrow = nrow(pts1_o))
  dist_mvmts <- matrix(ncol = 1, nrow = nrow(mvmts))

  for (u in 1:nrow(dist_mvmts)) {
    pts <- rbind(pts1[u, ], pts2[u, ])
    pts_o <- pts
    pts.sf <- rbind(pts1.sf[u,], pts2.sf[u,])

    ##### If detection lat/long = subsequent detection lat/long then dist = 0 km
    if (sum(pts1[u,] == pts2[u,]) == 2) {
      dist_mvmts[u] <- 0
    } else {
      ## If there's a point on land and the other offshore approximate river point by
      ##   closest point on coastline, OR if there are two points on land belonging to
      ##   two distinct installations
      
      ext.pts <- unlist(extract(raster, pts.sf, ID = FALSE))
      
      if (any(any(sum(is.na(ext.pts)) == 1,
                  all(sum(
                    is.na(ext.pts)
                  ) == 2, inst1[u] != inst2[u])),
              all(u == 1, sum(is.na(ext.pts)) >= 1))) {
        Aust_sub <-
          try(crop(raster,
                   ext(min(pts[, 1]) - 2,
                       max(pts[, 1]) + 2,
                       min(pts[, 2]) - 2,
                       max(pts[, 2]) + 2)))
        if (inherits(Aust_sub, "try-error")) {
          stop("detection locations outside extent of land raster")
        }
        
        Aust_sub <- cbind(crds(Aust_sub, na.rm = FALSE),
                          values(Aust_sub, mat = FALSE))
        Aust_sub <- Aust_sub[!is.na(Aust_sub[, 3]), ]
      }
      
      if (all(u == 1, sum(is.na(ext.pts)) >= 1)) {
        if (sum(is.na(ext.pts)) == 1) {
          dist_sub <-
            which.min(geodist(
              Aust_sub[, 1:2],
              matrix(rep(pts[is.na(ext.pts) , 1:2], nrow(Aust_sub)), 
                     nrow(Aust_sub), 2, byrow=TRUE, dimnames = list(NULL, c("lon","lat"))),
              paired = TRUE,
              measure = "geodesic",
              quiet = TRUE
            )) / 1000
          ## Find closest point on coast for river system
          pts[is.na(ext.pts), ] <- Aust_sub[dist_sub, 1:2]
        }
        if (sum(is.na(ext.pts)) > 1) {
          for (k in 1:2) {
            dist_sub <-
              which.min(geodist(
                Aust_sub[, 1:2],
                matrix(rep(pts[k, 1:2], nrow(Aust_sub)), 
                       nrow(Aust_sub), 2, byrow=TRUE, dimnames = list(NULL, c("lon","lat"))),
                paired = TRUE,
                measure = "geodesic",
                quiet = TRUE
              )) / 1000
            ## Find closest point on coast for river system
            pts[k, ] <- Aust_sub[dist_sub, 1:2]
          }
        }
      } else {
        if (any(sum(is.na(ext.pts)) == 1,
                all(sum(is.na(ext.pts)) == 2, inst1[u] != inst2[u]))) {
          if (sum(is.na(ext.pts)) == 1) {
            dist_sub <-
              which.min(geodist(
                Aust_sub[, 1:2],
                matrix(rep(pts[is.na(ext.pts), 1:2], nrow(Aust_sub)), 
                       nrow(Aust_sub), 2, byrow=TRUE, dimnames = list(NULL, c("lon","lat"))),
                paired = TRUE,
                measure = "geodesic",
                quiet = TRUE
              )) / 1000
            ## Find closest point on coast for river system
            pts[is.na(ext.pts), ] <- Aust_sub[dist_sub, 1:2]
          }
          if (sum(is.na(ext.pts)) > 1) {
            for (k in 1:2) {
              dist_sub <-
                which.min(
                  geodist(
                    Aust_sub[, 1:2],
                    matrix(rep(pts[k, 1:2], nrow(Aust_sub)), 
                           nrow(Aust_sub), 2, byrow=TRUE, dimnames = list(NULL, c("lon","lat"))),
                    paired = TRUE,
                    measure = "geodesic",
                    quiet = TRUE
                  )
                ) / 1000
              ## Find closest point on coast for river system
              pts[k, ] <- Aust_sub[dist_sub, 1:2]
              
            }
          }
        }
      }
      
      ##### Linear interpolation between positions to determine the presence of a land mass
      if (all(sum(is.na(ext.pts)) < 2,
              sum(pts[1,] == pts[2,]) == 0)) {
        interp <-
          cbind(approx(c(pts[1, 1], pts[2, 1]), c(pts[1, 2], pts[2, 2]), n = 200)$x,
                approx(c(pts[1, 1], pts[2, 1]), c(pts[1, 2], pts[2, 2]), n = 200)$y)
        int <- unlist(extract(raster, interp))
      } else
        int <- c(NA, NA
        )

      ##### Determine how distances are calculated
      ## None of the two positions were on land
      if (sum(pts == pts_o) == 4) {
        dist_mvmts[u] <- ifelse(
          any(
            sum(int == 1) == length(int),
            sum(is.na(int)) == length(int),
            sum(is.na(ext.pts)) > 0
          ),
          as.numeric(
            geodist(pts[1, ], pts[2, ],
                    measure = "geodesic",
                    quiet = TRUE) / 1000
          ),
          costDistance(tr, pts[1, ], pts[2, ]) / 1000
        )
      }
      ## One of the two positions was on land
      if (sum(pts == pts_o) == 2) {
        d <- which(rowSums(pts) != rowSums(pts_o))
        dist_mvmts[u] <- ifelse(
          any(
            sum(int == 1) == length(int),
            sum(is.na(int)) == length(int),
            sum(is.na(ext.pts)) > 0
          ),
          as.numeric(geodist(
            pts[1, ], pts[2, ],
            measure = "geodesic",
            quiet = TRUE
          )) / 1000 +
            as.numeric(geodist(
              pts_o[d, ], pts[d, ],
              measure = "geodesic",
              quiet = TRUE
            )) / 1000,
          costDistance(tr, pts[1, ], pts[2, ]) / 1000 +
            as.numeric(geodist(
              pts_o[d, ], pts[d, ],
              measure = "geodesic",
              quiet = TRUE
            )) / 1000
        )
      }
      ## The two positions were on land and transformed points have different coordinates
      if (all(sum(pts == pts_o) == 0,
              sum(pts[1, ] == pts[2, ]) < 2)) {
        dist_mvmts[u] <- ifelse(
          any(
            sum(int == 1) == length(int),
            sum(is.na(int)) == length(int),
            sum(is.na(ext.pts)) > 0
          ),
          as.numeric(geodist(
            pts[1,], pts[2,],
            measure = "geodesic",
            quiet = TRUE
          )) / 1000 +
            as.numeric(geodist(
              pts_o[1, 1:2], pts[1, 1:2],
              measure = "geodesic",
              quiet = TRUE
            )) / 1000 +
            as.numeric(geodist(
              pts_o[2, 1:2], pts[2, 1:2],
              measure = "geodesic",
              quiet = TRUE
            )) / 1000,
          
          costDistance(tr, pts[1,], pts[2,]) / 1000 +
            as.numeric(geodist(
              pts_o[1, ], pts[1, ],
              measure = "geodesic",
              quiet = TRUE
            )) / 1000 +
            as.numeric(geodist(
              pts_o[2, ], pts[2, ],
              measure = "geodesic",
              quiet = TRUE
            )) / 1000
        )
      }
      ## The two positions were on land and transformed points have same coordinates
      if (all(sum(pts == pts_o) == 0,
              sum(pts[1, ] == pts[2, ]) == 2)) {
        dist_mvmts[u] <-
          as.numeric(geodist(pts_o[1, ], pts_o[2, ],
                             measure = "geodesic",
                             quiet = TRUE)) / 1000
      }
    }
    dist[pts1_o[, 1] == pts1[u, 1] &
           pts1_o[, 2] == pts1[u, 2] &
           pts2_o[, 1] == pts2[u, 1] &
           pts2_o[, 2] == pts2[u, 2]] <- dist_mvmts[u]
  }
  
  return(dist)
}
