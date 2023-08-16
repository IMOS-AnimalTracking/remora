## code to prepare `spatial_qc_rasters` dataset goes here

usethis::use_data(spatial_qc_rasters, overwrite = TRUE)

## IDJ - modified from Xav Hoenner QC.R code 01/09/2022

wm <- rnaturalearth::ne_countries(scale = 10, returnclass = "sp")
wm <- as(wm, "SpatialPolygons")
Aust <- raster::crop(wm, raster::extent(100, 170, -55, 5))

r <- raster::raster(ncol=2570, nrow=2570)
raster::extent(r) <- raster::extent(Aust)
Aust.r <- raster::rasterize(Aust, r)
Aust.r[is.na(Aust.r)] <- -1
Aust.r[Aust.r > 0] <- NA
Aust.r[Aust.r == -1] <- 1

## create transition layer
tr <- gdistance::transition(Aust.r, function(x) 1/mean(x), directions=8)
tr <- gdistance::geoCorrection(tr, type="c", scl=FALSE)

## coerce Aust.r to SpatRaster
Aust_sr <- as(Aust.r, "SpatRaster")

## write to file
terra::writeRaster(Aust_sr, file = "data/Aust_sr.tif", overwrite = TRUE)
#saveRDS(Aust_sr, file = "data/Aust_sr.RDS", compress = "xz")

## save to sysdata.rda
usethis::use_data(tr, internal = TRUE, overwrite = TRUE, compress = "xz")

rm(list = ls())


