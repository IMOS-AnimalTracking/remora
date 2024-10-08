% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pull_var.R
\name{.pull_env}
\alias{.pull_env}
\title{Pull environmental data from IMOS THREDDS server and save to local system}
\usage{
.pull_env(
  dates,
  study_extent,
  var_name,
  folder_name = NULL,
  .cache,
  .crop,
  .nrt = FALSE,
  .output_format = ".grd",
  .parallel = TRUE,
  .ncores = NULL,
  verbose = TRUE
)
}
\arguments{
\item{dates}{detection data source in data frame with at the minimum a X, Y and date time field}

\item{study_extent}{\code{terra::ext} object defining the study area from which environmental data should be cropped}

\item{var_name}{variable needed options include current options ('rs_sst', 'rs_sst_interpolated',
'rs_salinity', 'rs_chl', 'rs_turbidity', 'rs_npp', 'rs_current', 'bathy', 'dist_to_land')}

\item{folder_name}{name of folder within 'imos.cache' where downloaded rasters should be saved.
default NULL produces automatic folder names based on study extent}

\item{.cache}{should the extracted environmental data be cached within the working directory?}

\item{.crop}{should the extracted environmental data be cropped to within the study site}

\item{.nrt}{should NRT Ocean Current data be used if DM data is not available for certain years?}

\item{.output_format}{File type for cached environmental layers. See \code{\link[raster]{writeFormats}}. The default format is 'raster'.}

\item{.parallel}{should the environmental data download be conducted in parallel to speed up the process? Currently, parallel processing
is only supported when downloading 'rs_current' data.}

\item{.ncores}{}
}
\value{
a SpatRaster object with daily spatial environmental layers associated with detection dates
}
\description{
Accesses and download environmental data from the IMOS THREDDS server
}
\details{
Internal function to download environmental raster layers from IMOS Thredds server (http://thredds.aodn.org.au/)
}
\keyword{internal}
