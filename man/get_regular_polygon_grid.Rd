% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regular_grid.R
\name{get_regular_polygon_grid}
\alias{get_regular_polygon_grid}
\title{Get regular QuadKey polygon grid derived from
the bounding box of the \code{quadkey} column of a data.frame.}
\usage{
get_regular_polygon_grid(data)
}
\arguments{
\item{data}{A data.frame with a \code{quadkey} column.}
}
\value{
A list with three elements:
\itemize{
\item \code{data} An \code{sf} POLYGON data.frame with all the QuadKeys within
the bounding box of the \code{quadkey} column of a data.frame.
Only the columns \code{quadkey}, \code{tileX}, \code{tileY} and \code{geometry} are returned.
\item \code{num_rows} The number of columns of the regular grid.
\item \code{num_cols} The number of rows of the regular grid.
}
}
\description{
This function estimates the bounding box of the QuadKeys given in the
\code{quadkey} column and adds the rows needed to complete a regular grid.

For a detailed explanation on how to use this
and other similar \code{quadkeyr} functions,
read the the vignette:
\url{https://docs.ropensci.org/quadkeyr/articles/facebook_mobility_csvs_to_raster_files.html}
}
\examples{
\donttest{
# Data File
path <- paste0(
  system.file("extdata", package = "quadkeyr"),
  "/cityA_2020_04_15_0000.csv"
)
data <- read.csv(path)
data <- format_fb_data(data)

get_regular_polygon_grid(data = data)
}
}
