% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qk_to_latlong_functions.R
\name{tileXY_to_pixelXY}
\alias{tileXY_to_pixelXY}
\title{Convert tile XY coordinates into pixel XY coordinates}
\usage{
tileXY_to_pixelXY(tileX, tileY)
}
\arguments{
\item{tileX}{Tile X coordinate.}

\item{tileY}{Tile Y coordinate.}
}
\value{
A list returning the pixel X and pixel Y coordinates.
}
\description{
Converts tile XY coordinates into pixel XY coordinates of
the upper-left pixel of the specified tile.
For further information, refer to the Microsoft Bing Maps Tile System
documentation.
}
\details{
Converting latitude/longitude coordinates into a QuadKey
and then back to latitude/longitude won't yield identical values,
unless the initial latitude/longitude coordinates
correspond to the upper-left Quadkey's pixel and tile XY coordinates
at the same zoom level.

Understanding this distinction is crucial for
the accurate use of these functions in coordinate conversions.

For a detailed explanation on how to use this
and other similar \code{quadkeyr} functions,
read the the vignette:
\url{https://docs.ropensci.org/quadkeyr/articles/quadkey_to_sf_conversion.html}
}
\examples{

tileXY_to_pixelXY(
  tileX = 3,
  tileY = 5
)

}
\references{
\url{https://learn.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system}
}
