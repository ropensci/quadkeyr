---
title: "quadkeyr: Tools for converting QuadKey-identified datasets (Microsoft's Bing Maps Tile system) into raster images and analyzing Meta (Facebook) Mobility Data."
tags:
  - R
  - Microsoft's Bing Maps Tile System
  - QuadKey
  - Tile map
  - Geospatial
  - Raster
  - Meta mobility data
  - Meta (Facebook) Mobility data
authors:
  - name: "Florencia D'Andrea"
    orcid: 0000-0002-0041-097X
    corresponding: true
    affiliation: 1
  - name: "Pilar Fernandez"
    orcid: 0000-0001-8645-2267
    affiliation: 2
affiliations:
 - name: Contract - Washington State University, USA
   index: 1
 - name: Washington State University, USA
   index: 2
date: 13 February 2024
bibliography: paper.bib
---

# Summary

The `quadkeyr` R package presents a comprehensive toolkit for 
generating raster images from Quadkey-Identified data 
within Microsoft’s Bing Maps Tile System [@bing_maps].
Designed to integrate Quadkey-Identified data into R workflows,
this package facilitates the creation of QuadKey grids and raster images and
introduces specialized functions for 
the processing of Meta (also known as Facebook) Mobility data [@data4good].

The goal of `quadkeyr` is to:

1.  [**Convert a QuadKey to a Simple Features data.frame (and more)**](https://docs.ropensci.org/quadkeyr/articles/quadkey_to_sf_conversion.html) 
`quadkeyr` provides functions to convert QuadKeys
to a `sf` POINT data.frame or `sf` POLYGON data.frame [@sf] (\autoref{fig:fig1}).
Additionally, it offers all the R functions 
described in the [official documentation](https://learn.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system) [@bing_maps]
for converting QuadKeys to and from tiles, pixels, and geographic coordinates.

![Convert a QuadKey to a Simple Features data.frame (and more) \label{fig:fig1}](../vignettes/workflow_quadkey.png){height="200pt"}

2.  [**Generate Raster Images from Quadkey-Identified    Data**](https://docs.ropensci.org/quadkeyr/articles/quadkey_identified_data_to_raster.html) 
Complete a grid of QuadKeys within a specified area and zoom level, 
and create a `stars` raster [@stars]. 
You can also directly convert QuadKeys in a data.frame column 
into an `sf` POLYGON data.frame (\autoref{fig:fig2}).

![Generate a `sf` POLYGON data.frame or a `stars` object from Quadkey-Identified datasets \label{fig:fig2}](../vignettes/workflow_raster.png){height="200pt"}

3. [**Convert Meta (Facebook) Mobility QuadKey-Identified Datasets into Raster Files**](https://docs.ropensci.org/quadkeyr/articles/facebook_mobility_csvs_to_raster_files.html)
Convert Meta (Facebook) mobility data `.csv` files into `.tif` files by day and
hour reported (\autoref{fig:fig3}).

![Convert Meta (Facebook) Mobility QuadKey-Identified Datasets into Raster Files \label{fig:fig3}](../vignettes/workflow_facebook.png){height="200pt"}

4. [**Offer an App for visualizing QuadKeys on a   map**](https://docs.ropensci.org/quadkeyr/articles/quadkey_visualization_app.html)
Introduce a QuadKey visualization application 
enabling users to validate function outcomes.

# Statement of need

## What are QuadKeys in Tile Maps?

Tile maps divide the Earth’s surface into a grid of tiles, 
with each tile corresponding to a specific geographic area 
at various zoom levels.

QuadKeys represent a location on a map by encoding
its hierarchical spatial position
as a sequence of characters \autoref{fig:fig3}. 
They provide an efficient method to address and retrieve specific map tiles, 
facilitating rapid display within mapping applications.

![The QuadKey of any tile starts with the QuadKey of its parent tile (the containing tile at the previous level). Image extracted from Microsoft’s Bing Maps Tile System webpage. \label{fig:fig4}](../vignettes/bing_quadkeys.jpg){height="170pt"}

## QuadKeys, tiles, pixels and geographic coordinates

Tile maps are composed of pixels that are grouped into tiles. 
Later, the tiles are converted to QuadKeys to optimize map performance, 
among other benefits described in detail 
in the [official documentation](https://learn.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system) [@bing_maps].

![Pixels (0, 0) and (2047, 2047) for a map with zoom level 3. Image extracted from Microsoft’s Bing Maps Tile System webpage \label{fig:fig5}](../vignettes/bing_pixel.jpg){height="170pt"}

Pixels and tiles are expressed as two-dimensional coordinates,
(`pixelX`, `pixelY`) and (`tileX`, `tileY`),
but QuadKeys are one-dimensional numeric strings. 
This is important to understand how the conversion works.

Each geographic pair of coordinates (latitude/longitude) 
will belong to a specific pixel referenced by coordinates (`pixelX`, `pixelY`)
for each zoom level. 
In \autoref{fig:fig5}, you can see pixels (0, 0) and (2047, 2047) 
for zoom level 3. 

![Tile coordinates. Image extracted from Microsoft’s Bing Maps Tile System webpage. \label{fig:fig6}](../vignettes/bing_tiles.jpg){height="170pt"}

For instance, a pixel for zoom level 3
represented by the coordinates `pixelX = 255` and `pixelY = 12` 
is part of the tile with coordinates `tileX = 0` and `tileY = 0`. 
The pixel with coordinates (0,0) belongs to tile (0,0), 
and the pixel (2047, 2047) is part of tile (7,7). 
You can verify this by comparing \autoref{fig:fig5} and \autoref{fig:fig6}.

## Converting QuadKeys to and from tiles, pixels, and geographic coordinates

The conversion from QuadKey to geographic coordinates
involves a series of smaller transformations that relate to 
the structure of Tile Maps. All of these intermediary functions, 
as well as those facilitating the reverse conversion, 
are available for use within the `quadkeyr` package.

There are different tools in Python, R and other programming languages 
that help to convert Quadkeys to tiles, pixel or geographic coordinates as well. 
The package `slippymath` [@slippymath] is the sole package
in the R ecosystem dedicated to analyzing tile maps. 
Despite some similarities 
between `quadkeyr` and `slippymath`, 
the packages were developed with different objectives.

The goal of `quadkeyr` is 
to provide a new tool to facilitate the analysis of Meta (Facebook) Mobility data, 
so it has specific functions tailored for this purpose
and is based solely on the Microsoft's Bing Tile Maps documentation.
On the other hand, `slippymath` is a mature and smaller package 
with general tools for analyzing tile maps.
It was designed to be easily incorporated as a 
dependency of higher-level tools.

## Raster Images

Recent changes in the geospatial R ecosystem 
such as the retirement of `rgdal` [@rgdal], `rgeos` [@rgeos] and
`maptools` [@maptools],
affected packages that relied on them,
as `sp` [@sp] and `raster` [@raster],
which have recently ceased being updated. 
[@rspatial-evolution-1;@rspatial-evolution-2]
The `quadkeyr` package addresses this issue 
by adopting the more modern R `stars` package [@stars], 
which converts the `sf` POLYGON data.frame provided as input 
to a `stars` object.
This object can then be saved as a raster image.

## Meta (Facebook) mobility data and Data for Good

Meta (formerly Facebook) provides datasets
in their program Data for good 
that are QuadKey-Identified, 
such as the citizen mobility dataset [@data4good].
To facilitate the processing of these data,
`quadkeyr` offers specific functions 
designed to analyze the `.csv` files 
by date and hour provided and generate `.tif` raster images 
using no more than three functions.
So far, we are not aware of any other package
that provides these functionalities.

# Acknowledgements

This project was made possible by the MIDAS-NIH COVID-19 urgent grant program
and by the cooperative agreement CDC-RFA-FT-23-0069
from the CDC’s Center for Forecasting and Outbreak Analytics.
Its contents are solely the responsibility of the authors and 
do not necessarily represent the official views of
the Centers for Disease Control and Prevention.

We want to acknowledge the ROpenSci community, 
specially Maria Paula Caldas, 
Vincent van Hees and Miles McBain for their review comments.

# References
