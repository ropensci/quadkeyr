
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quadkeyr <img src="vignettes/logo.png" align="right" height="150"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/ropensci/quadkeyr/actions/workflows/check-release.yaml/badge.svg)](https://github.com/ropensci/quadkeyr/actions/workflows/check-release.yaml)
[![pkgcheck](https://github.com/ropensci/quadkeyr/workflows/pkgcheck/badge.svg)](https://github.com/%3Corg%3E/%3Crepo%3E/actions?query=workflow%3Apkgcheck)
[![Codecov](https://img.shields.io/codecov/c/github/ropensci/quadkeyr)](https://codecov.io/gh/Fernandez-Lab-WSU/quadkeyr)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/619_status.svg)](https://github.com/ropensci/software-review/issues/619)
[![status](https://joss.theoj.org/papers/3301002821465f4f160e7ce272c00c6f/status.svg)](https://joss.theoj.org/papers/3301002821465f4f160e7ce272c00c6f)
[![DOI](https://zenodo.org/badge/727433503.svg)](https://zenodo.org/doi/10.5281/zenodo.10835287)
<!-- badges: end -->

### What can this package do for you?

The `quadkeyr` R package presents a comprehensive toolkit tailored for
generating raster images from Quadkey-Identified data within
[Microsoft’s Bing Maps Tile
System](https://learn.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system).
Designed to integrate Quadkey-Identified data into R workflows, this
package facilitates the creation of QuadKey grids and raster images and
introduces specialized functions for the processing of [Meta Mobility
data, previously referred to as Facebook mobility
data](https://dataforgood.facebook.com/).

### What are QuadKeys in Tile Maps?

Tile maps divide the Earth’s surface into a grid of tiles, with each
tile corresponding to a specific geographic area at various zoom levels.

QuadKeys represent a location on a map by encoding its hierarchical
spatial position as a sequence of characters. They provide an efficient
method to address and retrieve specific map tiles, facilitating rapid
display within mapping applications.

<img src="https://github.com/ropensci/quadkeyr/blob/main/docs/articles/bing_quadkeys.jpg?raw=true" width="70%" style="display: block; margin: auto;" />
<p style="text-align: center">
The QuadKey of any tile starts with the QuadKey of its parent tile (the
containing tile at the previous level). Image extracted from Microsoft’s
Bing Maps Tile System webpage.
</p>

------------------------------------------------------------------------

The goal of `quadkeyr` is to:

1.  [**Convert a QuadKey to a Simple Features data.frame (and
    more)**](https://docs.ropensci.org/quadkeyr/articles/quadkey_to_sf_conversion.html)
    `quadkeyr` provides functions to convert a QuadKey to a `sf` POINT
    data.frame or `sf` POLYGON data.frame. Additionally, it offers all
    the R functions described in the [official
    documentation](https://learn.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system)
    for converting QuadKeys to and from tiles, pixels, and geographic
    coordinates.

<img src="vignettes/workflow_quadkey.png" width="80%" style="display: block; margin: auto;" />

2.  [**Generate Raster Images from Quadkey-Identified
    Data**](https://docs.ropensci.org/quadkeyr/articles/quadkey_identified_data_to_raster.html)
    Complete a grid of QuadKeys within a specified area and zoom level,
    and create a `stars` raster. You can also directly convert QuadKeys
    in a data.frame column into an `sf` POLYGON data.frame.

<img src="vignettes/workflow_raster.png" width="80%" style="display: block; margin: auto;" />

3.  [**Convert Meta (Facebook) Mobility QuadKey-identified Datasets into
    Raster
    Files**](https://docs.ropensci.org/quadkeyr/articles/facebook_mobility_csvs_to_raster_files.html)
    Convert Meta (Facebook) mobility data `.csv` files into `.tif` files
    by day and hour reported.

<img src="vignettes/workflow_facebook.png" width="80%" style="display: block; margin: auto;" />

4.  [**Offer an App for visualizing QuadKeys on a
    map**](https://docs.ropensci.org/quadkeyr/articles/quadkey_visualization_app.html)
    Introduce a QuadKey visualization application enabling users to
    validate function outcomes.

## Install `quadkeyr`

### R-Universe

For the latest development version of `quadkeyr`:

``` r
install.packages("quadkeyr",
                 repos = "https://ropensci.r-universe.dev")
```

### GitHub

You can install the development version of `quadkeyr` from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("Fernandez-Lab-WSU/quadkeyr")
```

## Code of Conduct

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.

## Contribute

If you’d like to contribute to this project, please follow the
[contributing
guidelines](https://github.com/ropensci/quadkeyr/blob/main/.github/CONTRIBUTING.md)

## Links of interest

- [Bing Maps Tile Systems -
  Microsoft](https://learn.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system)
- [R - `slippymath`: Slippy Map Tile
  Tools](https://cran.r-project.org/web/packages/slippymath/index.html)
- [Python - `pyquadkey2`](https://docs.muetsch.io/pyquadkey2/)
- [Python - `python-quadkey`](https://github.com/CartoDB/python-quadkey)
- [Python - `mercantile`](https://pypi.org/project/mercantile/)
- [Rust - `geo-quadkey-rs`](https://lib.rs/crates/geo-quadkey-rs)
- [TypeScript -
  `quadkey-tilemath`](https://github.com/glassonion1/quadkey-tilemath)
- [GeoCode Map viewer](https://tools.9revolution9.com/geo/geocode/)
