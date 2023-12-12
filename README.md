
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quadkeyr <img src="man/figures/logo.png" align="right" height="200" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![pkgcheck](https://github.com/Fernandez-Lab-WSU/quadkeyr/actions/workflows/pkgcheck.yaml/badge.svg)](https://github.com/Fernandez-Lab-WSU/quadkeyr/actions/workflows/pkgcheck.yaml)
[![R-CMD-check](https://github.com/Fernandez-Lab-WSU/quadkeyr/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/Fernandez-Lab-WSU/quadkeyr/actions/workflows/check-standard.yaml)
[![Codecov](https://img.shields.io/codecov/c/github/Fernandez-Lab-WSU/quadkeyr)](https://codecov.io/gh/Fernandez-Lab-WSU/quadkeyr)
<!-- badges: end -->

⚠️ This package is under review

The `quadkeyr` R package presents a comprehensive toolkit tailored for
generating raster images from QuadKeys within [Microsoft’s Bing Maps
Tile
System](https://learn.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system).
Designed to integrate Bing Maps data into R workflows, this package
facilitates the creation of QuadKey grids and rasters and introduces
specialized functions for the processing of [Facebook Mobility
data](https://dataforgood.facebook.com/).

------------------------------------------------------------------------

The goal of `quadkeyr` is to:

1.  [**Provide Bing Maps Tile System
    functions**](https://fernandez-lab-wsu.github.io/quadkeyr/articles/quadkey_conversion.html)
    R functions to convert geographical coordinates to and from QuadKeys
    as described in the [official
    documentation](https://learn.microsoft.com/en-us/bingmaps/articles/bing-maps-tile-system)

2.  [**Create rasters based on
    QuadKeys**](https://fernandez-lab-wsu.github.io/quadkeyr/articles/get_grid_from_quadkeys.html)
    Complete a grid of QuadKeys within a specified area and level of
    detail, and create a raster from them.

3.  [**Generate rasters from Facebook mobility
    data**](https://fernandez-lab-wsu.github.io/quadkeyr/articles/create_rasters_from_grid.html)
    Adapt the functions described in points 1 and 2 to effectively
    process and analyze Facebook mobility data.

4.  Introduce a QuadKey map application enabling users to validate
    function outcomes and offering tools for visualizing the generated
    data.

## Installation

You can install the development version of `quadkeyr` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Fernandez-Lab-WSU/quadkeyr")
```

## Code of Conduct

Please note that the `quadkeyr` project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

### References

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