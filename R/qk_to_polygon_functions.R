#' Convert a QuadKey into a square polygon
#'
#' This functions creates a `sf` POLYGON data.frame from a QuadKey string.
#'
#' @param quadkey The QuadKey as a string
#'
#' @seealso \code{\link{quadkey_df_to_polygon}}
#'
#' @return A `sf` POLYGON data.frame with a `quadkey` and `geometry` column.
#' @export
#'
#' @examples
#'
#' # Quadkey as string
#' quadkey_to_polygon(quadkey = "213")
#'
#' # QuadKeys as column in a data.frame
#' # get data file
#' path <- paste0(
#'   system.file("extdata", package = "quadkeyr"),
#'   "/cityA_2020_04_15_0000.csv"
#' )
#' data <- read.csv(path)
#' data <- format_fb_data(data)
#'
#' quadkey_df_to_polygon(data = data)
#'
quadkey_to_polygon <- function(quadkey) {
  # The conversion to character is not straightfoward
  # as there could be leading zeros or scientific notation
  if (!is.character(quadkey) || length(quadkey) != 1) {
    stop("Please provide a QuadKey as a single string")
  }
  if (!grepl("[0-9]+", quadkey) |
    any(!unlist(strsplit(quadkey, "")) %in%
      c("0", "1", "2", "3"))) {
    stop("QuadKeys can contain only the numbers '0', '1', '2', or '3'")
  }

  tileX <- quadkey_to_tileXY(quadkey)$tileX
  tileY <- quadkey_to_tileXY(quadkey)$tileY

  # I add the other 3 points I need to complete the polygon
  # using the upper left coordinates of the closer tiles.
  polygon <- expand.grid(
    tileX = c(tileX, tileX + 1),
    tileY = c(tileY, tileY + 1)
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      pixelX = tileXY_to_pixelXY(
        .data$tileX, # conversion to coords
        .data$tileY
      )$pixelX,
      pixelY = tileXY_to_pixelXY(
        .data$tileX,
        .data$tileY
      )$pixelY
    ) |>
    dplyr::mutate(
      lat = pixelXY_to_latlong(
        .data$pixelX,
        .data$pixelY,
        nchar(quadkey)
      )$lat,
      lon = pixelXY_to_latlong(
        .data$pixelX,
        .data$pixelY,
        nchar(quadkey)
      )$lon
    ) |>
    sf::st_as_sf(
      coords = c("lon", "lat"), # class sf
      crs = 4326
    )

  # Create polygon https://github.com/r-spatial/sf/issues/243
  quadkey_polygon <- polygon |>
    sf::st_bbox() |>
    sf::st_as_sfc() |>
    sf::st_sf() |>
    sf::st_set_geometry("geometry") |>
    cbind(quadkey)

  return(quadkey_polygon)
}

#' Convert data.frame with `quadkey` column to a `sf` POLYGON data.frame
#'
#' @param data A data.frame with a `quadkey` column
#'
#' @seealso \code{\link{quadkey_df_to_polygon}}
#'
#' @return The same original data.frame with a `sf` POLYGON data.frame with a
#' `geometry` column.
#'
#' @export
#'
#' @examples
#'
#' # Quadkey as string
#' quadkey_to_polygon(quadkey = "213")
#'
#' # QuadKeys as column in a data.frame
#' # get data file
#' path <- paste0(
#'   system.file("extdata", package = "quadkeyr"),
#'   "/cityA_2020_04_15_0000.csv"
#' )
#' data <- read.csv(path)
#' data <- format_fb_data(data)
#'
#' quadkey_df_to_polygon(data = data)
quadkey_df_to_polygon <- function(data) {
  # The conversion to character is not straightfoward
  # as there could be leading zeros or scientific notation
  if (!is.character(data$quadkey)) {
    stop("quadkey should be a character column")
  }
  if (length(unique(nchar(data$quadkey))) > 1) {
    stop("All the QuadKeys should have the same number of digits")
  }
  if (!all(grepl("[0-9]+", unique(data$quadkey))) |
    any(!unlist(strsplit(unique(data$quadkey), "")) %in%
      c("0", "1", "2", "3"))) {
    stop("QuadKeys can contain only the numbers '0', '1', '2', or '3'")
  }


  data_sf <- data |>
    dplyr::rowwise() |>
    dplyr::mutate(geometry = quadkey_to_polygon(.data$quadkey)$geometry) |>
    as.data.frame() |>
    sf::st_as_sf()

  return(data_sf)
}
