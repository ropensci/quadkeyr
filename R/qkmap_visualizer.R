library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(dplyr)
library(DT)

qkmap_app <- function(...) {
  ui <- bslib::page_navbar(
    title = "QuadKey Map Visualizer",
    bg = "#ffffff",
    theme = bslib::bs_theme(version = 5,
                            bootswatch = "simplex",
                            base_font = bslib::font_google("Raleway", wght = "400"),
                            heading_font = bslib::font_google("Raleway", wght = "200")),
    bslib::nav_panel(title = 'Quadkey',
              bslib::layout_sidebar(class = 'p-0',
                     sidebar = bslib::sidebar(width = 300,
        shiny::textInput('qk',
                  'Please, insert the QuadKey',
                  placeholder = "complete and click search"),
        shiny::actionButton('search', 'Search'),
        shiny::hr(),
        DT::DTOutput('qkvalues')

      ),
      leaflet::leafletOutput('mapqk'))

    ),
    bslib::nav_panel(title = 'Grid',
           bslib::layout_sidebar(class = 'p-0',
                     sidebar = bslib::sidebar(
                       shiny::p("Write the coordinates in decimal degrees"),

            bslib::layout_columns(
          shiny::textInput('xmin', 'xmin'),
          shiny::textInput('ymin', 'ymin')
        ),
        bslib::layout_columns(
          shiny::textInput('xmax', 'xmax'),
          shiny::textInput('ymax', 'ymax')
        ),
        shiny::sliderInput('levelofdetail',
                    'Select the level of detail:',
                    min = 1,
                    max = 23,
                    value = 6),
        shiny::actionButton('grid', 'Create Grid'),
        shiny::hr(),
        shiny::p("Note that for higher levels of detail, smaller areas are preferable to prevent long processing times.")

      ),
      #autoWaiter(),
      leaflet::leafletOutput('mapgrid')))


  )
  server <- function(input, output, session) {

   qk_results <-  shiny::eventReactive(input$search,{

    tile = quadkey_to_tileXY(input$qk)
    pixel = tileXY_to_pixelXY(tile$tileX,
                          tile$tileY)
    coords = pixelXY_to_latlong(pixel$pixelX,
                              pixel$pixelY,
                              level = tile$level)


      return(list(tileX = tile$tileX ,
                  tileY = tile$tileY,
                  level = tile$level,
                  pixelX = pixel$pixelX,
                  pixelY = pixel$pixelY,
                  lat = coords$lat,
                  lon = coords$lon))
    })

    output$qkvalues <- DT::renderDT({

        shiny::req(input$search)

        dataqk <- t(data.frame(c('TileX',
                                 qk_results()$tileX),
                               c('TileY',
                                 qk_results()$tileY),
                               c('Level of Detail',
                                 qk_results()$level),
                               c('PixelX',
                                 qk_results()$pixelX),
                               c('PixelY',
                                 qk_results()$pixelY),
                               c('Latitude',
                                 qk_results()$lat),
                               c('Longitude',
                                 qk_results()$lon)))

        colnames(dataqk) <- c('',as.character(input$qk))

        DT::datatable(dataqk,
                      rownames= FALSE,
                      options = list(ordering = FALSE,
                                     dom = 't'), # remove table interactive default options
                      colnames = rep("", ncol(dataqk))) # remove column names

    })

    output$mapqk <- leaflet::renderLeaflet({



      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::addMarkers(data = qk_results(),
                   lat = ~lat,
                   lng = ~lon,
                   popup = ~paste(qk_results()$lat, qk_results()$lon),
                   label =  ~as.character(input$qk)) |>
        leaflet::setView(zoom = 6,
                lat = qk_results()$lat,
                lng = qk_results()$lon)

    })


    polygrid <- shiny::eventReactive(input$grid,{

     grid <- create_qk_grid(xmin = as.numeric(input$xmin),
                                xmax = as.numeric(input$xmax),
                                ymin = as.numeric(input$ymin),
                                ymax = as.numeric(input$ymax),
                                level = as.numeric(input$levelofdetail))


     if(nrow(grid$data) > 2000){

       shiny::showNotification(
         "The grid you want to create have more than 2000 polygons and could take considerable time to run",
         type = "message"
       )
     }

      grid_coords <- extract_qk_coord(data = grid$data)

      polygrid <-  grid_to_polygon(grid_coords)

      polygrid

    })

    output$mapgrid <- leaflet::renderLeaflet({

      shiny::req(polygrid())

      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::addPolygons(data = polygrid(),
                    color = 'red',
                    layerId = ~as.character(quadkey),
                    group = 'qks',
                    popup = ~quadkey)
        # addMarkers(data = qk_results(),
        #            lat = ~lat,
        #            lng = ~lng,
        #            popup = ~paste(qk_results()$lat, qk_results()$lng),
        #            label =  ~as.character(input$qk)) |>
        # setView(zoom = 6,
        #         lat = qk_results()$lat,
        #         lng = qk_results()$lng)

    })

    }

  shiny::shinyApp(ui, server) }
