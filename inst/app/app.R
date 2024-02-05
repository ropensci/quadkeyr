qkmap_app <- function(...){
  ui <- bslib::page_navbar(
    title = "QuadKey Visualizer App",
    bg = "#ffffff",
    theme = bslib::bs_theme(version = 5,
                            bootswatch = "simplex",
                            base_font = bslib::font_google("Raleway", 
                                                           wght = "400"),
                            heading_font = bslib::font_google("Raleway",
                                                              wght = "200")),
    bslib::nav_panel(title = 'Quadkey',
                     bslib::layout_sidebar(class = 'p-0',
                                           sidebar = bslib::sidebar(width = 300,
                                                                    shiny::textInput('qk',
                                                                                     'Visualising a QuadKey',
                                                                                     placeholder = "complete and click search"),
                                                                    shiny::actionButton('search', 'Search'),
                                                                    shiny::hr(),
                                                                    DT::DTOutput('qkvalues')),
                                           leaflet::leafletOutput('mapqk'))
    ),
    bslib::nav_panel(title = 'Creating a QuadKey grid',
                     bslib::layout_sidebar(class = 'p-0',
                                           sidebar = bslib::sidebar(
                                             shiny::p(paste("Write the bounding box",
                                                            "coordinates in decimal degrees")),
                                             bslib::layout_columns(
                                               shiny::textInput('xmin', 'xmin'),
                                               shiny::textInput('ymin', 'ymin')
                                             ),
                                             bslib::layout_columns(
                                               shiny::textInput('xmax', 'xmax'),
                                               shiny::textInput('ymax', 'ymax')
                                             ),
                                             shiny::sliderInput('zoom',
                                                                'Select the zoom level:',
                                                                min = 1,
                                                                max = 23,
                                                                value = 6),
                                             shiny::actionButton('grid',
                                                                 'Create Grid'),
                                             shiny::hr(),
                                             textOutput("qk_nr"),
                                             shiny::hr(),
                                             shiny::p(paste("Note that for higher zoom levels,",
                                                            "it is preferable to use smaller areas",
                                                            " (< 2000 QuadKeys)",
                                                            "to prevent long processing times.")
                                             )),
                                           leaflet::leafletOutput('mapgrid')))
  )
  
  server <- function(input, output, session){
    
    # TAB 1: QuadKey
    # Get the QuadKey Coorinates
    qk_results <-  shiny::eventReactive(input$search,
                                        ignoreNULL = FALSE,{
                                          shiny::validate(
                                            shiny::need(input$qk,
                                                        "Please, write a QuadKey number and press 'Search'")
                                          )
                                          tile <- quadkeyr::quadkey_to_tileXY(as.character(input$qk))
                                          
                                          pixel <- quadkeyr::tileXY_to_pixelXY(tileX = tile$tileX,
                                                                               tileY = tile$tileY)
                                          
                                          coords <- quadkeyr::pixelXY_to_latlong(pixelX = pixel$pixelX,
                                                                                 pixelY = pixel$pixelY,
                                                                                 zoom = tile$zoom)
                                          
                                          return(list(tileX = tile$tileX ,
                                                      tileY = tile$tileY,
                                                      zoom = tile$zoom,
                                                      pixelX = pixel$pixelX,
                                                      pixelY = pixel$pixelY,
                                                      lat = coords$lat,
                                                      lon = coords$lon))
                                        })
    # Data table with QuadKey Info
    output$qkvalues <- DT::renderDT({
      
      dataqk <- t(data.frame(c('TileX',
                               qk_results()$tileX),
                             c('TileY',
                               qk_results()$tileY),
                             c('Zoom Level',
                               qk_results()$zoom),
                             c('PixelX',
                               qk_results()$pixelX),
                             c('PixelY',
                               qk_results()$pixelY),
                             c('Latitude',
                               qk_results()$lat),
                             c('Longitude',
                               qk_results()$lon)))
      
      DT::datatable(dataqk,
                    rownames = FALSE,
                    # remove table interactive default options
                    options = list(ordering = FALSE,
                                   dom = 't'),
                    colnames = rep("", ncol(dataqk))) 
      
    })
    
    qk_polygon <- shiny::eventReactive(input$search,
                                       ignoreNULL = FALSE,{
                        shiny::validate(
        shiny::need(input$qk,
                    "Please, write a QuadKey number and press 'Search'")
      )
                       quadkey_to_polygon(as.character(input$qk))                  
                       })
    
    # Quadkey location map
    output$mapqk <- leaflet::renderLeaflet({
      # get polygon bounding box
      bbx <-  sf::st_bbox(qk_polygon())
      # Map tab 1, QuadKey polygon and upper-left corner coordinate
      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::addMeasure(primaryLengthUnit="kilometers",
                            secondaryLengthUnit="kilometers") |> 
        leaflet::addPolygons(data = qk_polygon(),
                             group = 'polygon',
                             layerId = ~quadkey, # https://rstudio.github.io/leaflet/showhide.html
                             color = "red",
                             fillColor = NA,
                             label =  ~as.character(input$qk)) |> 
        leaflet::addMarkers(data = qk_results(),
                            lat = ~lat,
                            lng = ~lon,
                             ~paste(qk_results()$lat, 
                                           qk_results()$lon),
                            label =  ~as.character(input$qk)) |>
        leaflet::fitBounds(lng1 = bbx$xmin[[1]],
                           lat1 = bbx$ymin[[1]],
                           lng2 = bbx$xmax[[1]],
                           lat2 = bbx$ymax[[1]])
    })
    
    
    # TAB 2: Grid  
    grid_selected <- shiny::eventReactive(input$grid,
                                 ignoreNULL = FALSE,{
                                   # Warning to the user: all the fields should be complete
                                   shiny::validate(
                                     shiny::need(input$xmin != '' && 
                                                   input$ymin != '' &&
                                                   input$xmax != '' && 
                                                   input$ymax != '',   
                                                 "Please, complete the fields and press 'Search'")
                                   )
                                   griduser <- quadkeyr::create_qk_grid(xmin = as.numeric(input$xmin),
                                                                        xmax = as.numeric(input$xmax),
                                                                        ymin = as.numeric(input$ymin),
                                                                        ymax = as.numeric(input$ymax),
                                                                        zoom = as.numeric(input$zoom))
                                   griduser$data
                                   })
    
    output$qk_nr <- renderText({
      
      paste("Number of QuadKeys in the grid:", nrow(grid_selected()))
      
    })
    
    polygrid <- reactive({
      
      # Inform the user about potential high running times
      if(nrow(grid_selected()) > 2000){
        shiny::showNotification(
          duration = NULL,
          paste("The grid you want to create has",
                "more than 2000 polygons",
                "and could take considerable time",
                "to run"),
          type = "message"
        )}

      grid_coords <- quadkeyr::get_qk_coord(data = grid_selected())
      polygrid <-  quadkeyr::grid_to_polygon(grid_coords)
      polygrid
    })
    
    
    # QuadKey grid map
    output$mapgrid <- leaflet::renderLeaflet({
      
      shiny::req(polygrid())
      
      leaflet::leaflet() |>
        leaflet::addMeasure(primaryLengthUnit="kilometers",
                            secondaryLengthUnit="kilometers") |> 
        leaflet::addTiles() |>
        leaflet::addPolygons(data = polygrid(),
                             color = 'red',
                             layerId = ~as.character(quadkey),
                             group = 'qks',
                             popup = ~quadkey)
    })}
  shiny::shinyApp(ui = ui, server = server)
}
qkmap_app()

