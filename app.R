library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


ui <- mainPanel(
  titlePanel("Explorador cartográfico"),
  
  div(class = "outer",
      
  tags$head(
    # Include our custom CSS
    includeCSS("styles.css")
  ),
  
  leafletOutput("mymap", width = "100%", height = "100%")
  
  , absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
      width = 330, height = "auto",
  
      h3("Selector de capas"),
  
      checkboxInput("zipcode",  "Código Postal", value = FALSE),
      
      h4("Secciones y distritos censales"),
      
      checkboxInput("seccion",  "Sección censal", value = FALSE),
      checkboxInput("distrito", "Distrito censal", value = FALSE),
      
      h3("Información catastral"),
      
      checkboxInput("catastro_parcela", "Parcela", value = FALSE),
      checkboxInput("catastro_subparce", "Subparcela", value = FALSE),
      checkboxInput("catastro_masa", "Masa", value = FALSE),
      checkboxInput("catastro_constru", "Construcción", value = FALSE)
    )
  )
)

server <- function(input, output, session) {

  output$mymap <- renderLeaflet({
    m <- leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(lat = 41, lng = -3.4, zoom = 7)
    
  })
  
  observe({
    
    proxy <- leafletProxy("mymap") %>% clearTiles() %>%
      addProviderTiles(providers$OpenStreetMap.HOT)
    # addProviderTiles(providers$Stamen.TonerLite,
    #                    options = providerTileOptions(noWrap = TRUE))
    
    if (input$zipcode){
      proxy <- proxy %>% addWMSTiles(
          "http://www.ign.es/wms-inspire/ign-base",
          layers = "codigo-postal",
          options = WMSTileOptions(format = "image/png", transparent = TRUE),
          tileOptions(tms = TRUE),
          attribution = "")
    }
    
    if (input$seccion){
      proxy <- proxy %>% addWMSTiles(
        "http://servicios.internet.ine.es/WMS/WMS_INE_SECCIONES_G01/MapServer/WMSServer",
        layers = "2018_Secciones",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        tileOptions(tms = TRUE),
        attribution = "")
    }
    
    if (input$distrito){
      proxy <- proxy %>% addWMSTiles(
        "http://servicios.internet.ine.es/WMS/WMS_INE_SECCIONES_G01/MapServer/WMSServer",
        layers = "2018_Distritos",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        tileOptions(tms = TRUE),
        attribution = "")
    }
    
    if (input$catastro_parcela){
      proxy <- proxy %>% addWMSTiles(
        "http://ovc.catastro.meh.es/Cartografia/WMS/ServidorWMS.aspx",
        #layers = "PARCELA,TXTPARCELA",
        layers = "PARCELA",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        tileOptions(tms = TRUE),
        attribution = "")
    }
    
    if (input$catastro_masa){
      proxy <- proxy %>% addWMSTiles(
        "http://ovc.catastro.meh.es/Cartografia/WMS/ServidorWMS.aspx",
        layers = "MASA,TXTMASA",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        tileOptions(tms = TRUE),
        attribution = "")
    }
    
    if (input$catastro_subparce){
      proxy <- proxy %>% addWMSTiles(
        "http://ovc.catastro.meh.es/Cartografia/WMS/ServidorWMS.aspx",
        layers = "SUBPARCE,TXTSUBPARCE",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        tileOptions(tms = TRUE),
        attribution = "")
    }
    
    if (input$catastro_constru){
      proxy <- proxy %>% addWMSTiles(
        "http://ovc.catastro.meh.es/Cartografia/WMS/ServidorWMS.aspx",
        layers = "CONSTRU,TXTCONSTRU",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        tileOptions(tms = TRUE),
        attribution = "")
    }
    
    proxy
    
  })
    
}

shinyApp(ui, server)
