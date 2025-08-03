library(leaflet)

load('streets.Rdata')



ui <- fluidPage(leafletOutput("map",height=600)
               ,textOutput("osm_ids")
               ,actionButton("remove", "Remove all")
               )

server <- function(input, output, session) {
  
  selected <- reactiveValues(osm_id = vector())  
  

  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addPolylines(data=streets,layerId=streets$osm_id)
  })
  
  observeEvent(input$map_shape_click, {
   click <- input$map_shape_click
   proxy <- leafletProxy("map")
   if (! click$id %in% selected$osm_id) {
     selected$osm_id <- c(selected$osm_id, click$id)
     selectedstreets <- streets[streets$osm_id %in% selected$osm_id,]
     proxy %>% addPolylines(data=selectedstreets,layerId=selectedstreets$osm_id,color="red",opacity=1)
     #print(selected$osm_ids)
     }
   else {
     selected$osm_id <- selected$osm_id[selected$osm_id!=click$id]
     removedstreet   <- streets[streets$osm_id %in% click$id,]
     proxy %>% removeShape(layerId=click$id) %>% addPolylines(data=removedstreet,layerId=removedstreet$osm_id)
     }
   })
  
  output$osm_ids <- renderText(paste(selected$osm_id,collapse=','))
  
  observeEvent(input$remove, {
    proxy <- leafletProxy("map")
    removedstreet   <- streets[streets$osm_id %in% selected$osm_id,]
    proxy %>% removeShape(layerId=selected$osm_id) %>% addPolylines(data=removedstreet,layerId=removedstreet$osm_id)
    selected$osm_id <- vector()
  })
  
}


shinyApp(ui = ui, server = server)



