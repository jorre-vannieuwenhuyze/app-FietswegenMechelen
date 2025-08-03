library(tidyverse)
library(shiny)
library(leaflet)
library(sf)


if (file.exists("data.Rdata")) {
  load("data.Rdata")
  } else {
  data.points <- tibble(ID=NA,lat=NA,lng=NA) %>% head(-1)
  #data.lines <- tibble()
  }



#data <- tibble(geom = st_sfc(st_point(c(4.480325,51.028211)))) %>% st_as_sf()




# -------------------------------------------------------------------------
# UI ----------------------------------------------------------------------
# -------------------------------------------------------------------------

ui <- bootstrapPage(
   tags$style(type = "text/css", "html, body {width:100%;height:100%}")
  ,leafletOutput("map",height="100%",width="100%")
  ,absolutePanel(top=10,right=10,style="background-color: white;padding: 20px;"
    ,radioButtons("type","Wegtype",c("Fietsstraat"="fietsstraat","30km/u weg"="30straat","50km/u weg"="50straat"))
    ,actionButton("save","Save"),br()
    ,actionButton("del","Delete last point"),br()
    ,verbatimTextOutput("text")
    )
  )







# -------------------------------------------------------------------------
# SERVER ------------------------------------------------------------------
# -------------------------------------------------------------------------

server <- function(input, output, session) {
  rv <- reactiveValues(
     data.points=data.points
    ,pointID = ifelse(nrow(data.points)==0,0,as.numeric(max(data.points$ID)))
    ,selectedpoints = tibble()
    )
  output$map <- renderLeaflet({
    ( leaflet() 
    %>% setView(lng=4.480325,lat=51.028211,zoom=14) 
    %>% addProviderTiles(providers$CartoDB.Positron) 
    #%>% addCircleMarkers(data=rv$data.points)
    )
    })  
  #clicklist <- reactiveVal(list()) # empty list
  observeEvent(input$map_click, {
    leafletProxy("map",session) %>% addCircleMarkers(lng=input$map_click$lng,lat=input$map_click$lat)
    n = nrow(rv$selectedpoints)
    if (n>0) {
      prevpoint = rv$selectedpoints[n,]
      leafletProxy("map",session) %>% addPolylines(lng=c(prevpoint$lng,input$map_click$lng),lat =c(prevpoint$lat,input$map_click$lat)) 
      }
    rv$pointID = rv$pointID+1
    rv$selectedpoints = bind_rows(rv$selectedpoints,tibble(ID=sprintf("p%06d",rv$pointID),lng=input$map_click$lng,lat=input$map_click$lat,new=TRUE))
    # geom <- ( c(input$map_click$lng,input$map_click$lat)
    #   %>% st_point()        
    #   )
    #pointdata <- tibble(geom = st_sfc(st_point(c(input$map_click$lng,input$map_click$lat)))) %>% st_as_sf()
    #rv$path <- bind_rows(rv$path,pointdata)
    })
  
  
  output$text <- renderPrint({as.data.frame(rv$selectedpoints)})

  }


#leafletProxy("map",session) %>% clearMarkers()




# -------------------------------------------------------------------------
# APP ---------------------------------------------------------------------
# -------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
