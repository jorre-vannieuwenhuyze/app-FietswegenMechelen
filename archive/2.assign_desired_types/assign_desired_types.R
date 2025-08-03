setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

library(shiny)
library(leaflet)
library(tidyverse)
library(sf)

#######################################################
#### FUNCTIONS
#######################################################

col2hex <- function(cname) {
  colMat <- col2rgb(cname)
  rgb(red=colMat[1,]/255,green=colMat[2,]/255,blue = colMat[3,]/255)
}
bind_rows_sf <- function(...){
  sf_list <- rlang::dots_values(...)[[1]]
  sfg_list_column <- lapply(sf_list, function(sf) sf$geometry[[1]]) %>% st_sfc
  df <- lapply(sf_list, function(sf) st_set_geometry(sf, NULL)) %>% bind_rows
  sf_appended <- st_sf(data.frame(df, geom=sfg_list_column))
  return(sf_appended)
}


#######################################################
#### LOAD DATA
#######################################################


#### Gewenste straten

gewenst <- tribble(
  ~type,~color
  ,'Voetgangerszone','blue4'  
  ,'Fietsweg'       ,'red4' 
  ,'Fietsstraat'    ,'red3'
  ,'Zone 30'        ,'yellow2'
  ,'Zone 50'        ,'yellow3'
  ,'Zone 70'        ,'yellow4'
  ,'------'         ,'gray70'                 
)
gewenst$color <- col2hex(gewenst$color)



#### Criteria
criteria <- tribble(
 ~criterium
,'Comfort'
,'Snelheid'
,'Aangenaamheid'
,'Duidelijkheid'
,'Totaal'
)





#### Database

load('../1.get_osm_data/output/streets.all.Rdata')
streets.all$gewenst <- factor("------",rev(gewenst$type))
if(file.exists("output/streets.gewenst.Rdata")){
  load('output/streets.gewenst.Rdata')
  streets.all.ids     <- streets.all$osm_id
  streets.gewenst.ids <- streets.gewenst$osm_id
  streets.all         <- filter(streets.all,!osm_id %in% streets.gewenst.ids)
  streets.gewenst     <- filter(streets.gewenst,osm_id %in% streets.all.ids)
  streets.all         <- rbind(streets.all,streets.gewenst)
  }
streets.all         <- arrange(streets.all,gewenst)

getcolor   <- function(x){unname(setNames(gewenst$color,gewenst$type)[as.character(x)])}
getopacity <- function(x){ifelse(x=="------",.2,1)}
#streets.all$color   <- getcolor(streets.all$gewenst)
#streets.all$opacity <- getopacity(streets.all$gewenst)


#######################################################
### MAP IONS
#######################################################


addStreets <- function(map,data,layerId=~osm_id,label=~paste(naam,osm_id),col=~getcolor(gewenst),opacity=~getopacity(gewenst),...) {
  addPolylines(map=map,data=data,layerId=layerId,label=label,col=col,opacity=opacity,...)
  }


#######################################################
### UI
#######################################################

ui <- bootstrapPage(
   tags$style(type = "text/css", "html, body {width:100%;height:100%}")
  ,leafletOutput("map",height="100%",width="100%")
  ,absolutePanel(top=10,right=10,style="background-color: white;padding: 20px;"
    ,radioButtons("situatie","Wat wens je te zien?",choices=list("Gewenste situatie"='gewenst',"Bestaande situatie"='bestaand'),selected='gewenst')             
    ,conditionalPanel(condition="input.situatie=='gewenst'"
      ,radioButtons(inputId="gewenst",label="Gewenst type:",choices=gewenst$type)
      ,uiOutput("nieuwenaam")
      ,actionButton("save", "Save")
      )
    ,conditionalPanel(condition = "input.situatie=='bestaand'"
      ,lapply(criteria$criterium, function(i) {sliderInput(i,i,0,10,5)})
      ,textAreaInput("motivatie","")
      ,actionButton("save", "Save")
      )
    )
  ,absolutePanel(bottom=10,left=10
    ,actionButton("remove", "Remove all selected streets")
    )
  )




#######################################################
### SERVER
#######################################################






server <- function(input, output, session) {
  rv <- reactiveValues(allstreets = streets.all
                      ,selectedstreets = tibble()
                      ,weight=2
                      )  
  ### BASIC MAP
  output$map <- renderLeaflet({
    ( leaflet() 
    %>% setView(lng=4.480325,lat=51.028211,zoom=14) 
    %>% addProviderTiles(providers$CartoDB.Positron) 
    %>% addStreets(rv$allstreets,weight=rv$weight)
    )
  })
  ### ZOOM BEHAVIOR
  # observeEvent(input$map_zoom, {
  #   rv$weight = case_when(input$map_zoom <=12 ~1 
  #                        ,input$map_zoom ==13 ~2
  #                        ,input$map_zoom ==14 ~3
  #                        ,input$map_zoom ==15 ~4 
  #                        ,input$map_zoom ==16 ~5 
  #                        ,input$map_zoom ==17 ~7 
  #                        ,input$map_zoom ==18 ~9 
  #                        ,input$map_zoom ==19 ~14 
  #                        ,input$map_zoom >=20 ~21)
  #   leaflet <- leafletProxy("map") %>% clearShapes() %>% addStreets(rv$allstreets,weight=rv$weight) 
  #   if (nrow(rv$selectedstreets)!=0) { leaflet <- leaflet %>% addStreets(rv$selectedstreets,col="magenta",opacity=1,weight=rv$weight) }
  #   leaflet
  # })
  ### SELECT STREET
  observeEvent(input$map_shape_click, {
    clickid <- input$map_shape_click$id
    if (nrow(rv$selectedstreets)==0) {
      rv$selectedstreets <- filter(rv$allstreets,osm_id==clickid)
      leafletProxy("map") %>% addStreets(rv$selectedstreets,col="magenta",opacity=1,weight=rv$weight)
    }
    else if (! clickid %in% rv$selectedstreets$osm_id) {
      rv$selectedstreets <- rbind(rv$selectedstreets,filter(rv$allstreets,osm_id==clickid))
      leafletProxy("map") %>% addStreets(rv$selectedstreets,col="magenta",opacity=1,weight=rv$weight)
    }
    else {
      rv$selectedstreets <- filter(rv$selectedstreets,osm_id!=clickid)
      removedstreet      <- filter(rv$allstreets,osm_id %in% clickid)
      leafletProxy("map") %>% addStreets(removedstreet,weight=rv$weight)
    }
  })
  ### TEXT FOR SELECTED STREETS
  output$selected_streets <- renderText({
    if (nrow(rv$selectedstreets)!=0) {
      selectedstreets <- ifelse(rv$selectedstreets$naam!="",rv$selectedstreets$naam,rv$selectedstreets$osm_id)
      paste(unique(selectedstreets),collapse=', ')
      }
    else {""}
    "test"
    })

  observeEvent(input$remove, {
    if (nrow(rv$selectedstreets)!=0) {
      leafletProxy("map") %>% removeShape(layerId=rv$selectedstreets$osm_id) %>% 
        addPolylines(data=rv$selectedstreets,layerId=~osm_id,label=~paste(naam,osm_id),col=~getcolor(gewenst),opacity=~getopacity(gewenst),weight=rv$weight)
      rv$selectedstreets <- tibble()
    }
  })
  
  observeEvent(input$save, {
    if (nrow(rv$selectedstreets)!=0) {
      rv$selectedstreets$naam    <- ifelse(rv$selectedstreets$naam=="",input$nieuwenaam,rv$selectedstreets$naam)
      rv$selectedstreets$gewenst <- input$gewenst
      rv$allstreets <- filter(rv$allstreets,!osm_id %in% rv$selectedstreets$osm_id)
      rv$allstreets <- rbind(rv$allstreets,rv$selectedstreets)
      rv$allstreets <- arrange(rv$allstreets,gewenst)
      streets.gewenst <- filter(rv$allstreets,gewenst!="------")
      save(streets.gewenst,file="output/streets.gewenst.Rdata")
      leafletProxy("map") %>% removeShape(layerId=rv$selectedstreets$osm_id) %>% 
        addPolylines(data=rv$selectedstreets,layerId=~osm_id,label=~paste(naam,osm_id),col=~getcolor(gewenst),opacity=~getopacity(gewenst),weight=rv$weight)
      rv$selectedstreets <- tibble()
    }
  })
  
  output$nieuwenaam <- renderUI({
    value <- ifelse(nrow(rv$selectedstreets)==0,"",unique(rv$selectedstreets$naam[rv$selectedstreets$naam!="------"])[1])
    textInput("nieuwenaam",label="Nieuwe naam:",value=value)
  })
}





#######################################################
### APP
#######################################################

shinyApp(ui = ui, server = server)



