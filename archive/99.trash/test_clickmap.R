setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

library(shiny)
library(leaflet)
library(tidyverse)
library(sf)



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





load('../1.get_osm_data/output/streets.all.Rdata')
streets.all$gewenst <- "NA"

if(file.exists("output/streets.gewenst.Rdata")){
  load('output/streets.gewenst.Rdata')
  streets.all <- filter(streets.all,!osm_id %in% streets.gewenst$osm_id)
  streets.all <- rbind(streets.all,streets.gewenst)
  }
streets.all <- arrange(streets.all,gewenst)


bbox <- unname(st_bbox(streets.all))


labels.gewenst <- tribble(
~type,~color
,'Voetgangerszone','gray40'  
,'Fietsweg'       ,'brown3' 
,'Fietsstraat'    ,'brown4'
,'Zone 30'        ,'orange'
,'Zone 50'        ,'orange3'
,'Zone 70'        ,'orange4'
,'NA'             ,'blue'                 
)
labels.gewenst$color <- col2hex(labels.gewenst$color)
#colorstreets <- function(x)(setNames(labels.gewenst$color,labels.gewenst$type)[x])








ui <- fluidPage(
  sidebarLayout(
     mainPanel(leafletOutput("map",height=600))
    ,sidebarPanel(
      wellPanel(
         h3("Selected streets")
        ,textOutput("selected_streets")
        ,actionButton("remove", "Remove selection")
        )
      ,wellPanel(
         textInput("nieuwenaam",label="Nieuwe naam:",value="")
        ,radioButtons(inputId="gewenst",label="Gewenst type:",choices=labels.gewenst$type)
        ,actionButton("save", "Save")
        )
      )
    )
  )


getcolor   <- function(x){unname(setNames(labels.gewenst$color,labels.gewenst$type)[x])}
getopacity <- function(x){ifelse(x=="NA",.2,1)}
getweight  <- function(x){ifelse(x=="NA",4,6)}



server <- function(input, output, session) {

  rv <- reactiveValues(selectedstreets = tibble(),allstreets = streets.all)  
  
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>% 
      addPolylines(data=rv$allstreets,layerId=~osm_id,label=~naam,col=~getcolor(gewenst),opacity=~getopacity(gewenst),weight=~getweight(gewenst))
  })
  
  
  observeEvent(input$map_click, {
    print(input$map_click$lng)
  })
    
  

  
}


shinyApp(ui = ui, server = server)



