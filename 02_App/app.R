library(leaflet)
library(tidyverse)
library(sf)



# DATA --------------------------------------------------------------------

colors <- list(
   "wandelpad"= "#FFCC00", # "#D2B48C", "#B0A98F"
   "klinkers" = "#FFA500",
   "fietspad"="#E60000", # "#B2182B", #"#CC0000"
   "straat"="#999999", #"#4D4D4D",  "#CCCCCC", 
   "lijn"="#FFFFFF" 
   )

streettypes <- tribble(
   ~img,~color,~weight,
   "autoontradend_fietsontradend",colors$wandelpad,2,
   "autoontradend_fietstoegangsweg",colors$wandelpad,4,
   "autoontradend_fietsverbindingsweg30",colors$fietspad,4,
   "autoontradend_fietsverbindingsweg50",colors$fietspad,6,
   "autoontradend_fietsverbindingsweg50",colors$lijn,1,
   # "autotoegangsweg_fietsontradend",
   "autotoegangsweg_fietstoegangsweg",colors$wandelpad,8,
   "autotoegangsweg_fietstoegangsweg",colors$klinkers,4,
   "autotoegangsweg_fietsverbindingsweg30",colors$fietspad,6,
   "autotoegangsweg_fietsverbindingsweg30",colors$klinkers,2,
   # "autotoegangsweg_fietsverbindingsweg50",
   # "autoverbindingsweg30_fietsontradend",
   # "autoverbindingsweg30_fietstoegangsweg",
   "autoverbindingsweg30_fietsverbindingsweg30",colors$fietspad,8,
   "autoverbindingsweg30_fietsverbindingsweg30",colors$straat,3,
   # "autoverbindingsweg30_fietsverbindingsweg50",
   # "autoverbindingsweg50_fietsontradend",
   # "autoverbindingsweg50_fietstoegangsweg",
   "autoverbindingsweg50_fietsverbindingsweg30",colors$fietspad,8,
   "autoverbindingsweg50_fietsverbindingsweg30",colors$straat,5,
   "autoverbindingsweg50_fietsverbindingsweg50",colors$fietspad,10,
   "autoverbindingsweg50_fietsverbindingsweg50",colors$straat,7,   
   # "autoverbindingsweg70_fietsontradend",
   # "autoverbindingsweg70_fietstoegangsweg",
   # "autoverbindingsweg70_fietsverbindingsweg30",
   # "autoverbindingsweg70_fietsverbindingsweg50",
   ) |>
   mutate(order=row_number())

data <- read_rds("data/streets.rds") |>
   mutate(
      hover=pmap(
         list(name,type,img),
         function(name,type,img){HTML(paste0("<h4>",name,"</h4><h5>",type,"</h5><img src='",img,".png' width='400'/>"))}
         )
      ) |>
   left_join(streettypes,join_by(img)) |>
   mutate(
      section=section |> as.character() |> replace_na(""),
      id=paste0(name,'_',section,'_',row_number()) |> str_remove_all(' '),
      .by=c(name,section)
      ) |>
   nest(data=c(geometry,hover)) |>
   arrange(order)
   







# UI ----------------------------------------------------------------------

ui <- fluidPage(
  tags$style(type = "text/css", "html, body {width:100%; height:100%; margin:0; padding:0;}"),
  leafletOutput("map", width = "100%", height = "100vh")
   )





# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {

  # Initial map
   output$map <- renderLeaflet({
      create_addPolylines <- function(data_,color_,weight_,id_){
         function(.x) { addPolylines(.x, data=data_, color=color_, weight=weight_, layerId=id_, label=~hover, opacity=1 ) }
         }
      polylines <- data |>
         mutate(functions = pmap(list(data,color,weight,id),create_addPolylines)) |>
         pull(functions)     
      leaflet() |> 
         addProviderTiles(providers$CartoDB.Positron) |>
         setView(lng = 4.4786, lat = 51.0259, zoom = 14) |>
         magrittr::freduce(polylines)
         })

  # React to zoom level
  observe({
    zoom <- input$map_zoom
    if (is.null(zoom)) return()
   scale_weight <- function(base_weight) {
      round(base_weight * (zoom - 10) / 4)
      }
   create_addPolylines <- function(data_,color_,weight_,id_){
      function(.x) { 
         .x |>
         removeShape(id_) |>
         addPolylines(data=data_, color=color_, weight=scale_weight(weight_), layerId=id_, label=~hover, opacity=1 ) }
      }
   polylines_zoom <- data |>
      mutate(
         functions = pmap(list(data,color,weight,id),create_addPolylines)
         ) |>
      pull(functions)    
    leafletProxy("map") |> magrittr::freduce(polylines_zoom)
  })
}



shinyApp(ui = ui, server = server)
















# ui <- tags$html(
#   tags$head(
#     tags$style(HTML("
#       html, body {
#         height: 100%;
#         margin: 0;
#         padding: 0;
#         overflow: hidden;
#       }
#       #map {
#         height: 100%;
#         width: 100%;
#       }
#       .control-panel {
#         background: rgba(255,255,255,0.9);
#         padding: 10px;
#         border-radius: 8px;
#         box-shadow: 0 0 15px rgba(0,0,0,0.2);
#       }
#     "))
#   ),
#    tags$body(
#       leafletOutput("map", width = "100%", height = "100%"),
#       absolutePanel(
#          top = 20, 
#          right = 20, 
#          width = 250, 
#          class = "control-panel",
#          radioButtons(
#             "situatie",
#             p("Wat wens je te zien?"),
#             choices = list(
#                "Bestaande situatie" = "bestaand",
#                "Gewenste situatie" = "gewenst"
#                ),
#             selected = "gewenst"
#             )
#          )
#       )
#    )

