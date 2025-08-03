library(leaflet)
library(tidyverse)

col2hex <- function(cname) {
  colMat <- col2rgb(cname)
  rgb(red=colMat[1,]/255,green=colMat[2,]/255,blue = colMat[3,]/255)
  }

### LOAD DATA
load("data.Rdata")

### DEFINE OBJECTS
continuous.var <- names(data)[unlist(lapply(data,is.numeric))]
types.gewenst  <- c("Voetgangerszone"="voetgangerszone"
                   ,"Fietsweg"="fietsweg"
                   ,"Fietsstraat"="fietsstraat"
                   ,"Zone 30"="zone30"
                   ,"Zone 50"="zone50"
                   ,"Zone 70"="zone70"
                   )

evalpalette <- colorNumeric(palette=c('red4','darkgoldenrod1','green4'),domain=0:10)


ui <- fluidPage(
  titlePanel(h1("Mechelse Fietsroutes")),
  sidebarLayout(
    sidebarPanel(
       radioButtons("situatie",
         p("Wat wens je te zien?"),
         choices=list("Bestaande situatie"='bestaand',"Gewenste situatie"='gewenst'),
         selected='gewenst'),
       conditionalPanel(
         condition = "input.situatie=='bestaand'",
         radioButtons("bestaand","Welk criterium wil je zien?",continuous.var)
         ),
       conditionalPanel(
         condition = "input.situatie=='gewenst'",
         checkboxGroupInput(inputId ="gewenst"
                           ,label   ="Welke straattypes wil je zien?"
                           ,choices =types.gewenst
                           ,selected=types.gewenst
                           )
       ),       
       ),
    mainPanel(
      leafletOutput("map", height = 500)
    )
  )
)


server <- function(input, output) { 
  
  output$map <- renderLeaflet({
    map <- leaflet()  %>% addProviderTiles(providers$CartoDB.Positron)
    if (input$situatie=='bestaand') {
      map <- map %>% addPolylines(data=data,label=~data[['naam']],color=~evalpalette(data[[input$bestaand]]),opacity=.95)
      }
    if (input$situatie=='gewenst') {
       if ('voetgangerszone' %in% input$gewenst){
          d <- filter(data,gewenst=='voetgangerszone')
          map <- (    map 
                 %>% addPolylines(data=d,label=~d[['naam']],color=col2hex('gray50'),opacity=1,weight=3)
                 )
          }
      if ('fietsweg' %in% input$gewenst){
         d <- filter(data,gewenst=='fietsweg')
         map <- (    map 
                %>% addPolylines(data=d,label=~d[['naam']],color=col2hex('brown4'),opacity=1,weight=3)
                )
         }
      if ('fietsstraat' %in% input$gewenst){
        d <- filter(data,gewenst=='fietsstraat')
        map <- (    map 
               %>% addPolylines(data=d,label=~d[['naam']],color=col2hex('brown4'),opacity=1,weight=6)
               %>% addPolylines(data=d,label=~d[['naam']],color=col2hex('purple')   ,opacity=1,weight=2)
               )
        }       
      if ('zone30' %in% input$gewenst){
        d <- filter(data,gewenst=='zone30')
        map <- (    map 
                    %>% addPolylines(data=d,label=~d[['naam']],color=col2hex('brown4'),opacity=1,weight=6)
                    %>% addPolylines(data=d,label=~d[['naam']],color=col2hex('white'),opacity=1,weight=3)
        )
      }  
      if ('zone50' %in% input$gewenst){
        d <- filter(data,gewenst=='zone50')
        map <- (    map 
                    %>% addPolylines(data=d,label=~d[['naam']],color=col2hex('brown4'),opacity=1,weight=6)
                    %>% addPolylines(data=d,label=~d[['naam']],color=col2hex('yellow'),opacity=1,weight=3)
        )
      }
      if ('zone70' %in% input$gewenst){
        d <- filter(data,gewenst=='zone70')
        map <- (    map 
                    %>% addPolylines(data=d,label=~d[['naam']],color=col2hex('brown4'),opacity=1,weight=6)
                    %>% addPolylines(data=d,label=~d[['naam']],color=col2hex('red'),opacity=1,weight=3)
        )
      }  
    }
    return(map)
  })
  
}


shinyApp(ui = ui, server = server)