setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

library(openxlsx)
library(tidyverse)
library(sf)

load('../1.get_osm_data/streets.Rdata')

data <- as_tibble(read.xlsx("../0.input_data/evaluaties.xlsx"))
data <- separate_rows(data,osm_id)
data <- right_join(streets,data,by="osm_id")
data$naam <- ifelse(is.na(data$name),data$naam,data$name)
data <- select(data,-c(osm_id,name,highway,ref))

save(data,file="data.Rdata")
save(data,file="../3.app/data.Rdata")





### test leaflet

library(RColorBrewer)
library(leaflet)

#gewenst.col <- brewer.pal(8,'Dark2')
#gewenst.col <- tibble(gewenst=unique(data$gewenst)
#                      ,gewenst.col=brewer.pal(8,'Dark2')[1:length(unique(data$gewenst))]
#)
#data <- left_join(data,gewenst.col,by="gewenst")

evalpalette <- colorNumeric(palette=c('red4','darkgoldenrod1','green4'),domain=0:10)
#gewenstpalette <- function(x){}


( leaflet() %>% addProviderTiles(providers$CartoDB.Positron)
  #%>% addPolylines(data=data,label=~data[['ondergrond']],color=~evalpalette(data[['ondergrond']]),opacity=.95)
  %>% addPolylines(data=data,label=~data[['naam']],color='#781F19',opacity=1,weight=5)
  %>% addPolylines(data=data,label=~data[['naam']],color='white',opacity=1,weight=1)
)







#https://www.keene.edu/campus/maps/tool/
# geometries <- list()
# for (id in data$id) {
#    d <- read.csv(paste0("data/polylines/",id),header=FALSE)
#    d <- as.matrix(d)
#    d <- st_linestring(d)
#    geometries[[id]] <- d
#    }
# geometries <- st_sfc(geometries)
# geometries <- st_sf(tibble(id=names(geometries),geometry=geometries))
# data <- full_join(geometries,data,by='id')
# rm(geometries,id,d)




