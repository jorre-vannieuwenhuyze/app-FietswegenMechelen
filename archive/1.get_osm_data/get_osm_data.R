setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
unlink("output",recursive=TRUE)
dir.create("output")

library(tidyverse)
library(osmdata)
library(sf)
library(lwgeom)
library(units)
library(leaflet)

boundaries <- opq(bbox = "Mechelen") %>% add_osm_feature(key='admin_level',value='8') %>% osmdata_sf #%>% unique_osmdata
boundaries <- boundaries$osm_multipolygons
boundaries <- filter(boundaries,name=="Mechelen")

streets.all <- opq("Mechelen") %>% add_osm_feature("highway") %>% osmdata_sf()
streets.all <- streets.all$osm_lines
streets.all <-  st_zm(streets.all)

streets.all$naam <- ifelse(!is.na(streets.all$name),streets.all$name,
                    ifelse(!is.na(streets.all$ref),streets.all$ref,
                    ""))
streets.all$id <- streets.all$osm_id
streets.all <- select(streets.all,osm_id,id,naam,geometry)

streets.all <- st_transform(streets.all,3812)
buffer <- set_units(0.0001,'metre')
for (idtochange in c("37703570","31565403","23886987","45552275","718488645","28601958","22884423")) {
  x <- filter(streets.all,id==idtochange)
  i <- st_collection_extract(st_intersection(x,streets.all),"POINT")
  i <- st_buffer(i,buffer)
  x <- st_collection_extract(st_split(x,i),"LINESTRING")
  l <- st_length(x)
  x <- x[l>2*buffer,]
  x$id <- paste(x$id,1:nrow(x),sep='.')
  streets.all <- filter(streets.all,id!=idtochange)
  streets.all <- rbind(streets.all,x)
  }
streets.all <- st_transform(streets.all,4326)
rm(idtochange,x,i,l)
 
leaflet() %>% addTiles() %>% setView(lng=4.480325,lat=51.028211,zoom=14) %>% addPolylines(data=streets.all,label=~id)

save(streets.all,file="output/streets.all.Rdata")








