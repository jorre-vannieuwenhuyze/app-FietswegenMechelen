setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

library(tidyverse)
library(sf)
library(lwgeom)
library(leaflet)

load("../2.assign_desired_types/output/streets.gewenst.Rdata")
streets.gewenst <- filter(streets.gewenst,naam!="")
streets.gewenst <- select(streets.gewenst,-gewenst)
#streets.gewenst <- filter(streets.gewenst,naam %in% c("Pasbrugplein","Pasbrugstraat","Nekkerspoelstraat"))
d <- streets.gewenst









