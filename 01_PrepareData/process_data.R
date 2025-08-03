rm(list=ls())

library(tidyverse)
library(sf)
library(magick)




# plannen -----------------------------------------------------------------

data_plans <- tribble(
   ~folder,~file,~type,
   "autoontradend_fietsontradend","plan1","Wandelweg",
   "autoontradend_fietstoegangsweg","plan2","Winkelstraat/woonerf",
   "autoontradend_fietsverbindingsweg30","plan2","Smalle wandel-/fietsweg",
   "autoontradend_fietsverbindingsweg50","plan2","Brede fietsweg",
   "autotoegangsweg_fietsontradend",NA,NA,
   "autotoegangsweg_fietstoegangsweg","plan2","Woonstraat",
   "autotoegangsweg_fietsverbindingsweg30","plan1","Fietsstraat",
   "autotoegangsweg_fietsverbindingsweg50",NA,NA,
   "autoverbindingsweg30_fietsontradend",NA,NA,
   "autoverbindingsweg30_fietstoegangsweg","plan2","Verbindings-/ontsluitingweg 30km/u met lokaal fietspad",
   "autoverbindingsweg30_fietsverbindingsweg30","plan2","Verbindings-/ontsluitingsweg 30km/u",
   "autoverbindingsweg30_fietsverbindingsweg50",NA,NA,
   "autoverbindingsweg50_fietsontradend",NA,NA,
   "autoverbindingsweg50_fietstoegangsweg","plan2","Verbindings-/ontsluitingweg 50km/u met stenen fietspaden",
   "autoverbindingsweg50_fietsverbindingsweg30","plan2","Verbindings-/ontsluitingsweg 50km/u met smalle fietspaden",
   "autoverbindingsweg50_fietsverbindingsweg50","plan2","Verbindings-/ontsluitingsweg 50km/u met brede fietspaden",
   "autoverbindingsweg70_fietsontradend","plan1","Autoweg 70km/u zonder fietspaden",
   "autoverbindingsweg70_fietstoegangsweg",NA,NA,
   "autoverbindingsweg70_fietsverbindingsweg30",NA,NA,
   "autoverbindingsweg70_fietsverbindingsweg50","plan1","Autoweg 70km/u met fietspaden",
   ) |>
   filter(!is.na(file))

transform_plans <- function(folder,file) {
   img <- "/home/jorre/Documents/01.Engagementen/WIMapp/figuren/plannen/" |>
      file.path(folder,paste0(file,".pdf")) |>
      image_read_pdf(density = 50)
   "02_App/www/" |>
      file.path(paste0(folder,".png")) |>
      image_write(image=img, format = "png")
   return(NA)
   }

walk2(data_plans$folder,data_plans$file,transform_plans)





# Data --------------------------------------------------------------------

read_polylines <- function(filename) {
   path <- paste0("01_PrepareData/input/",filename,".txt")
   if ( !file.exists(path) ) stop(filename,"does not exist")
   coords <- path |>
      read_delim(delim = ",", col_names = c("lon", "lat"), trim_ws = TRUE) |>
      as.matrix() |>
      st_linestring()
   return(coords)
   }

data <- "01_PrepareData/input/straten.xlsx" |>
   readxl::read_xlsx() |>
   mutate(
      file=ifelse(is.na(section),name,paste0(name,'_',section)),
      geometry=map(file,read_polylines) |> st_sfc(crs = 4326),
      file=NULL,
      ) |>
   st_as_sf() |>
   left_join(select(data_plans,type,img=folder),join_by(type))

write_rds(data,"02_App/data/streets.rds")




