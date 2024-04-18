# remotes::install_github("josephlara/sismar")

# LOAD DEPENDENCIES -------------------------------------------------------


library(tidyverse)
library(sf)
library(sismar)
library(janitor)
library(mozR)
library(glamr)
library(openxlsx)
library(googledrive)
load_secrets()

unzip("Data/districts.zip", exdir = "Data/")

shape_file <- "GIS/NEW_161_Districts.shp"
coordinate_file <- "GIS/sisma_us_gis.xlsx"
output_folder <- "Dataout/"




check_coord_us_dist <- function(shape_file){
  
  # Read the shapefile containing district boundaries
  districts <- sf::st_read(shape_file) %>% 
    select(shape_snu     = Province, 
           shape_psnu    = District, 
           shape_snuuid  = SNU1Uid, 
           shape_psnuuid = PSN_Uuid,
           geometry)
  
  facilities_file <- googlesheets4::read_sheet("1cs8dC6OIFsjIJPIew3puPd1NkjGVeijz4jRnk3yiMR4", 
                                               sheet = "lista_us_sisma")
  
  # prepare the facilities data and remove any sites without longitude/latitude
  facilities <- facilities_file %>%
    dplyr::select(us = orgunitlevel4, 
                  longitude, 
                  latitude, 
                  organisationunitid,
                  sisma_prov_uid  = snuuid,
                  sisma_dist_uid = psnuuid,
                  sisma_prov     = orgunitlevel2,
                  sisma_dist    = orgunitlevel3) %>% 
    tidyr::drop_na()
  
  # Convert facilities dataframe to sf object 
  facilities_sf <- sf::st_as_sf(x = facilities, 
                                coords = c("longitude", "latitude"), 
                                crs = st_crs(districts)) 
  
  # Perform spatial join between districts and facilities and remove the geometry column
  district_info <- st_join(districts, facilities_sf) %>% 
    sf::st_drop_geometry()
  
  #add a flag to check if the psnu is as expected
  output_file <- district_info %>% 
    mutate(check_psnu = case_when(sisma_dist_uid == shape_psnuuid ~ 0,
                                  .default = 1)
    ) %>% 
    select(sisma_prov, 
           shape_prov = shape_snu,
           sisma_prov_uid, 
           shape_prov_uid = shape_snuuid,
           sisma_dist,
           shape_dist = shape_psnu, 
           sisma_dist_uid, 
           shape_dist_uid = shape_psnuuid,
           us, 
           sisma_uid = organisationunitid, 
           coord_alerta = check_psnu
           )
  
  return(output_file)
}

df <- check_coord_us_dist(shape_file)
