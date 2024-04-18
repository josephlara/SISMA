# remotes::install_github("josephlara/sismar")

# LOAD DEPENDENCIES -------------------------------------------------------


library(tidyverse)
library(sismar)
library(janitor)
library(mozR)
library(glamr)
library(openxlsx)
library(googledrive)
load_secrets()

df <- read_delim("Dataout/db_smi.txt",
                 delim = "\t", 
                 escape_double = FALSE,
                 trim_ws = TRUE)


attach_meta_uid <- function(df, sheetname = "map_distrito_psnuuid") {
  
  #save google id
  path_meta <- googlesheets4::as_sheets_id("1cs8dC6OIFsjIJPIew3puPd1NkjGVeijz4jRnk3yiMR4")
  
  site_map <- googlesheets4::read_sheet(path_meta, sheet = sheetname) %>% 
    dplyr::select(distrito, snuuid, psnuuid)
  
  df <- df %>% 
    dplyr::left_join(site_map, 
                     by = dplyr::join_by(distrito)) %>% 
    dplyr::relocate(snuuid, .after = provincia) %>% 
    dplyr::relocate(psnuuid, .after = distrito)
  
  return(df)
  
}


attach_meta_coord <- function(df, sheetname = "map_us_coord") {
  
  #save google id
  path_meta <- googlesheets4::as_sheets_id("1cs8dC6OIFsjIJPIew3puPd1NkjGVeijz4jRnk3yiMR4")
  
  site_map <- googlesheets4::read_sheet(path_meta, sheet = sheetname) %>% 
    dplyr::select(sisma_uid = organisationunitid, 
                  latitude, 
                  longitude) %>% 
    dplyr::filter(!is.na(latitude))
  
  df <- df %>% 
    dplyr::left_join(site_map, 
                     by = dplyr::join_by(sisma_uid)) %>% 
    dplyr::relocate(latitude, .after = us) %>% 
    dplyr::relocate(longitude, .after = us)
  
  return(df)
  
}

attach_meta_partner <- function(df, sheetname = "Alcancar") {
  
  #save google id
  path_meta <- googlesheets4::as_sheets_id("1OwvyfilL4yitA1Xot3Kdbm7vNThBrJb6QhQm6ScDga8")
  
  site_map <- googlesheets4::read_sheet(path_meta, sheet = sheetname) %>% 
    dplyr::select(sisma_uid,
                  starts_with("support_partner"))
  
  df <- df %>% 
    dplyr::left_join(site_map, 
                     by = dplyr::join_by(sisma_uid)) %>% 
    dplyr::relocate(starts_with("support_partner"), .after = us)
  
  return(df)
  
}

df_1 <- df %>% 
  attach_meta_partner(sheetname = "MSSFPO") %>% 
  attach_meta_partner(sheetname = "Alcancar") %>% 
  attach_meta_partner(sheetname = "PEPFAR_Clinical") %>% 
  attach_meta_uid() %>% 
  attach_meta_coord()


write_tsv(
  df_1,
  "Dataout/db_smi_pcmd.txt",
  na = ""
)


