

# LOAD DEPENDENCIES -------------------------------------------------------


library(tidyverse)
library(sismar)
library(janitor)
library(mozR)
library(glamr)
library(openxlsx)
library(googledrive)
load_secrets()


# USER INPUT --------------------------------------------------------------

year <- "2023"

path_smi_pav <- glue::glue("Data/smi_ccr_{year}.csv")

test <- clean_sisma_csv(path_smi_pav) %>% 
  select(indicator) %>% 
  distinct()

write_csv(test,
          "Documents/map_ccr.csv")
