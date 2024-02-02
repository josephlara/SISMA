

# LOAD DEPENDENCIES -------------------------------------------------------


library(tidyverse)
library(sismar)
library(janitor)
library(mozR)
library(glamr)
library(openxlsx)
load_secrets()



# USER INPUT --------------------------------------------------------------

year <- "2023"

# INPUT/OUTPUT PATHS ------------------------------------------------------------


# input paths
path_ajmhcmm_results <- glue::glue("Data/ajm_hc_mm_{year}.csv")


# output paths
output_ajmhcmm_results <- glue::glue("Data/processed/ajm_hc_mm_{year}.txt")



# identify processed files
path_outputs <- "Dataout/"
sisma_historical_files <- dir("Dataout/", pattern = "*.txt")


# PROCESS PORTUGUESE-------------------------------------------------------------------


# 2023 only


df <- process_sisma_csv(file = path_ajmhcmm_results,
                        type = "HIV AJMHCMM")



# WRITE TO DISK -----------------------------------------------------------


write_tsv(df,
          output_ajmhcmm_results,
          na = "")


