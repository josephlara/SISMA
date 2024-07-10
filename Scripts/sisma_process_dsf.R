
# remotes::install_github("josephlara/sismar")
# https://github.com/josephlara/SISMA
# install.packages('glamr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))

# LOAD DEPENDENCIES -------------------------------------------------------

library(tidyverse)
library(sismar)
library(janitor)
library(mozR)
library(glamr)
library(openxlsx)
library(googledrive)

# USER INPUT --------------------------------------------------------------

year <- "2024"

# INPUT/OUTPUT PATHS ------------------------------------------------------------

# input paths
path_smi_cpn <- glue::glue("Data/smi_cpn_{year}.csv")
path_smi_mat <- glue::glue("Data/smi_mat_{year}.csv")
path_smi_ccr <- glue::glue("Data/smi_ccr_{year}.csv")
path_smi_cpp <- glue::glue("Data/smi_cpp_{year}.csv")
path_smi_ccd <- glue::glue("Data/smi_ccd_{year}.csv")
path_smi_ccs <- glue::glue("Data/smi_ccs_{year}.csv")
path_smi_ug <- glue::glue("Data/smi_ug_{year}.csv")

# output paths
output_smi_cpn <- glue::glue("Data/processed/smi_cpn_{year}.txt")
output_smi_mat <- glue::glue("Data/processed/smi_mat_{year}.txt")
output_smi_ccr <- glue::glue("Data/processed/smi_ccr_{year}.txt")
output_smi_cpp <- glue::glue("Data/processed/smi_cpp_{year}.txt")
output_smi_ccd <- glue::glue("Data/processed/smi_ccd_{year}.txt")
output_smi_ccs <- glue::glue("Data/processed/smi_ccs_{year}.txt")
output_smi_ug <- glue::glue("Data/processed/smi_ug_{year}.txt")


# path for saving historical datasets on google drive
path_historic_output_gdrive <- as_id("https://drive.google.com/drive/folders/1otEzX8FK6867lpB47k3RsvhjHO9cKPP9")

# PROCESS PORTUGUESE-------------------------------------------------------------------

df_smi_cpn <- process_sisma_csv(path_smi_cpn,
                                type = "SMI-CPN")

df_smi_mat <- process_sisma_csv(path_smi_mat, 
                                type = "SMI-MAT")

df_smi_ccr <- process_sisma_csv(path_smi_ccr, 
                                type = "SMI-CCR")

df_smi_cpp <- process_sisma_csv(path_smi_cpp, 
                                type = "SMI-CPP")

df_smi_ccd <- process_sisma_csv(path_smi_ccd, 
                                type = "SMI-CCD")

df_smi_ccs <- process_sisma_csv(path_smi_ccs,
                                type = "SMI-CCS")

df_smi_ug <- process_sisma_csv(path_smi_ug,
                                type = "SMI-UG")

# WRITE TO DISK -----------------------------------------------------------

write_tsv(df_smi_cpn,
          output_smi_cpn)

write_tsv(df_smi_mat,
          output_smi_mat)

write_tsv(df_smi_ccr,
          output_smi_ccr)

write_tsv(df_smi_cpp,
          output_smi_cpp)

write_tsv(df_smi_ccd,
          output_smi_ccd)

write_tsv(df_smi_ccs,
          output_smi_ccs)

write_tsv(df_smi_ug,
          output_smi_ug)

# COMPILE HISTORICAL DATA -------------------------------------------------

historical_smi <- 
  list.files("Data/processed/", pattern = "^smi_", full.names = TRUE) %>% 
  map(~ read_tsv(.x)) %>%
  reduce(rbind)

# WRITE TO LOCAL DISK -----------------------------------------------------

write_tsv(
  historical_smi,
  "Dataout/db_smi_dsf.txt",
  na = ""
)

# https://drive.google.com/drive/folders/1otEzX8FK6867lpB47k3RsvhjHO9cKPP9
drive_put("Dataout/db_smi_dsf.txt",
          path = path_historic_output_gdrive)

