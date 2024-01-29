
# PURPOSE:
# AUTHOR:  Joe Lara | USAID
# DATE: 2023-05-01
# NOTES: 

# DEPENDENCIES & SETUP -----------------

library(tidyverse)
library(mozR)
library(glamr)
library(glitr)
library(readxl)
library(janitor)
library(glue)
library(ggthemes)

# PATHS --------------------------------

path_sisma <- "Data/mer/SISMA_TARV_activos e novos inicios_T12023_25042023.csv"
path_sesp <- "~/GitHub/Enhanced_Monitoring/Dataout/em_dsd.txt"

# LOAD FUNCTIONS -----------------------

# LOAD DATA ----------------------------

df_sisma <- clean_sisma_csv(path_sisma)

df_sesp <- read_delim(path_sesp, 
                     delim = "\t", escape_double = FALSE, 
                     trim_ws = TRUE)

ajuda_site_map <- pull_sitemap() %>% 
  select(sisma_uid_datim_map, datim_uid)

# MUNGE --------------------------------


df_sisma_ajuda <- df_sisma %>% 
  inner_join(ajuda_site_map, by = c("sisma_uid" = "sisma_uid_datim_map")) %>% 
  filter(str_detect(indicator, "mz_hiv_sida_nat_no_fim_do_mes"),
         !is.na(value)) %>% 
  mutate(age = case_when(str_detect(indicator, "0_4_anos") ~ "<=4",
                         str_detect(indicator, "5_9_anos") ~ "5-9",
                         str_detect(indicator, "10_14_anos") ~ "10-14",
                         str_detect(indicator, "15_19_anos") ~ "15-19",
                         str_detect(indicator, "_20_") ~ "20+",
                         TRUE ~ NA_character_),
         sex = case_when(str_detect(indicator, "masculino") ~ "Male",
                         str_detect(indicator, "feminino") ~ "Female",
                         TRUE ~ "Unknown"),
         indicator = "Activos_TARV") %>% 
  relocate(value, .after = everything()) 


filter_period <- unique(df_sisma$period)


df_sesp_1 <- df_sesp %>% 
  mutate(period = period - days(19)) %>% 
  filter(period %in% filter_period,
         !is.na(TX_ACTIVE)) %>% 
  select(period, datim_uid, TX_ACTIVE) %>% 
  glimpse()


final <- left_join(df_sisma_ajuda, df_sesp_1, by = c("period" = "period", "datim_uid" = "datim_uid"))

# ANALYTICS ----------------------------

# SAVE TO DISK -------------------------
