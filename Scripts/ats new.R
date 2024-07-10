

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


path_ats_ci <- "Documents/ats_ci_lig_2024 new.csv"

test <- clean_sisma_csv(path_ats_ci) %>% 
  select(indicator) %>% 
  distinct()

write_csv(test, file = "Documents/ats_new.csv", na="")


test2 <- test %>% 
  mutate(
    age = case_when(
      str_detect(indicator, "_0_11_meses_") ~ "0-11 meses",
      str_detect(indicator, "_12_23_") ~ "12-23 meses",
      str_detect(indicator, "_18_23_") ~ "18-23 meses"),
    age_coarse = "<15",
    sex = case_when(str_detect(indicator, "masculino") ~ "Masculino",
                    str_detect(indicator, "feminino") ~ "Feminino",
                    TRUE ~ NA_character_),
    disaggregate = case_when(str_detect(indicator, "_brigada_movel_") ~ "Brigada Movel",
                             str_detect(indicator, "_posto_fixo_") ~ "Posto Fixo",
                             TRUE ~ NA_character_),
    indicator_new = case_when(str_detect(indicator, "_hib_1a_dose_") ~ "PAV_DPT_HEP_HIB_1D",
                              str_detect(indicator, "_hib_2a_dose_") ~ "PAV_DPT_HEP_HIB_2D",
                              str_detect(indicator, "_hib_3a_dose_") ~ "PAV_DPT_HEP_HIB_3D",

                              str_detect(indicator, "_bcg_") ~ "PAV_BCG",

                              str_detect(indicator, "_ipv_1a_dose_") ~ "PAV_IVP_1D",
                              str_detect(indicator, "_ipv_2a_dose_") ~ "PAV_IVP_2D",
                              
                              str_detect(indicator, "_pcv_1a_dose_") ~ "PAV_PCV_1D",
                              str_detect(indicator, "_pcv_2a_dose_") ~ "PAV_PCV_2D",
                              str_detect(indicator, "_pcv_3a_dose_") ~ "PAV_PCV_3D",
                              
                              str_detect(indicator, "_polio_1a_") ~ "PAV_POLIO_1D",
                              str_detect(indicator, "_polio_2a_") ~ "PAV_POLIO_2D",
                              str_detect(indicator, "_polio_3a_") ~ "PAV_POLIO_3D",
                              str_detect(indicator, "_polio_primario_") ~ "PAV_POLIO_PRIM",
                              
                              str_detect(indicator, "_rv_1a_dose_") ~ "PAV_RV_1D",
                              str_detect(indicator, "_rv_2a_dose_") ~ "PAV_RV_2D",
                              
                              str_detect(indicator, "_sarampo_0") ~ "PAV_SARAMPO",
                              str_detect(indicator, "_sarampo_1") ~ "PAV_SARAMPO",
                              str_detect(indicator, "_sarampo_rubeola_") ~ "PAV_SARAMPO_RUBEOLA",
                              
                              str_detect(indicator, "_criancas_completamente_") ~ "PAV_COMPL_VAC"
                              
    ),
    source = "LdR PAV"
  )

write_csv(test2, file = "Documents/pav_map.csv", na="")
