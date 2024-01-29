
rm(list = ls())

# LOAD DEPENDENCIES -------------------------------------------------------


library(tidyverse)
library(mozR)

year <- "2023"

ats_results_path <- glue::glue("Data/ats_resultados_{year}.csv")
ats_hist_path <- glue::glue("Data/ats_hist_chave_{year}.csv")
ats_ci_path <- glue::glue("Data/ats_ci_lig_{year}.csv")
ats_ccsd_path <- glue::glue("Data/ats_smi_ccs_ccd_{year}.csv")
ats_saaj_path <- glue::glue("Data/ats_saaj_cm_{year}.csv")
ats_smi_path <- glue::glue("Data/ats_smi_{year}.csv")
ats_auto_path <- glue::glue("Data/ats_autoteste_{year}.csv")
cpn_path <- glue::glue("Data/smi_cpn_{year}.csv")
hiv_tarv_path <- glue::glue("Data/tarv_{year}.csv")
prep_path <- glue::glue("Data/prep_{year}.csv")
its_path <- glue::glue("Data/its_{year}.csv")


# ALREADY INBEDDED DATA ---------------------------------------------------



df <- data_sisma_ats_saaj_map

write_csv(df,
          "Dataout/ats_saaj_cm_indicators.csv",
          na = "")


df <- data_sisma_ats_autoteste_map

write_csv(df,
          "Dataout/ats_autoteste_indicators.csv",
          na = "")

df <- data_sisma_hiv_tarv_map

write_csv(df,
          "Dataout/tarv_indicators.csv",
          na = "")


df <- data_sisma_smi_mat_map

write_csv(df,
          "Dataout/smi_maternity.csv",
          na = "")


df <- data_sisma_ats_ci_map

write_csv(df,
          "Dataout/ats_ci_indicators.csv",
          na = "")


df <- data_sisma_ats_hist_map

write_csv(df,
          "Dataout/ats_hist_indicators.csv",
          na = "")


df <- data_sisma_ats_smi_map

write_csv(df,
          "Dataout/ats_smi_indicators.csv",
          na = "")

df <- data_sisma_ats_ccsd_map

write_csv(df,
          "Dataout/ats_ccsd_indicators.csv",
          na = "")




# NEW SISMA DATA ----------------------------------------------------------

df <- clean_sisma_csv(its_path)

df_indicators <- df %>% 
  distinct(indicator)

mz_its_1a_consulta

df_feature_eng <- df %>% 
  distinct(indicator) %>% 
  mutate(
    
    indicator_new = case_when(indicator == "mz_its_1a_consulta" ~ "ITS_1CONS_TOT",
                              str_detect(indicator, "1a_consulta_populacao_chave_") ~ "ITS_1CONS",
                              str_detect(indicator, "_testados_da_sifilis_") ~ "ITS_TESTE_SIF",
                              str_detect(indicator, "_testados_para_hiv_") ~ "ITS_TESTE_HIV",
                              str_detect(indicator, "_corrimento_uretral_") ~ "ITS_CORRI",
                              str_detect(indicator, "_ulcera_genital_") ~ "ITS_ULCERA",
                              str_detect(indicator, "_leucorreia_") ~ "ITS_LEUCOR",
                              str_detect(indicator, "_consulta_prevencao_") ~ "ITS_CONS_PREV",
                              str_detect(indicator, "_todas_as_consultas_") ~ "ITS_CONS_TOT",
                              str_detect(indicator, "mz_its_caso_indice") ~ "ITS_CASO_TIPO",
                              str_detect(indicator, "mz_its_contacto") ~ "ITS_CASO_TIPO",
                              TRUE ~ indicator),
    
    sex = case_when(str_detect(indicator, "masculino") ~ "Masculino",
                    str_detect(indicator, "_hsh") ~ "Masculino",
                    str_detect(indicator, "feminino") ~ "Feminino",
                    TRUE ~ NA_character_),
    
    age = case_when(str_detect(indicator, "_10_14_anos") ~ "10-14",
                    str_detect(indicator, "_15_19_anos") ~ "15-19",
                    str_detect(indicator, "_20_24_anos") ~ "20-24",
                    str_detect(indicator, "_25_anos") ~ "25+",
                    str_detect(indicator, "_10") ~ "<10"),
    
    sub_group = case_when(str_detect(indicator, "_hsh") ~ "HSH",
                          str_detect(indicator, "_pid") ~ "PID",
                          str_detect(indicator, "_rec") ~ "REC",
                          str_detect(indicator, "_ts") ~ "TS",
                          str_detect(indicator, "_tg") ~ "TG",
                          TRUE ~ NA_character_),
    
    result_status = case_when(str_detect(indicator, "positivo") ~ "Positivo",
                              str_detect(indicator, "negativo") ~ "Negativo",
                              str_detect(indicator, "_indeterminado") ~ "Indet.",
                              str_detect(indicator, "_nao_feito") ~ "Nao Feito",
                              TRUE ~ NA_character_),
    
    disaggregate = case_when(str_detect(indicator, "_lubrificantes") ~ "Lubrificantes",
                             str_detect(indicator, "_preservatito") ~ "Preservatito",
                             str_detect(indicator, "_convites_entregues") ~ "Convites Entregues",
                             TRUE ~ NA_character_)
)

df_count <- df %>% 
  count(indicator,
        wt = value)

write_csv(df_count,
          "Dataout/count.csv",
          na = "")
