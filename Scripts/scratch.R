

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
path_ats_results <- glue::glue("Data/ajm_hc_mm_{year}.csv")

df <- clean_sisma_csv(path_ats_results)

df_indicators <- df %>% distinct(indicator) %>% 
  mutate(
    indicator_new = case_when(
                              str_detect(indicator, "_ajm_novos_utentes_hiv_inscritos_") ~ "AJM_NOVOS_INSCRITOS",
                              str_detect(indicator, "_ajm_utentes_faltosos_") ~ "AJM_FALTOSOS",
                              str_detect(indicator, "_ajm_utentes_novo_inicio_reinicio_tarv_") ~ "AJM_TARV_REINICIO",
                              str_detect(indicator, "_ajm_utentes_com_carga_viral_elevada_inscritos_") ~ "AJM_CV_ALTA",
                              str_detect(indicator, "_ajm_utentes_com_risco_de_adesao_inscritos_") ~ "AJM_INSCRITOS_RISCO_ADESAO",
                              str_detect(indicator, "_ajm_utentes_com_o_tempo_de_permanencia_12_meses_") ~ "AJM_PERMAN_12M",
                              str_detect(indicator, "_ajm_utentes_com_o_tempo_de_permanencia_de_6_11_meses_") ~ "AJM_PERMAN_6_11M",
                              str_detect(indicator, "_ajm_utentes_com_o_tempo_de_permanencia_de_6_meses_") ~ "AJM_PERMAN_6M",
                              str_detect(indicator, "_ajm_utentes_em_pr_ep") ~ "AJM_PREP",
                              str_detect(indicator, "_ajm_utentes_inscritos_em_seguimento_na_us_comunidade_") ~ "AJM_SEG_COM",
                              str_detect(indicator, "_ajm_utentes_inscritos_em_seguimento_somente_na_us_") ~ "AJM_SEG_US",
                              str_detect(indicator, "_ajm_utentes_que_sairam_") ~ "AJM_SAIDA",
                              
                              str_detect(indicator, "_hc_novos_utentes_hiv_inscritos_") ~ "HC_NOVOS_INSCRITOS",
                              str_detect(indicator, "_hc_utentes_faltosos_") ~ "HC_FALTOSOS",
                              str_detect(indicator, "_hc_utentes_novo_inicio_reinicio_tarv_inscritos_") ~ "HC_TARV_REINICIO",
                              str_detect(indicator, "_hc_utentes_com_carga_viral_elevada_inscritos_") ~ "HC_CV_ALTA",
                              str_detect(indicator, "_hc_utentes_com_risco_de_adesao_inscritos_") ~ "HC_INSCRITOS_RISCO_ADESAO",
                              str_detect(indicator, "_hc_utentes_com_o_tempo_de_permanencia_12_meses_") ~ "HC_PERMAN_12M",
                              str_detect(indicator, "_hc_utentes_com_o_tempo_de_permanencia_de_6_11_meses_") ~ "HC_PERMAN_6_11M",
                              str_detect(indicator, "_hc_utentes_com_o_tempo_de_permanencia_de_6_meses_") ~ "HC_PERMAN_6M",
                              str_detect(indicator, "_hc_utentes_inscritos_em_seguimento_na_us_comunidade_") ~ "HC_SEG_COM",
                              str_detect(indicator, "_hc_utentes_inscritos_em_seguimento_somente_na_us_") ~ "HC_SEG_US",
                              str_detect(indicator, "_hc_utentes_que_sairam_") ~ "AJM_SAIDA",
                              
                              str_detect(indicator, "_mm_criancas_10_anos_hiv_inscritas_") ~ "MM_NOVOS_INSCRITOS", # WHAT IS THIS??!! DOES NOT OVERLAP WITH 4th ITEM??!!
                              str_detect(indicator, "_mm_mulheres_gravidas_inscritas_") ~ "MM_NOVOS_INSCRITOS",
                              str_detect(indicator, "_mm_mulheres_lactantes_inscritas_") ~ "MM_NOVOS_INSCRITOS",
                              str_detect(indicator, "_mm_novos_utentes_hiv_inscritos_") ~ "MM_NOVOS_INSCRITOS",
                              str_detect(indicator, "_mm_utentes_faltosos_") ~ "MM_FALTOSOS",
                              str_detect(indicator, "_mm_utentes_novo_inicio_reinicio_tarv_inscritos_") ~ "MM_TARV_REINICIO",
                              str_detect(indicator, "_mm_utentes_com_carga_viral_elevada_inscritos_") ~ "MM_CV_ALTA",
                              str_detect(indicator, "_mm_utentes_com_risco_de_adesao_inscritos_") ~ "MM_INSCRITOS_RISCO_ADESAO",
                              str_detect(indicator, "_mm_utentes_com_o_tempo_de_permanencia_12_meses_") ~ "MM_PERMAN_12M",
                              str_detect(indicator, "_mm_utentes_com_o_tempo_de_permanencia_de_6_11_meses_") ~ "MM_PERMAN_6_11M",
                              str_detect(indicator, "_mm_utentes_com_o_tempo_de_permanencia_de_6_meses_") ~ "MM_PERMAN_6M",
                              str_detect(indicator, "_mm_utentes_inscritos_em_seguimento_na_us_comunidade_") ~ "MM_SEG_COM",
                              str_detect(indicator, "_mm_utentes_inscritos_em_seguimento_somente_na_us_") ~ "MM_SEG_US",
                              str_detect(indicator, "_mm_utentes_que_sairam_") ~ "MM_SAIDA",
                              
                              

                              TRUE ~ indicator),
    
    sex = case_when(str_detect(indicator, "masculino") ~ "Masculino",
                    str_detect(indicator, "feminino") ~ "Feminino",
                    str_detect(indicator, "_mulheres_gravidas_") ~ "Feminino",
                    str_detect(indicator, "_mulheres_lactantes_") ~ "Feminino",
                    TRUE ~ NA_character_),
    
    age = case_when(str_detect(indicator, "_0_4_anos") ~ "<5",
                    str_detect(indicator, "_5_9_anos") ~ "05-09",
                    str_detect(indicator, "_10_14_anos") ~ "10-14",
                    str_detect(indicator, "_15_19_anos") ~ "15-19",
                    str_detect(indicator, "_20_24_anos") ~ "20-24",
                    str_detect(indicator, "_15_24") ~ "15-24",
                    str_detect(indicator, "_25_29_anos") ~ "25-29",
                    str_detect(indicator, "_30_anos") ~ "30+",
                    str_detect(indicator, "_25_anos") ~ "25+",
                    ),
    disaggregate = case_when(str_detect(indicator, "_sairam_por_desistencia_") ~ "Desistencia",
                             str_detect(indicator, "_sairam_por_alta_") ~ "Alta",
                             str_detect(indicator, "_sairam_por_abandono_") ~ "Abandono",
                             str_detect(indicator, "_sairam_por_obito_") ~ "Obito",
                             str_detect(indicator, "_sairam_por_referido_") ~ "Referido",
                             str_detect(indicator, "_sairam_por_transferido_") ~ "Transferido",
                             TRUE ~ NA_character_)
    ) 



mz_ajmhcmm_ajm_novos_utentes_hiv_inscritos_10_14_anos_feminino_2 # WTF

,


    
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
    
    disaggregate = case_when(str_detect(indicator, "_sairam_por_desistencia_") ~ "Desistencia",
                             str_detect(indicator, "_sairam_por_alta_") ~ "Alta",
                             str_detect(indicator, "_sairam_por_abandono_") ~ "Abandono",
                             str_detect(indicator, "_sairam_por_obito_") ~ "Obito",
                             str_detect(indicator, "_sairam_por_referido_") ~ "Referido",
                             TRUE ~ NA_character_)
  )

write_csv(df_indicators,
          "Documents/ajm_hc_mm_indicators.csv",
          na = "")



