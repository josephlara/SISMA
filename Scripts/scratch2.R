

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

# df00 <- read_csv("Data/ajm_hc_mm_2023.csv")
# df0 <- read_csv("Data/ajm_hc_mm_2023.csv") %>% clean_names()

# input paths
path_dah_results <- glue::glue("Data/dah_{year}.csv")

df <- clean_sisma_csv(path_dah_results)

df_indicators <- df %>% distinct(indicator)

# df1 <- df0 %>% 
#   select('mz_ajmhcmm_ajm_novos_utentes_hiv_inscritos_10_14_anos_feminino',
#          'mz_ajmhcmm_ajm_novos_utentes_hiv_inscritos_10_14_anos_feminino_2',
#          'mz_ajmhcmm_ajm_novos_utentes_hiv_inscritos_10_14_anos_masculino',
#          'mz_ajmhcmm_ajm_novos_utentes_hiv_inscritos_10_14_anos_masculino_2',
#          'mz_ajmhcmm_ajm_novos_utentes_hiv_inscritos_15_24_feminino',
#          'mz_ajmhcmm_ajm_novos_utentes_hiv_inscritos_15_24_feminino',
#          'mz_ajmhcmm_ajm_novos_utentes_hiv_inscritos_15_24_masculino',
#          'mz_ajmhcmm_ajm_novos_utentes_hiv_inscritos_15_24_masculino_2')

df1 <- df %>%
  mutate(
    indicator_new = case_when(
                              str_detect(indicator, "_utentes_com_cd4_baixo_com_resultado_de_cr_ag_serico_durante_o_mes") ~ "CD4_BAIXO_CRAG_RES",
                              str_detect(indicator, "_utentes_com_cd4_baixo_com_resultado_de_cr_ag_serico_positivo_durante_o_mes_") ~ "CD4_BAIXO_CRAG_POS",
                              str_detect(indicator, "_utentes_com_cr_ag_no_lcr_positivo_que_iniciaram_tratamento_de_1a_escolha_de_mcc_durante_o_mes_") ~ "CRAG_LCR_POS_TX_1E",
                              

                              TRUE ~ NA_character_),
    
    sex = case_when(str_detect(indicator, "masculino") ~ "Masculino",
                    str_detect(indicator, "feminino") ~ "Feminino",
                    str_detect(indicator, "_mulheres_gravidas_") ~ "Feminino",
                    TRUE ~ NA_character_),
    
    age = case_when(str_detect(indicator, "_0_4_anos") ~ "<5",
                    str_detect(indicator, "_5_9_anos") ~ "05-09",
                    str_detect(indicator, "_10_14_anos") ~ "10-14",
                    str_detect(indicator, "_15_19_anos") ~ "15-19",
                    str_detect(indicator, "_20_") ~ "20+",
                    str_detect(indicator, "_0_14_") ~ "<15",
                    str_detect(indicator, "_15_anos") ~ "15+"
                    )) 



write_csv(
  df_indicators,
  "Documents/ajm_hc_mm_map.csv",
  na = ""
)

    
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



