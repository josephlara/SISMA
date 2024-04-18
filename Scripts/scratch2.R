

# LOAD DEPENDENCIES -------------------------------------------------------


library(tidyverse)
library(sismar)
library(janitor)
library(mozR)
library(glamr)
library(openxlsx)
load_secrets()


# USER INPUT --------------------------------------------------------------

year <- "2024"

# INPUT/OUTPUT PATHS ------------------------------------------------------------

# df00 <- read_csv("Data/ajm_hc_mm_2023.csv")
# df0 <- read_csv("Data/ajm_hc_mm_2023.csv") %>% clean_names()

# input paths
path_results <- glue::glue("Data/tarv_{year}.csv")

df <- clean_sisma_csv(path_results)

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
      str_detect(indicator, "_activos_com_dah_em_tarv_ate_o_fim_de_mes_anterior_") ~ "DAH_ACTIVO_MES_ANT",
      str_detect(indicator, "_utentes_novos_inicios_em_tarv_que_iniciaram_seguimento_para_doenca_avancada_por_hiv_durante_o_mes_") ~ "DAH_NOVO_TARV_NOVO",
      str_detect(indicator, "_utentes_reinicios_em_tarv_que_iniciaram_seguimento_para_doenca_avancada_por_hiv_durante_o_mes_") ~ "DAH_NOVO_TARV_REINCIO",
      str_detect(indicator, "_utentes_activos_em_tarv_que_iniciaram_seguimento_para_doenca_avancada_por_hiv_durante_o_mes_") ~ "DAH_NOVO_TARV_ACTIVO",
      str_detect(indicator, "_utentes_em_seguimento_para_doenca_avancada_por_hiv_") ~ "DAH_SEG_TOTAL",
      str_detect(indicator, "_utentes_em_seguimento_para_doenca_avancada_que_sairam_da_abordagem_durante_o_mes_por_qualquer_motivo_saida_por_alta_obito_transferido_suspenso_abandono_") ~ "DAH_SAIDA",

      str_detect(indicator, "_obitos_nos_utentes_com_diagnostico_de_doenca_avancada_na_coorte_") ~ "COORTE_OBITO",
      str_detect(indicator, "_novos_inscritos_em_seguimento_de_dah_na_coorte_") ~ "COORTE_INSCR_DAH",
      
      str_detect(indicator, "_utentes_com_pedido_de_cd4_de_rastreio_durante_o_mes_") ~ "CD4_PED",
      str_detect(indicator, "_mulheres_gravidas_hiv_utentes_com_pedido_de_cd4_de_rastreio_") ~ "CD4_PED_MG", # perhaps use same indicator and code pw disag
      str_detect(indicator, "_utentes_com_resultado_de_cd4_de_rastreio_disponivel_durante_o_mes_") ~ "CD4_RES_DISP",
      str_detect(indicator, "_mulheres_gravidas_hiv_utentes_com_resultado_de_cd4_de_rastreio_disponivel_") ~ "CD4_RES_DISP_MG", # perhaps use same indicator and code pw disag
      str_detect(indicator, "_utentes_com_resultado_de_cd4_baixo_durante_o_mes_") ~ "CD4_RES_BAIXO",
      str_detect(indicator, "_mulheres_gravidas_hiv_utentes_com_resultado_de_cd4_baixo_") ~ "CD4_RES_BAIXO_MG", # perhaps use same indicator and code pw disag
      str_detect(indicator, "_utentes_elegiveis_a_pedido_de_tb_lam_com_resultado_de_tb_lam_durante_o_mes_") ~ "TB_LAM_RES",
      str_detect(indicator, "_mulheres_gravidas_hiv_utentes_elegiveis_a_pedido_de_tb_lam_com_resultado_de_tb_lam_") ~ "TB_LAM_RES", # perhaps use same indicator and code pw disag
      str_detect(indicator, "_utentes_com_resultado_de_tb_lam_positivo_durante_o_mes_") ~ "TB_LAM_POS", 
      str_detect(indicator, "_mulheres_gravidas_hiv_utentes_com_resultado_de_tb_lam_positivo") ~ "TB_LAM_POS", # perhaps use same indicator and code pw disag
      str_detect(indicator, "_utentes_com_cd4_baixo_com_resultado_de_cr_ag_serico_durante_o_mes") ~ "CD4_BAIXO_CRAG_RES",
      str_detect(indicator, "_utentes_com_cd4_baixo_com_resultado_de_cr_ag_serico_positivo_durante_o_mes_") ~ "CD4_BAIXO_CRAG_POS",
      str_detect(indicator, "_mulheres_gravidas_hiv_utentes_com_cd4_baixo_com_resultado_de_cr_ag_serico_positivo_") ~ "CD4_BAIXO_CRAG_POS", # perhaps use same indicator and code pw disag
      str_detect(indicator, "_utentes_com_cr_ag_serico_positivo_e_com_o_resultado_de_cr_ag_no_lcr_registrado_durante_o_mes_") ~ "CRAG_POS_LCR_RES",
      str_detect(indicator, "_mulheres_gravidas_hiv_utentes_com_cr_ag_serico_positivo_e_com_o_resultado_de_cr_ag_no_lcr_registrado_") ~ "CRAG_POS_LCR_RES", # perhaps use same indicator and code pw disag
      str_detect(indicator, "_utentes_com_cr_ag_serico_positivo_que_iniciaram_tratamento_preventivo_de_mcc_durante_o_mes_") ~ "CRAG_POS_TX_MCC",
      str_detect(indicator, "_mulheres_gravidas_hiv_utentes_com_cr_ag_serico_positivo_que_iniciaram_tratamento_preventivo_de_mcc_") ~ "CRAG_POS_TX_MCC", # perhaps use same indicator and code pw disag
      str_detect(indicator, "_utentes_com_cr_ag_no_lcr_positivo_que_iniciaram_tratamento_de_1a_escolha_de_mcc_durante_o_mes_") ~ "CRAG_LCR_POS_TX_1E",
      str_detect(indicator, "_mulheres_gravidas_hiv_utentes_com_cr_ag_no_lcr_positivo_que_iniciaram_tratamento_de_1a_escolha_de_mcc_") ~ "CRAG_LCR_POS_TX_1E", # perhaps use same indicator and code pw disag
      str_detect(indicator, "_utentes_com_novo_diagnostico_de_sarcoma_de_kaposi_e_com_indicacao_para_quimioterapia_durante_o_mes_") ~ "SK_DIAG_IND_TX",
      str_detect(indicator, "_mulheres_gravidas_hiv_utentes_com_novo_diagnostico_de_sarcoma_de_kaposi_e_com_indicacao_para_quimioterapia_") ~ "SK_DIAG_IND_TX", # perhaps use same indicator and code pw disag
      str_detect(indicator, "_utentes_com_sarcoma_de_kaposi_que_iniciaram_ciclo_1_de_quimioterapia_durante_o_mes_") ~ "SK_DIAG_TX_1C",
      str_detect(indicator, "_mulheres_gravidas_hiv_utentes_com_sarcoma_de_kaposi_que_iniciaram_ciclo_1_de_quimioterapia_") ~ "SK_DIAG_TX_1C", # perhaps use same indicator and code pw disag
      
      TRUE ~ NA_character_),
    
    sub_group = case_when(str_detect(indicator, "_mulheres_gravidas_") ~ "MG",
                          TRUE ~ NA_character_),
    
    disaggregate = case_when(str_detect(indicator, "_novos_inicios_de_tarv_") ~ "Novo em TARV",
                             str_detect(indicator, "_activos_em_tarv_") ~ "Activo em TARV",
                             str_detect(indicator, "_reinicios_de_tarv_") ~ "Reinicio de TARV",
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
                    str_detect(indicator, "_15_anos") ~ "15+"),
    
    source = "LdR DAH"
    
    )


df2 <- df1 %>%
  select(!c(period, snu, psnu, sitename, sisma_uid, value)) %>% 
  distinct(indicator, indicator_new, sub_group, disaggregate, sex, age, source)




write_csv(
  df_indicators,
  "Documents/tarv_map.csv",
  na = ""
)
