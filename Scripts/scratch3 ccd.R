

# LOAD DEPENDENCIES -------------------------------------------------------


library(tidyverse)
library(sismar)
library(janitor)
library(mozR)
# library(glamr)
library(openxlsx)
# load_secrets()


# USER INPUT --------------------------------------------------------------

year <- "2024"

path_smi_ccd <- glue::glue("Data/smi_ccd_{year}.csv")

test <- clean_sisma_csv(path_smi_ccd) %>% 
  select(indicator) %>% 
  distinct()



test2 <- test %>% 
  mutate(
    age = case_when(
      str_detect(indicator, "_0_5_meses_") ~ "0-5 meses",
      str_detect(indicator, "_0_59_meses_") ~ "0-59 meses",
      str_detect(indicator, "_0_59m_") ~ "0-59 meses",
      str_detect(indicator, "_6_11_meses_") ~ "6-11 meses",
      str_detect(indicator, "_6_23_meses_") ~ "6-23 meses",
      str_detect(indicator, "_12_23_meses_") ~ "12-23 meses",
      str_detect(indicator, "_12_59_meses_") ~ "12-59 meses",
      str_detect(indicator, "_24_59_meses_") ~ "24-59 meses",
      str_detect(indicator, "_5_14a_") ~ "5-14",
      str_detect(indicator, "_de_5_14_") ~ "5-14",
      str_detect(indicator, "_5_14_anos_") ~ "5-14"),
    
    disaggregate = case_when(str_detect(indicator, "_sro_e_zinco_") ~ "SRO / Zinco",
                             str_detect(indicator, "_so_com_sro_") ~ "SRO",
                             str_detect(indicator, "_so_com_zinco_") ~ "Zinco",
                             
                             str_detect(indicator, "_ciprofloxacina_") ~ "Ciprofloxacina",
                             str_detect(indicator, "_amoxacilina_") ~ "Amoxacilina",
                             str_detect(indicator, "_coartem_") ~ "Coartem",
                             str_detect(indicator, "_artesunato_as_") ~ "Zinco",
                             str_detect(indicator, "_artesunato_amodiaquina_") ~ "Artesunato Amodiaquina",
                             str_detect(indicator, "_quinino_") ~ "Quinino",
                             str_detect(indicator, "_desparasitante_") ~ "Desparasitante",
                             
                             str_detect(indicator, "_especializada_doenca_cronica_") ~ "Esp. Doenca Cronica",
                             str_detect(indicator, "_consulta_de_crianca_em_risco_") ~ "CCR",
                             str_detect(indicator, "_urgencia_de_pediatria_") ~ "Urgencia",
                             str_detect(indicator, "_sal_ferroso_") ~ "Sal Ferroso",
                             
                             TRUE ~ NA_character_),
    
    disaggregate_sub = case_when(str_detect(indicator, "brigada_movel") ~ "Brigada Movel",
                                 str_detect(indicator, "posto_fixo") ~ "Posto Fixo",
                                 TRUE ~ NA_character_),
    
    sex = case_when(str_detect(indicator, "masculino") ~ "Masculino",
                    str_detect(indicator, "feminino") ~ "Feminino",
                    TRUE ~ NA_character_),
    
    result_status = case_when(str_detect(indicator, "positivo") ~ "Positivo",
                              str_detect(indicator, "negativo") ~ "Negativo",
                              str_detect(indicator, "indeterminado") ~ "Indet."),
    
    indicator_new = case_when(str_detect(indicator, "_primeiras_consultas_") ~ "CCD_CONSULTA_PRIM",
                              str_detect(indicator, "_consultas_seguintes_") ~ "CCD_CONSULTA_SEG",
                              str_detect(indicator, "_receberam_o_desparasitante_") ~ "CCD_DESPARASIT",
                              str_detect(indicator, "_clinicamente_diagnosticadas_com_anemia_") ~ "CCD_DIAG_ANEMIA",
                              str_detect(indicator, "_suplementadas_com_vitamina_a_") ~ "CCD_SUP_VIT_A",

                              str_detect(indicator, "_novo_caso_de_outras_doencas_") ~ "CCD_NOVO_DIAG_OUTRA",
                              str_detect(indicator, "_referidas_para_") ~ "CCD_REF_PARA",
                              str_detect(indicator, "_total_por_sexo_") ~ "CCD_TOTAL",
                              str_detect(indicator, "_com_teste_de_hiv_") ~ "CCD_TESTE_HIV",
                              str_detect(indicator, "_elegiveis_para_testagem_") ~ "CCD_TESTE_ELIG_HIV",
                              str_detect(indicator, "_total_de_criancas_testadas_brigada_") ~ "CCD_TESTE_HIV_TOT",
                              str_detect(indicator, "_total_de_criancas_testadas_posto_") ~ "CCD_TESTE_HIV_TOT",
                              str_detect(indicator, "_anemia_que_iniciaram_tratamento_com_sal_ferroso_") ~ "CCD_ANEMIA_TX",
                              str_detect(indicator, "_sintomas_de_tuberculose_") ~ "CCD_SINAIS_TB",
                              str_detect(indicator, "_caso_de_pneumonia_") ~ "CCD_CASO_PNEUM",
                              str_detect(indicator, "_caso_de_diarreia_") ~ "CCD_CASO_DAIRREIA",
                              str_detect(indicator, "_caso_de_disenteria_") ~ "CCD_CASO_DISENTERIA",
                              str_detect(indicator, "_caso_de_malaria_") ~ "CCD_CASO_MALARIA_TX",
                              str_detect(indicator, "_teste_para_malaria_") ~ "CCD_TESTE_MALARIA",
                              str_detect(indicator, "_teste_de_malaria_") ~ "CCD_TESTE_MALARIA",
                              str_detect(indicator, "_com_desnutricao_aguda_grave_") ~ "CCD_DESN_AGUDA",
                              str_detect(indicator, "_com_desnutricao_aguda_moderada_") ~ "CCD_DESN_MODER",
                              str_detect(indicator, "_iniciam_profilaxia_com_ctz_") ~ "CCD_INICIO_CTZ",
                              
                              str_detect(indicator, "_contacto_com_tb_") ~ "CCD_CONTACTO_TB",
                              str_detect(indicator, "_criancas_referidas_") ~ "CCD_REFERIDAS",
                              str_detect(indicator, "_desenvolvimento_psicomotor_") ~ "CCD_ATRASO_PSICOMOTOR",
                              str_detect(indicator, "_com_desnutricao_aguda_grave_") ~ "CCD_DESN_AGUDA",
                              str_detect(indicator, "_com_desnutricao_aguda_grave_") ~ "CCD_DESN_AGUDA",
                              str_detect(indicator, "_com_desnutricao_aguda_grave_") ~ "CCD_DESN_AGUDA",
                              
                              str_detect(indicator, "_com_baixo_peso_idade_") ~ "CCD_BAIXO_PESO"
    )
  )


write_csv(test2, file = "Documents/ccd_map.csv", na="")
  