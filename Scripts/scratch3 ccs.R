

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

path_smi_ccd <- glue::glue("Data/smi_ccs_{year}.csv")

path_smi_ccs <- "Data/smi_ccs_2024.csv"

test <- sismar::clean_sisma_csv(path_smi_ccs) |> 
  select(indicator) |> 
  distinct()


test2 <- test %>% 
  mutate(
    age = case_when(
      str_detect(indicator, "_0_5_meses_") ~ "0-5 meses",
      str_detect(indicator, "_6_m") ~ "6 meses",
      str_detect(indicator, "_0_59_meses_") ~ "0-59 meses",
      str_detect(indicator, "_0_59m_") ~ "0-59 meses",
      str_detect(indicator, "_6_11_meses_") ~ "6-11 meses",
      str_detect(indicator, "_6_23_meses_") ~ "6-23 meses",
      str_detect(indicator, "_12_23_meses_") ~ "12-23 meses",
      str_detect(indicator, "_12_59_meses_") ~ "12-59 meses",
      str_detect(indicator, "_24_59_meses_") ~ "24-59 meses",
      str_detect(indicator, "_0_23_meses_") ~ "0-23 meses",
      str_detect(indicator, "_5_14a_") ~ "5-14",
      str_detect(indicator, "_5_a_14_a") ~ "5-14",
      str_detect(indicator, "_de_5_14_") ~ "5-14",
      str_detect(indicator, "_5_14_anos_") ~ "5-14"),
    
    disaggregate = case_when(str_detect(indicator, "_sro_e_zinco_") ~ "SRO / Zinco",
                             str_detect(indicator, "_so_com_sro_") ~ "SRO",
                             str_detect(indicator, "_so_com_zinco_") ~ "Zinco",
                             str_detect(indicator, "_a_ccr_") ~ "CCR",
                            
                             
                             str_detect(indicator, "_ciprofloxacina_") ~ "Ciprofloxacina",
                             str_detect(indicator, "_amoxacilina_") ~ "Amoxacilina",
                             str_detect(indicator, "_coartem_") ~ "Coartem",
                             str_detect(indicator, "_artesunato_as_") ~ "Zinco",
                             str_detect(indicator, "_artesunato_amodiaquina_") ~ "Artesunato Amodiaquina",
                             str_detect(indicator, "_quinino_") ~ "Quinino",
                             str_detect(indicator, "_desparasitante_") ~ "Desparasitante",
                             
                             str_detect(indicator, "_especializada_doenca_cronica_") ~ "Esp. Doenca Cronica",
                             str_detect(indicator, "crianca_doente") ~ "Crianca Doente",
                             str_detect(indicator, "_urgencia_de_pediatria_") ~ "Urgencia",
                             str_detect(indicator, "_sal_ferroso_") ~ "Sal Ferroso",
                       
                             str_detect(indicator, "_continuado") ~ "Continuado",
                             str_detect(indicator, "_exclusivo") ~ "Exclusivo",
                             str_detect(indicator, "_1a_dose") ~ "1a Dose",
                             str_detect(indicator, "_2a_dose") ~ "2a Dose",
                             str_detect(indicator, "_3a_dose") ~ "3a Dose",
                             str_detect(indicator, "total_de_maes_") ~ "Maes",
                             str_detect(indicator, "_de_criancas_com_test") ~ "Criancas",
                             str_detect(indicator, "_de_criancas_test") ~ "Criancas",
                             
                             
                             TRUE ~ NA_character_),
    
    disaggregate_sub = case_when(str_detect(indicator, "brigada_movel_posto_fix") ~ "Total",
      
      str_detect(indicator, "brigada_movel") ~ "Brigada Movel",
                                 str_detect(indicator, "posto_fixo") ~ "Posto Fixo",
                                 
                                 
                                 TRUE ~ NA_character_),
    
    sex = case_when(str_detect(indicator, "masculino") ~ "Masculino",
                    str_detect(indicator, "feminino") ~ "Feminino",
                    TRUE ~ NA_character_),
    
    result_status = case_when(str_detect(indicator, "positivo") ~ "Positivo",
                              str_detect(indicator, "negativo") ~ "Negativo",
                              str_detect(indicator, "indeterminado") ~ "Indet."),

    
     indicator_new = case_when(str_detect(indicator, "_primeiras_consultas_") ~ "CCS_CONSULTA_PRIM",
                               str_detect(indicator, "_consultas_seguintes_") ~ "CCS_CONSULTA_SEG",
                               str_detect(indicator, "_receberam_o_desparasitante_") ~ "CCS_DESPARASIT",
                               str_detect(indicator, "_suplementadas_com_vitamina_a_") ~ "CCS_SUP_VIT_A",
                               str_detect(indicator, "_consultas_de_") ~ "CCS_CONSULTA_TOTAL",
                               str_detect(indicator, "_aleitamento_materno") ~ "CCS_ALEITAMENTO_MAT",
                               str_detect(indicator, "_receberam_a_dose") ~ "CCS_MNP",
                               
                               
                               str_detect(indicator, "_referidas_para_") ~ "CCS_REF_PARA",
                               str_detect(indicator, "_total_por_sexo_") ~ "CCS_TOTAL",
                               str_detect(indicator, "_com_teste_de_hiv") ~ "CCS_TESTE_HIV",
                               str_detect(indicator, "_elegiveis_para_testagem_") ~ "CCS_TESTE_ELIG_HIV",
                               str_detect(indicator, "_total_de_maes_testadas_para_hiv") ~ "CCS_TESTE_HIV_TOT",
                               str_detect(indicator, "_total_de_criancas_testadas_hiv") ~ "CCS_TESTE_HIV_TOT",




                               str_detect(indicator, "_com_desnutricao_aguda") ~ "CCS_DESN_AGUDA",
                               str_detect(indicator, "_com_desnutricao_croni") ~ "CCS_DESN_CRONICA",
                               str_detect(indicator, "_de_tb_") ~ "CCS_SINTOMAS_TB",
                               str_detect(indicator, "seroestado_descon") ~ "CCS_SEROESTADO_DESC",
                               
                               
                               str_detect(indicator, "_contacto_com_tb_") ~ "CCS_CONTACTO_TB",
                               str_detect(indicator, "_criancas_referidas_") ~ "CCS_REFERIDAS",
                               str_detect(indicator, "_desenv_psicomotor_") ~ "CCS_ATRASO_PSICOMOTOR",
                               str_detect(indicator, "_crescimento_insuficie") ~ "CCS_CRESC",
                               str_detect(indicator, "_sobrepeso_") ~ "CCS_SOBRE_OBES",
                               
                              
                               
                               str_detect(indicator, "_baixo_peso") ~ "CCS_BAIXO_PESO"
    )
   )

test2 <- test2 |> 
  mutate(source = "LdR CCS",
         age_coarse = case_when(disaggregate == "Maes" ~ "15+",
                                !is.na(age) ~ "<15",
                                .default = NULL)
         )|> 
  select(indicator, indicator_new, sex, age, age_coarse,
         disaggregate, disaggregate_sub, result_status, source)

write_csv(test2, file = "Documents/ccs_map_1.csv", na="")
