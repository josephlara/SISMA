library(tidyverse)
library(googlesheets4)
library(sismar)
library(mozR)

df_smi <- read_delim("Dataout/db_smi.txt", 
                     delim = "\t", escape_double = FALSE, 
                     trim_ws = TRUE)

df_vl <- read_delim("~/GitHub/Enhanced_Monitoring/Dataout/em_disa.txt", 
                    delim = "\t", escape_double = FALSE, 
                    trim_ws = TRUE) 

df_dpi <- read_delim("~/GitHub/Enhanced_Monitoring/Dataout/em_dpi.txt", 
                     delim = "\t", escape_double = FALSE, 
                     trim_ws = TRUE)

map_sisma <- pull_sitemap(sheetname = "list_sisma") |> 
  select(sisma_uid, provincia, distrito, us)


df_vl1 <- df_vl |> 
  select(!c(starts_with("his_"), datim_uid, site_nid, psnuuid, support_type, agency, partner, snu, psnu, sitename)) |> 
  rename(periodo = period,
         sexo = sex,
         idade = age,
         sub_grupo = group,
         disagregacao = motive,
         disagregacao_sub = tat_step
         ) |> 
  mutate(fonte = "DISA",
         periodo_coorte = NA_character_,
         resultado_estado = NA_character_,
         idade_agrupada = case_when(idade == "Unknown Age" ~ "Desconh.",
                                    idade %in% c("<01", "01-04", "05-09", "10-14") ~ "<15",
                                    idade %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+") ~ "15+")
         ) |> 
  pivot_longer(c(VL, VLS, TAT), names_to = "indicador", values_to = "valor") |> 
  filter(!is.na(sisma_uid)) |> 
  left_join(map_sisma, by = join_by(sisma_uid))


df_dpi1 <- df_dpi |> 
  select(!c(starts_with("his_"), datim_uid, site_nid, psnuuid, support_type, ajuda, agency, partner, snu, psnu, sitename)) |> 
  rename(periodo = period,
         sexo = sex,
         indicador = indicator,
         disagregacao = disaggregate,
         disagregacao_sub = tat_step,
         resultado_estado = result,
         valor = value
  ) |> 
  mutate(fonte = "DISA",
         periodo_coorte = NA_character_,
         idade = NA_character_,
         idade_agrupada = NA_character_
  ) |> 
  filter(!is.na(sisma_uid)) |> 
  left_join(map_sisma, by = join_by(sisma_uid))


write_tsv(df_vl1, 
          "Dataout/db_disa_vl.txt",
          na = "")

write_tsv(df_dpi1, 
          "Dataout/db_disa_dpi.txt",
          na = "")



  setdiff(names(df_smi), names(df_vl1))
setdiff(names(df_vl1), names(df_smi))
names(df_smi)
names(df_vl1)
