

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
path_ats_results <- glue::glue("Data/ats_resultados_{year}.csv")
path_ats_hist <- glue::glue("Data/ats_hist_chave_{year}.csv")
path_ats_ci <- glue::glue("Data/ats_ci_lig_{year}.csv")
path_ats_ccsd <- glue::glue("Data/ats_smi_ccs_ccd_{year}.csv")
path_ats_saaj <- glue::glue("Data/ats_saaj_cm_{year}.csv")
path_ats_smi <- glue::glue("Data/ats_smi_{year}.csv")
path_ats_auto <- glue::glue("Data/ats_autoteste_{year}.csv")
path_hiv_tarv <- glue::glue("Data/tarv_{year}.csv")
path_hiv_prep <- glue::glue("Data/prep_{year}.csv") #
path_hiv_apss <- glue::glue("Data/apss_{year}.csv") 
path_hiv_its <- glue::glue("Data/its_{year}.csv") #
path_smi_cpn <- glue::glue("Data/smi_cpn_{year}.csv") #
path_smi_mat <- glue::glue("Data/smi_mat_{year}.csv") #
path_smi_ccr <- glue::glue("Data/smi_ccr_{year}.csv") #

# output paths
output_ats <- glue::glue("Data/processed/ats_{year}.txt")
output_ats_auto <- glue::glue("Data/processed/ats_autoteste_{year}.txt")
output_hiv_tarv <- glue::glue("Data/processed/hiv_tarv_{year}.txt")
output_hiv_prep <- glue::glue("Data/processed/hiv_prep_{year}.txt")
output_hiv_apss <- glue::glue("Data/processed/hiv_apss_{year}.txt")
output_hiv_its <- glue::glue("Data/processed/hiv_its_{year}.txt")
output_smi_cpn <- glue::glue("Data/processed/smi_cpn_{year}.txt")
output_smi_mat <- glue::glue("Data/processed/smi_mat_{year}.txt")
output_smi_ccr <- glue::glue("Data/processed/smi_ccr_{year}.txt")


# identify processed files
path_outputs <- "Dataout/"
sisma_historical_files <- dir("Dataout/", pattern = "*.txt")


# PROCESS PORTUGUESE-------------------------------------------------------------------


df_ats <- bind_rows(process_sisma_csv(path_ats_results, type = "ATS Result"),
                    process_sisma_csv(path_ats_hist, type = "ATS History"),
                    process_sisma_csv(path_ats_ci, type = "ATS CI"),
                    process_sisma_csv(path_ats_ccsd, type = "ATS CCSD"),
                    process_sisma_csv(path_ats_saaj, type = "ATS SAAJ"),
                    process_sisma_csv(path_ats_smi, type = "ATS SMI")
)
# 2023 only
df_autoteste <- process_sisma_csv(path_ats_auto, 
                                  type = "ATS Auto")

# 2023 only
df_apss <- process_sisma_csv(path_hiv_apss, 
                             type = "HIV APSS")

# 2023 only
df_its <- process_sisma_csv(path_hiv_its,
                            type = "HIV ITS")

# 2023 only
df_prep <- process_sisma_csv(path_hiv_prep, 
                             type = "HIV PREP")

# 2023 only
df_tarv <- process_sisma_csv(path_hiv_tarv, 
                             type = "HIV TARV")

df_smi_ccr <- process_sisma_csv(path_smi_ccr, 
                                type = "SMI-CCR")

df_smi_cpn <- process_sisma_csv(path_smi_cpn,
                                type = "SMI-CPN")

# 2023 only
df_smi_mat <- process_sisma_csv(path_smi_mat, 
                                type = "SMI-MAT")


# WRITE TO DISK -----------------------------------------------------------


write_tsv(df_ats,
          output_ats)

write_tsv(df_autoteste,
          output_ats_auto)

write_tsv(df_tarv,
          output_hiv_tarv)

write_tsv(df_prep,
          output_hiv_prep)

write_tsv(df_apss,
          output_hiv_apss)

write_tsv(df_its,
          output_hiv_its)

write_tsv(df_smi_cpn,
          output_smi_cpn)

write_tsv(df_smi_mat,
          output_smi_mat)

write_tsv(df_smi_ccr,
          output_smi_ccr)



# PLOTS -------------------------------------------------------------------


df <- df_ats %>% 
  filter(indicador == "ATS_TST_POS",
         periodo < "2022-06-20") %>% 
  summarize(
    valor = sum(valor, na.rm=T),
    .by = c(provincia, us) 
  )

df2 <- df_ats %>% 
  filter(indicador == "ATS_TST_POS",
         periodo < "2022-06-20") %>% 
  summarize(
    valor = sum(valor, na.rm=T),
    .by = c(provincia, us, periodo) 
  )

ggplot(df, aes(factor(provincia, levels=c('Niassa', 'Cabo Delgado', 'Nampula', 'Zambezia', 'Tete', 'Manica', 'Sofala', 'Inhambane', 'Gaza', 'Maputo Provincia', 'Maputo Cidade')), y = valor)) +
  geom_boxplot(alpha = .5)+
  coord_flip() +
  labs(
    x = "",
    y = "",
    subtitle = "Analise de Anomolia de ATS_TST_POS",
    caption = "Source: MISAU")

df2_outlier <- df2 %>% 
  filter(valor > 300)


ggplot(df2, aes(factor(provincia, levels=c('Niassa', 'Cabo Delgado', 'Nampula', 'Zambezia', 'Tete', 'Manica', 'Sofala', 'Inhambane', 'Gaza', 'Maputo Provincia', 'Maputo Cidade')), y = valor)) +
  geom_boxplot(alpha = .5)+
  coord_flip() +
  labs(
    x = "",
    y = "",
    subtitle = "Analise de Anomolia de ATS_TST_POS",
    caption = "Source: MISAU") + 
  facet_wrap(~ periodo) +
  geom_text(aes(label = us),
            data = df2_outlier,
            size = 2.5,
            vjust = -.5,
            hjust = 0,
            nudge_y = 0, 
            nudge_x = 0)



# DATA REPORTING COMPLETENESS CHECK -----------------------------------------------------------------


report_tarv <- report_create("Data/tarv_2023", "tarv", "2023-06")
report_ptv <- report_create(df_smi_cpn, "ptv")
report_ccr <- report_create(df_smi_ccr, "ccr")
report_prep <- report_create(df_prep, "prep")
report_apss <- report_create(df_apss, "apss_pp")

report_write(df_report, file = "Dataout/reports/test_report.xlsx")


PERIOD = "2023-02"
FOLDER_PATH = ("Data/test/")

#read all files in a specific folder
input_files <- dir(FOLDER_PATH, full.name = TRUE, pattern = "*.csv")
all_data <- map_dfr(input_files, ~report_create_all(.x, period = PERIOD))

# COMPILE HISTORICAL DATA -------------------------------------------------


hiv_tarv_files <- list.files("Data/processed/", pattern = "^hiv_tarv", full.names = TRUE)


sisma_historical_df <- sisma_historical_files %>% 
  map(~ read_tsv(file.path("Data/processed/", .))) %>%
  reduce(rbind)

sisma_historical_df <- sisma_historical_files %>% 
  map(~ read_tsv(list.files("Data/processed/", pattern = "^hiv_tarv", full.names = TRUE))) %>%
  reduce(rbind)

sisma_historical_df <- map(hiv_tarv_files, ~read_tsv(.)) %>%
  reduce(rbind)


# -------------------------------------------------------------------------
