# remotes::install_github("josephlara/sismar")

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

year <- "2024"

# INPUT/OUTPUT PATHS ------------------------------------------------------------


# input paths
path_ats_results <- glue::glue("Data/ats_resultados_{year}.csv")
path_ats_hist <- glue::glue("Data/ats_hist_chave_{year}.csv")
path_ats_ci <- glue::glue("Data/ats_ci_lig_{year}.csv")
path_ats_ccsd <- glue::glue("Data/ats_smi_ccs_ccd_{year}.csv")
path_ats_saaj <- glue::glue("Data/ats_saaj_cm_{year}.csv")
path_ats_smi <- glue::glue("Data/ats_smi_{year}.csv")
path_ats_auto <- glue::glue("Data/ats_autoteste_{year}.csv")
path_hiv_dah <- glue::glue("Data/dah_{year}.csv")
path_hiv_tarv <- glue::glue("Data/tarv_{year}.csv")
path_hiv_prep <- glue::glue("Data/prep_{year}.csv")
path_hiv_apss <- glue::glue("Data/apss_{year}.csv") 
path_hiv_its <- glue::glue("Data/its_{year}.csv")
path_hiv_ajmhcmm <- glue::glue("Data/ajm_hc_mm_{year}.csv")
path_smi_cpn <- glue::glue("Data/smi_cpn_{year}.csv")
path_smi_mat <- glue::glue("Data/smi_mat_{year}.csv")
path_smi_ccr <- glue::glue("Data/smi_ccr_{year}.csv")
path_smi_cpp <- glue::glue("Data/smi_cpp_{year}.csv")
path_smi_pav <- glue::glue("Data/smi_pav_{year}.csv")
path_smi_ccd <- glue::glue("Data/smi_ccd_{year}.csv")

# output paths
output_ats <- glue::glue("Data/processed/ats_{year}.txt")
output_ats_auto <- glue::glue("Data/processed/ats_autoteste_{year}.txt")
output_hiv_tarv <- glue::glue("Data/processed/hiv_tarv_{year}.txt")
output_hiv_dah <- glue::glue("Data/processed/hiv_dah_{year}.txt")
output_hiv_prep <- glue::glue("Data/processed/hiv_prep_{year}.txt")
output_hiv_apss <- glue::glue("Data/processed/hiv_apss_{year}.txt")
output_hiv_its <- glue::glue("Data/processed/hiv_its_{year}.txt")
output_hiv_ajmhcmm <- glue::glue("Data/processed/ajm_hc_mm_{year}.txt")
output_smi_cpn <- glue::glue("Data/processed/smi_cpn_{year}.txt")
output_smi_mat <- glue::glue("Data/processed/smi_mat_{year}.txt")
output_smi_ccr <- glue::glue("Data/processed/smi_ccr_{year}.txt")
output_smi_cpp <- glue::glue("Data/processed/smi_cpp_{year}.txt")
output_smi_pav <- glue::glue("Data/processed/smi_pav_{year}.txt")
output_smi_ccd <- glue::glue("Data/processed/smi_ccd_{year}.txt")

# path for saving historical datasets on google drive
path_historic_output_gdrive <- as_id("https://https://drive.google.com/drive/folders/1otEzX8FK6867lpB47k3RsvhjHO9cKPP9")


# PROCESS PORTUGUESE-------------------------------------------------------------------


df_ats <- bind_rows(process_sisma_csv(path_ats_results, type = "ATS Result"),
                    process_sisma_csv(path_ats_hist, type = "ATS History"),
                    process_sisma_csv(path_ats_ci, type = "ATS CI"),
                    process_sisma_csv(path_ats_ccsd, type = "ATS CCSD"),
                    process_sisma_csv(path_ats_saaj, type = "ATS SAAJ"),
                    process_sisma_csv(path_ats_smi, type = "ATS SMI")
)

df_autoteste <- process_sisma_csv(path_ats_auto, 
                                  type = "ATS Auto")

df_apss <- process_sisma_csv(path_hiv_apss, 
                             type = "HIV APSS")

df_its <- process_sisma_csv(path_hiv_its,
                            type = "HIV ITS")

df_prep <- process_sisma_csv(path_hiv_prep, 
                             type = "HIV PREP")

df_tarv <- process_sisma_csv(path_hiv_tarv, 
                             type = "HIV TARV")

df_dah <- process_sisma_csv(path_hiv_dah, 
                             type = "HIV DAH")

df_ajmhcmm <- process_sisma_csv(path_hiv_ajmhcmm, 
                             type = "HIV AJMHCMM")

df_smi_ccr <- process_sisma_csv(path_smi_ccr, 
                                type = "SMI-CCR")

df_smi_cpn <- process_sisma_csv(path_smi_cpn,
                                type = "SMI-CPN")

df_smi_mat <- process_sisma_csv(path_smi_mat, 
                                type = "SMI-MAT")

df_smi_cpp <- process_sisma_csv(path_smi_cpp, 
                                type = "SMI-CPP")

df_smi_pav <- process_sisma_csv(path_smi_pav, 
                                type = "SMI-PAV")

df_smi_ccd <- process_sisma_csv(path_smi_ccd, 
                                type = "SMI-CCD")


# WRITE TO DISK -----------------------------------------------------------


write_tsv(df_ats,
          output_ats)

write_tsv(df_autoteste,
          output_ats_auto)

write_tsv(df_tarv,
          output_hiv_tarv)

write_tsv(df_dah,
          output_hiv_dah)

write_tsv(df_ajmhcmm,
          output_hiv_ajmhcmm)

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

write_tsv(df_smi_cpp,
          output_smi_cpp)

write_tsv(df_smi_pav,
          output_smi_pav)

write_tsv(df_smi_ccd,
          output_smi_ccd)

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


historical_tarv <- 
  list.files("Data/processed/", pattern = "^hiv_tarv|^hiv_dah|^ajm_|^hiv_apss_", full.names = TRUE) %>% 
  map(~ read_tsv(.x)) %>%
  reduce(rbind)

  # attach_meta_coord()


historical_smi <- 
  list.files("Data/processed/", pattern = "^smi_", full.names = TRUE) %>% 
  map(~ read_tsv(.x)) %>%
  reduce(rbind)


historical_ats <- 
  list.files("Data/processed/", pattern = "^ats_20|^ats_auto", full.names = TRUE) %>% 
  map(~ read_tsv(.x)) %>%
  reduce(rbind)


historical_prev <- 
  list.files("Data/processed/", pattern = "^hiv_prep_|^hiv_its_", full.names = TRUE) %>% 
  map(~ read_tsv(.x)) %>%
  reduce(rbind)


# WRITE TO LOCAL DISK -----------------------------------------------------


write_tsv(
  historical_tarv,
  "Dataout/db_tarv.txt",
  na = ""
)


write_tsv(
  historical_smi,
  "Dataout/db_smi.txt",
  na = ""
)

write_tsv(
  historical_ats,
  "Dataout/db_ats.txt",
  na = ""
)

write_tsv(
  historical_prev,
  "Dataout/db_prev.txt",
  na = ""
)


# WRITE TO GOOGLE DRIVE ---------------------------------------------------


drive_put("Dataout/db_tarv.txt",
          path = path_historic_output_gdrive)

drive_put("Dataout/db_smi.txt",
          path = path_historic_output_gdrive)

drive_put("Dataout/db_ats.txt",
          path = path_historic_output_gdrive)

drive_put("Dataout/db_prev.txt",
          path = path_historic_output_gdrive)
