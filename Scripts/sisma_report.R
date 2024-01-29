library(tidyverse)
library(lubridate)
library(sismar)
library(glamr)


# Setting up credentials (only refresh if there is an error) ----------

glamr::load_secrets()

#Global
PERIOD <- "2023-03" #used to define a period in a file
YEAR <- "2023"  #used for file(s)
FOLDER_PATH <-  ("Data/test/")
FILE <- "tarv"
FILES <- "(tarv|prep|apss|smi_cpn|smi_ccr)" #adjust for files
MANY_FILES_PATTERN <-  paste0(FILES,"_",YEAR,"\\.csv$")
ONE_FILE_PATTERN <- paste0(FILE,"_",YEAR,"\\.csv$")


input_files <- dir(FOLDER_PATH, full.name = TRUE, pattern = ONE_FILE_PATTERN) # for one file
input_files <- dir(FOLDER_PATH, full.name = TRUE, pattern = MANY_FILES_PATTERN) # if datasets are in a folder

all_data <- map_dfr(input_files, ~report_create_all(.x, period = PERIOD))
