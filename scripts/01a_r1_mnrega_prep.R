# Load libs
library(dplyr)
library(janitor)
library(fuzzyjoin)
library(readr)
library(tidyr)
library(stringi)
library(here)
library(arrow)

source(here("scripts/00_utils.R"))

# Function to process each file
process_r1_files <- function(file_path) {
     
     year <- as.numeric(sub(".*/r1-all-(\\d{4})\\.csv\\.gz", "\\1", file_path))

     df <- read_csv(file_path) %>%
          filter(state %in% c("UTTAR PRADESH", "RAJASTHAN")) %>%
          clean_names() %>%
          dplyr::rename(panchayat = panchayats)
     
     if ("blocks" %in% names(df)) {
          df <- df %>%
               mutate(block = coalesce(block, blocks),
                      district = coalesce(district, district))
     }

     df <- df %>% mutate(
              year = year,
              key = normalize_string(paste(district, block, panchayat)),
              state_key =  normalize_string(paste(state, district, block, panchayat))) %>%
          drop_na(district, block, panchayat) %>%
          filter(!(duplicated(state_key) | duplicated(state_key, fromLast = TRUE))) %>%
          remove_na_columns()
     
     return(df)
}

# Directory containing the R1 files
file_directory <- here("data/mnrega/r1")
files <- list.files(path = file_directory, pattern = "\\.csv\\.gz$", full.names = TRUE)
data_list <- Map(process_r1_files, files)

# Combine
mnrega_r1_long <- bind_rows(data_list)

# Wide
mnrega_r1_wide <- mnrega_r1_long %>%
     group_by(state_key) %>%
     filter(n() == length(unique(year))) %>%
     ungroup() %>%
     pivot_wider(id_cols = c(state_key, state, district, block, panchayat),
                 names_from = year, 
                 values_from = c(number_of_jobcards_applied_for:active_workers_women),
                 names_sep = "_")



write_parquet(mnrega_r1_wide, here("data/mnrega/mnrega_r1.parquet"))
