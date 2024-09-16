# Load libs
library(stringr)
library(dplyr)
library(janitor)
library(stringi)
library(readr)
library(reshape)
library(tidyr)
library(purrr)
library(here)

source(here("scripts/00_utils.R"))

# Filter, add year, clean names, 
process_r5_files <- function(file_path) {
     
     year <- as.numeric(sub(".*/r5_5-all-(\\d{4})\\.csv\\.gz", "\\1", file_path))
     
     df <- read_csv(file_path) %>%
          filter(state %in% c("UTTAR PRADESH", "RAJASTHAN")) %>%
          clean_names()

     if ("x2_panchayat" %in% names(df)) {
          df <- df %>%
               dplyr::rename(panchayat = x2_panchayat)
     }
     
     df <- df %>%
          mutate(year = year,
                 district = if ("x2_district" %in% names(df)) coalesce(district, x2_district) else district,
                 block = if ("x2_block" %in% names(df)) coalesce(block, x2_block) else block,
                 key = normalize_string(paste(district, block, panchayat)),
                 state_key = normalize_string(paste(state, district, block, panchayat))) %>%
          drop_na(district, block, panchayat) %>%
          filter(!(duplicated(state_key) | duplicated(state_key, fromLast = TRUE))) %>% 
          remove_na_columns()
     
     return(df)
}

# Read all csv.gz files in the R3 files directory
file_directory <- "data/mnrega/r5"
files <- list.files(path = file_directory, pattern = "\\.csv\\.gz$", full.names = TRUE)
data_list <- Map(process_r5_files, files)

# Combine
mnrega_r5_long <- bind_rows(data_list)

## To wide
mnrega_r5_wide <- mnrega_r5_long %>%
     group_by(state_key) %>%
     filter(n() == length(unique(year))) %>%  # Keep only ids with all years present
     ungroup() %>%
     pivot_wider(id_cols = c(state_key, state, district, block, panchayat),
                 names_from = year, 
                 values_from = x3_a_hh_issued_jobcards_s_cs:x6_d_families_completed_100_days_total,
                 names_sep = "_")

write_parquet(mnrega_r5_wide, "data/mnrega/mnrega_r5.parquet")
