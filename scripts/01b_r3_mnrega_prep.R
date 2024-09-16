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
library(arrow)

source(here("scripts/00_utils.R"))

# Filter, add year, clean names, 
process_r3_files <- function(file_path) {
     
     year <- as.numeric(sub(".*/r3-all-(\\d{4})\\.csv\\.gz", "\\1", file_path))
     
     df <- read_csv(here(file_path)) %>%
          filter(state %in% c("UTTAR PRADESH", "RAJASTHAN")) %>%
          clean_names()
     
     if ("panchayats" %in% names(df)) {
          df <- df %>%
               dplyr::rename(panchayat = panchayats)
     }
     
     df <- df %>%
          mutate(year = year,
                 district = if ("district_2" %in% names(df)) coalesce(district, district_2) else district,
                 block = if ("block_2" %in% names(df)) coalesce(block, block_2) else block,
                 key = normalize_string(paste(district, block, panchayat)),
                 state_key = normalize_string(paste(state, district, block, panchayat))) %>%
          drop_na(district, block, panchayat) %>%
          filter(!(duplicated(state_key) | duplicated(state_key, fromLast = TRUE))) %>% 
          remove_na_columns()
     
     return(df)
}

# Read all csv.gz files in the R3 files directory
file_directory <- "data/mnrega/r3"
files <- list.files(path = file_directory, pattern = "\\.csv\\.gz$", full.names = TRUE)
data_list <- Map(process_r3_files, files)

# Let's agg.
aggregate_yearly <- function(df) {
     df %>%
          mutate(
               yearly_household = rowSums(select(., ends_with("_household")), na.rm = TRUE),
               yearly_persons = rowSums(select(., ends_with("_persons")), na.rm = TRUE)
          )
}

# Apply the function to each data frame in the list
dfs_aggregated <- lapply(data_list, aggregate_yearly)

# Combine
mnrega_r3_long <- bind_rows(dfs_aggregated)

## To wide
mnrega_r3_wide <- mnrega_r3_long %>%
     select(key, year, state_key, state, district, block, panchayat, yearly_household, yearly_persons) %>%
     group_by(state_key) %>%
     filter(n() == length(unique(year))) %>%  # Keep only ids with all years present
     ungroup() %>%
     pivot_wider(id_cols = c(state_key, state, district, block, panchayat),
                 names_from = year, 
                 values_from = yearly_household:yearly_persons,
                 names_sep = "_")

mnrega_r3_wide <- mnrega_r3_wide %>%
     mutate(
          avg_hh_14_15 = rowMeans(across(c(
               yearly_household_2014,
               yearly_household_2015
          )), na.rm = TRUE),
          
          avg_person_14_15 = rowMeans(across(c(
               yearly_persons_2014,
               yearly_persons_2015
          )), na.rm = TRUE),
          
     )

write_parquet(mnrega_r3_wide, here("data/mnrega/mnrega_r3.parquet"))
