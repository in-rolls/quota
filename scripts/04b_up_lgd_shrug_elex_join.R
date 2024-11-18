# Load libs
library(tidyverse)
library(readr)
library(stringdist)
library(fuzzyjoin)
library(magrittr)
library(here)
library(stringi)
library(arrow)
library(readr)
library(progress)
library(janitor)

# Load utils
source(here("scripts/00_utils.R"))

# Load elex
elex_up_05_10 <- read_parquet(here("data/up/up_05_10_fuzzy.parquet"))
mnrega_elex_raj_05_10 <- read_parquet(here("data/raj/mnrega_elex_raj_05_10.parquet"))

elex_up_05_10 <- elex_up_05_10 %>%
     mutate(lgd_match_name = normalize_string(gsub(" ", "", paste0(district_name_eng_2010, block_name_eng_2010, gp_name_eng_2010))))

# Load dat
lgd <- read_csv(here("data/lgd/up_village_gp_mapping_2024.csv")) %>% 
     clean_names() %>%
     mutate(state_id = "09") %>%
     group_by(district_name, subdistrict_name, local_body_name, village_name) %>%
     filter(n() == 1) %>%
     ungroup()

# VD, PCA
sh_pca_01 <- read_csv(here("data/shrug/shrug-pca01-csv/pc01_pca_clean_shrid.csv"))
sh_pca_01 <- sh_pca_01 %>%
     separate(shrid2, 
              into = c("census_yr", 
                       "state_id", 
                       "district_census_2011_code", 
                       "subdistrict_census_2011_code", 
                       "village_census_2011_code"), 
              sep = "-",
              remove = FALSE)

# We may want to remove cases where town is included: subdistrict = 00
# https://docs.devdatalab.org/SHRUG-Construction-Details/location-identifiers/town-and-village-identifiers/
sh_pca_01 <- sh_pca_01 %>% 
     filter(subdistrict_census_2011_code != "00") %>%
     filter(state_id == "08" | state_id == "09")

## Merge with LGD
shrug_lgd <- sh_pca_01 %>%
     inner_join(lgd, by = c("state_id", 
                            "district_census_2011_code", 
                            "subdistrict_census_2011_code", 
                            "village_census_2011_code")) %>%
     mutate(state = ifelse(state_id == "08", "Rajasthan", "Uttar Pradesh"),
            key = gsub(" ", "", normalize_string(paste(district_name, subdistrict_name, local_body_name))),
            state_key = gsub(" ", "", normalize_string(paste(state, district_name, subdistrict_name, local_body_name)))) %>%
     filter(state == "Uttar Pradesh")

## Merge to Elex (later to MNREGA raj)
shrug_lgd_elex_05_10_strict <- shrug_lgd[shrug_lgd$state == "Uttar Pradesh", ] %>%
     inner_join(elex_up_05_10, by = c("key" = "lgd_match_name"))

# Process row
process_row <- function(row, right_table) {

     right_table_subset <- right_table %>%
          filter(normalize_string(row$district_name_eng_2010) == normalize_string(district_name))
     
     # If no matching district found, return NULL
     if (nrow(right_table_subset) == 0) return(NULL)
     
     match_result <- stringdist_left_join(
          row,
          right_table_subset,
          by = c("lgd_match_name" = "key"),
          method = "jw",
          ignore_case = TRUE,
          distance_col = "dist_elex_lgd_match"
     ) %>%
          slice_min(dist_elex_lgd_match)
     
     return(match_result)
}

# Mini. SHRUG
base_shrug_cols <- c("district_name", "district_code", 
                     "district_census_2001_code", "subdistrict_name", "subdistrict_census_2001_code",
                     "village_name", "village_census_2001_code",
                     "state", "state_key", "shrid2", "key",
                     "local_body_name", "local_body_code")
shrug_lgd_raj <- shrug_lgd[shrug_lgd$state == "Uttar Pradesh", base_shrug_cols]

shrug_lgd_elex_05_10 <- elex_up_05_10 %>%
     split(seq(nrow(.))) %>%
     map_dfr(~ process_row(.x, shrug_lgd))

# Filter down to where  dist_mnrega_match < .15. 
shrug_lgd_elex_05_10_refine <- shrug_lgd_elex_05_10 %>%
     filter(dist_elex_lgd_match < 0.15)

write_parquet(shrug_lgd_elex_05_10_strict, sink = here("data/up/shrug_lgd_up_elex_05_10_strict.parquet"))
write_parquet(shrug_lgd_elex_05_10_refine, sink = here("data/up/shrug_lgd_up_elex_05_10.parquet"))
