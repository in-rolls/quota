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
elex_raj_05_10 <- read_parquet(here("data/raj/elex_raj_05_10.parquet"))
mnrega_elex_raj_05_10 <- read_parquet(here("data/raj/mnrega_elex_raj_05_10.parquet"))

# Rajasthan Districts line up except for Gehlot; Sawai Madhopur is handled with gsub space
#strings <- paste(sort(unique(lgd_raj$subdistrict_name)), collapse = "\n")
#cat(samiti_names_string, file = pipe("pbcopy"))
elex_lgd <- read_csv(here("data/raj/elex_lgd_crosswalk.csv")) %>%
     filter(type == 'subdistrict') %>%
     filter(!is.na(elex_area))

elex_raj_05_10 <- elex_raj_05_10 %>%
     left_join(elex_lgd, by = c("samiti_name_new_2010" = "elex_area")) %>%
     filter(!is.na(lgd_area)) %>%
     mutate(lgd_match_name = gsub(" ", "", paste0(dist_name_new_2010, lgd_area, gp_new_2010)))

mnrega_elex_raj_05_10 <- mnrega_elex_raj_05_10 %>%
     left_join(elex_lgd, by = c("samiti_name_new_2010" = "elex_area")) %>%
     filter(!is.na(lgd_area)) %>%
     mutate(lgd_match_name = gsub(" ", "", paste0(dist_name_new_2010, lgd_area, gp_new_2010)))

# Load dat
lgd_raj <- read_csv(here("data/lgd/raj_village_gp_mapping_2024.csv")) %>% 
     clean_names() %>%
     mutate(state_id = "08") %>%
     group_by(district_name, subdistrict_name, local_body_name, village_name) %>%
     filter(n() == 1) %>%
     ungroup()

lgd_up <- read_csv(here("data/lgd/up_village_gp_mapping_2024.csv")) %>% 
     clean_names() %>%
     mutate(state_id = "09") %>%
     group_by(district_name, subdistrict_name, local_body_name, village_name) %>%
     filter(n() == 1) %>%
     ungroup()

lgd <- bind_rows(lgd_raj, lgd_up)



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
            state_key = gsub(" ", "", normalize_string(paste(state, district_name, subdistrict_name, local_body_name))))

## Merge to Elex (later to MNREGA raj)
shrug_lgd_elex_05_10_strict <- shrug_lgd[shrug_lgd$state == "Rajasthan", ] %>%
     inner_join(elex_raj_05_10, by = c("key" = "match_name"))

# Process row
process_row <- function(row, right_table) {

     right_table_subset <- right_table %>%
          filter(normalize_string(row$dist_name_new_2010) == normalize_string(district_name))
     
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
shrug_lgd_raj <- shrug_lgd[shrug_lgd$state == "Rajasthan", base_shrug_cols]

shrug_lgd_elex_05_10 <- elex_raj_05_10 %>%
     split(seq(nrow(.))) %>%
     map_dfr(~ process_row(.x, shrug_lgd_raj))

# Filter down to where  dist_mnrega_match < .15. 
shrug_lgd_elex_05_10_refine <- shrug_lgd_elex_05_10 %>%
     filter(dist_elex_lgd_match < 0.15)

write_parquet(shrug_lgd_elex_05_10_strict, sink = here("data/raj/shrug_lgd_raj_elex_05_10_strict.parquet"))
write_parquet(shrug_lgd_elex_05_10_refine, sink = here("data/raj/shrug_lgd_raj_elex_05_10.parquet"))
write_parquet(shrug_lgd_elex_05_10, sink = here("data/raj/shrug_lgd_raj_elex_05_10_raw.parquet"))
