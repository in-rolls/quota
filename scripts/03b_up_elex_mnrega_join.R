# Load libs
library(readr)
library(tidyverse)
library(stringi)
library(stringdist)
library(dplyr)
library(purrr)
library(progress)
library(here)
library(arrow)
library(fuzzyjoin)

source(here("scripts/00_utils.R"))

# Load dat
mnrega_r1 <- read_parquet(here("data/mnrega/mnrega_r1.parquet"))
mnrega_r3 <- read_parquet(here("data/mnrega/mnrega_r3.parquet"))
mnrega_r5 <- read_parquet(here("data/mnrega/mnrega_r5.parquet"))
mnrega_r6 <- read_parquet(here("data/mnrega/mnrega_r6.parquet"))
elex_up_05_10 <- read_parquet(here("data/up/up_05_10_fuzzy.parquet"))

# Join MNREGA Reports
mnrega_up_dupes <- mnrega_r6 %>%
     inner_join(mnrega_r1, by = "state_key") %>%
     inner_join(mnrega_r3, by = "state_key") %>%
     inner_join(mnrega_r5, by = "state_key") %>%
     filter(state.x == "UTTAR PRADESH")

mnrega_up <- mnrega_up_dupes %>%
     select(-contains(".y"), -contains(".x.x"), -contains(".y.y")) %>%
     rename_with(~ str_remove(., "\\..*$"))

mnrega_up <- mnrega_up %>%
     mutate(district = tolower(district),
            match_name = gsub(" ", "", normalize_string(paste(district, block, panchayat)))
     )

elex_up_05_10 <- elex_up_05_10 %>%
     mutate(match_name = gsub(" ", "", normalize_string(paste(district_name_eng_2010, block_name_eng_2010, gp_name_eng_2010)))
     )

# Process each row
process_row <- function(row, right_table) {
     
     right_table_subset <- right_table %>%
          filter(tolower(district) == tolower(row$district_name_eng_2010))
     
     # If no matching district found, return NULL
     if (nrow(right_table_subset) == 0) return(NULL)
     
     match_result <- stringdist_left_join(
          row,
          right_table_subset,
          by = "match_name",
          method = "jw",
          ignore_case = TRUE,
          distance_col = "dist_mnrega_match"
     ) %>%
          slice_min(dist_mnrega_match)
     
     return(match_result)
}

mnrega_elex_up_05_10 <- elex_up_05_10 %>%
     split(seq(nrow(.))) %>%
     map_dfr(~ process_row(.x, mnrega_up))

# Remove duplicates and filter down to where  dist_mnrega_match < .1
mnrega_elex_up_05_10_dedupe <- mnrega_elex_up_05_10 %>%
     filter(dist_mnrega_match < 0.1) %>%
     group_by(match_name.x) %>%
     filter(n() == 1) %>%
     ungroup() %>%
     group_by(match_name.y) %>%
     filter(n() == 1) %>%
     ungroup()

write_parquet(mnrega_elex_up_05_10_dedupe, sink = here("data/up/mnrega_elex_up_05_10.parquet"))
