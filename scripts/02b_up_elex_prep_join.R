# Load libs.
library(readr)
library(arrow)
library(tidyverse)
library(stringi)
library(fuzzyjoin)
library(here)
library(progress)

# Load script
source(here("scripts/00_utils.R"))

# Load dat
up_2005 <- read_parquet(here("data/up/up_gp_sarpanch_2005_fixed_with_transliteration.parquet"))
up_2010 <- read_parquet(here("data/up/up_gp_sarpanch_2010_fixed_with_transliteration.parquet"))
up_2015 <- read_parquet(here("data/up/up_gp_sarpanch_2015_fixed_with_transliteration.parquet"))
up_2021 <- read_parquet(here("data/up/up_gp_sarpanch_2021_fixed_with_transliteration.parquet"))

# Jeff
## Notes from Jeff
### The variable gp_code_jj11 should be the unique identifier for the 2010 elections, 
# while gp_code_lgd21 will be the one for the 2015 and 2021 elections.  The 2010 data isn't as good quality
# since we didn't really use that much for our project, but the 2015-2021 match should be better. 
jeff <- haven::read_dta((here("data/up/weaver_data.dta"))) 

# Let's filter to winners for 2021
up_2021 <- up_2021 %>% filter(result == 'विजेता')

# See https://en.wikipedia.org/wiki/Kanpur_Dehat_district
up_2010$district_name     <- ifelse(up_2010$district_name == "रमाबाई नगर", "कानपुर देहात", up_2010$district_name)
up_2010$district_name_eng <- ifelse(up_2010$district_name_eng == "Ramabai Nagar", "Kanpur Dehat", up_2010$district_name_eng)

# Phase I Districts:  Azamgarh, Banda, Barabanki, Chandauli, Chitrakoot, Fatehpur, Gorakhpur, Hamirpur, Hardoi, Jalaun, Jaunpur, Kaushambi, Kushinagar, Lakimpur Kheri, Lalitpur, Mahoba, Mirzapur, Pratapgarh, Rae Bareli, Sitapur, Sonbhadra, Unnao  
# Phase II Districts: Ambedkar Nagar, Bahraich, Ballia, Balrampur, Basti, Budaun, Etah, Farrukhabad, Gonda, Jhansi, Maharajganj, Mau, Ramabai Nagar (Kanpur Dehat), Sant Kabir Nagar,  Shrawasti, Siddharth Nagar, Sultanpur
# Phase III Districts: Bijnor, Moradabad, Rampur, Saharanpur, Muzaffarnagar, Meerut, Ghaziabad, Bulandshahar, Aligarh, Mathura, Agra, Firozabad, Mainpuri, Bareilly, Pilibhit, Shahjahanpur, Lucknow, Kanpur Nagar, Allahabad, Faizabad, Deoria, Etawah, Ghazipur, Varanasi, Gautam Buddha Nagar, Baghpat, Hathras, Kannauj, Jyotiba Phule Nagar, Auraiya, Sant Ravidas Nagar, Kanshiram Nagar

# change spellings based on the db
phase_1 <- unlist(strsplit("Azamgarh, Banda, Barabanki, Chandauli, Chitrakoot, Fatehpur, Gorakhpur, Hamirpur, Hardoi, Jalaun, Jaunpur, Kaushambi, Kushinagar, Lakimpur Khiri, Lalitpur, Mahoba, Mirzapur, Pratapgarh, Rae bareli, Sitapur, Sonbhadra, Unnao", ", "))
phase_2 <- unlist(strsplit("Ambedkar Nagar, Bahraich, Baliya, Balrampur, Basti, Budaun, Eta, Farrukhabad, Gonda, Jhanshi, Maharajganj, Mau, Kanpur Dehat, Sant Kabeer Nagar, Shrawasti, Siddharth Nagar, Sultanpur", ", "))
phase_3 <- unlist(strsplit("Agra, Allahabad, Aligarh, Auraiya, Bagpat, Bareilly, Bijnaur, Bulandshahr, Devaria, Faizabad, Firozabad, Gautam Buddha Nagar, Ghaziabad, Ghazipur, Hathras, Jyotiba Phule Nagar, Itawah, Kanpur Nagar, Kannauj, Kanshiram Nagar, Lucknow, Mathura, Mainpuri, Meerut, Muradabad, Muzaffarnagar, Pilibhit, Rampur, Saharanpur, Shahjahanpur, Sant Ravidas Nagar, Varanasi", ", "))
     
# Transform
up_2005_dedupe <- up_2005 %>%
     mutate(female_res = grepl("Female", gp_res_status_fin_eng, ignore.case = TRUE),
            key = normalize_string(paste(district_name, block_name, gp_name_fin)),
            eng_key = normalize_string(paste(district_name_eng, block_name_eng, gp_name_eng)),
            phase_1 = district_name_eng %in% phase_1,
            phase_2 = district_name_eng %in% phase_2, 
            phase_3 = district_name_eng %in% phase_3,
            phase_1_bose = district_name_eng %in% c("Barabanki", "Chitrakoot", "Fatehpur", "Hardoi", "Lakimpur Khiri"),
            phase_2_bose = district_name_eng %in% c("Ambedkar Nagar", "Basti", "Eta", "Jhanshi")) %>%
     filter (!duplicated(key)) %>%
     filter(!is.na(key)) %>%
     filter(!is.na(gp_res_status_fin_eng) & (gp_res_status_fin_eng != "Unknown"))

up_2010_dedupe <- up_2010 %>%
     mutate(female_res = grepl("Female", gp_res_status_fin_eng, ignore.case = TRUE),
            key = normalize_string(paste(district_name, block_name, gp_name_fin)),
            eng_key = normalize_string(paste(district_name_eng, block_name_eng, gp_name_eng)),
            phase_1 = district_name_eng %in% phase_1,
            phase_2 = district_name_eng %in% phase_2, 
            phase_3 = district_name_eng %in% phase_3) %>%
     filter (!duplicated(key)) %>%
     filter(!is.na(key)) %>%
     filter(!is.na(gp_res_status_fin_eng) & (gp_res_status_fin_eng != "Unknown"))

up_2015_dedupe <- up_2015 %>%
     mutate(female_res = grepl("Female", gp_reservation_status_eng, ignore.case = TRUE),
            key = normalize_string(paste(district_name, block_name, gp_name)),
            eng_key = normalize_string(paste(district_name_eng, block_name_eng, gp_name_eng)),
            phase_1 = district_name_eng %in% phase_1,
            phase_2 = district_name_eng %in% phase_2, 
            phase_3 = district_name_eng %in% phase_3) %>%
     filter (!duplicated(key)) %>%
     filter(!is.na(key)) %>%
     filter(!is.na(gp_reservation_status_eng) & (gp_reservation_status_eng != "Unknown"))

up_2021_dedupe <- up_2021 %>%
     mutate(female_res = grepl("Female", gp_reservation_status_eng, ignore.case = TRUE),
            key = normalize_string(paste(district_name, block_name, gp_name)),
            eng_key = normalize_string(paste(district_name_eng, block_name_eng, gp_name_eng)),
            phase_1 = district_name_eng %in% phase_1,
            phase_2 = district_name_eng %in% phase_2, 
            phase_3 = district_name_eng %in% phase_3) %>%
     filter (!duplicated(key)) %>%
     filter(!is.na(key)) %>%
     filter(!is.na(gp_reservation_status_eng) & (gp_reservation_status_eng != "Unknown"))

# Add suffix
up_2005_dedupe_suff <- up_2005_dedupe %>%
     rename_with(~ paste0(., "_2005"))

up_2010_dedupe_suff <- up_2010_dedupe %>%
     rename_with(~ paste0(., "_2010"))

up_2015_dedupe_suff <- up_2015_dedupe %>%
     rename_with(~ paste0(., "_2015"))

up_2021_dedupe_suff <- up_2021_dedupe %>%
     rename_with(~ paste0(., "_2021"))

# Join
up_05_10 <- inner_join(up_2005_dedupe_suff, 
                       up_2010_dedupe_suff, 
                       by = c("key_2005" = "key_2010"))

write_parquet(up_05_10, sink = here("data/up/up_05_10_inner.parquet"))

up_05_10_15 <- inner_join(up_05_10, 
                          up_2015_dedupe_suff, 
                          by = c("key_2005" = "key_2015"))

up_all   <- inner_join(up_05_10_15, 
                       up_2021_dedupe_suff, 
                       by = c("key_2005" = "key_2021"))

write_parquet(up_all, sink = here("data/up/up_all_inner.parquet"))

# Fuzzy join
process_row <- function(row, 
                        df2, 
                        key1, 
                        key2, 
                        match_column1, 
                        match_column2, 
                        method = "jw", 
                        distance_col = "distance") {
     
     election_subset <- df2 %>%
          filter(tolower(!!sym(match_column2)) == tolower(row[[match_column1]]))
     
     # If no matching district found, return NULL
     if (nrow(election_subset) == 0) return(NULL)
     
     match_result <- stringdist_left_join(
          row,
          election_subset,
          by = setNames(key2, key1),
          method = method,
          ignore_case = TRUE,
          distance_col = distance_col
     ) %>%
          slice_min(!!sym(distance_col))
     
     return(match_result)
}

# Function to apply process_row_generalized across all rows with progress bar
apply_matching <- function(df1, 
                           df2, 
                           key1, 
                           key2, 
                           match_column1, 
                           match_column2, 
                           method = "jw", 
                           distance_col = "distance") {
     
     pb <- progress_bar$new(
          format = "[:bar] :current/:total (:percent) :elapsedfull",
          total = nrow(df1),
          clear = FALSE
     )
     
     processed_results <- df1 %>%
          split(seq(nrow(.))) %>%
          map_dfr(~ {
               pb$tick()
               process_row(.x, df2, key1, key2, match_column1, match_column2, method, distance_col)
          })
     
     return(processed_results)
}

process_matched_dataframe <- function(matched_df, 
                                      distance_threshold = 0.1, 
                                      distance_col = "distance", 
                                      group_by_cols_x, 
                                      group_by_cols_y) {
     matched_df %>%
          filter(!!sym(distance_col) < distance_threshold) %>%
          group_by(across(all_of(group_by_cols_x))) %>%
          mutate(dup_x = n() > 1) %>%
          ungroup() %>%
          group_by(across(all_of(group_by_cols_y))) %>%
          mutate(dup_y = n() > 1) %>%
          ungroup() %>%
          filter(!dup_x & !dup_y) %>%
          select(-dup_x, -dup_y)
}

up_05_10_f <- apply_matching(
     up_2005_dedupe_suff,
     up_2010_dedupe_suff,
     key1 = "key_2005",
     key2 = "key_2010",
     match_column1 = "district_name_2005",
     match_column2 = "district_name_2010",
     method = "jw",
     distance_col = "up_05_10_dist"
)

up_05_10_ff <- process_matched_dataframe(
     up_05_10_f,
     distance_threshold = 0.1,
     distance_col = "up_05_10_dist",
     group_by_cols_x = c("key_2005"),
     group_by_cols_y = c("key_2010")
)

up_05_10_15_f <- apply_matching(
     up_05_10_ff,
     up_2015_dedupe_suff,
     key1 = "eng_key_2010",
     key2 = "eng_key_2015",
     match_column1 = "district_name_2010",
     match_column2 = "district_name_2015",
     method = "jw",
     distance_col = "up_05_10_15_dist"
)

up_05_10_15_ff <- process_matched_dataframe(
     up_05_10_15_f,
     distance_threshold = 0.1,
     distance_col = "up_05_10_15_dist",
     group_by_cols_x = c("eng_key_2010"),
     group_by_cols_y = c("eng_key_2015")
)

up_05_10_15_21_f <- apply_matching(
     up_05_10_15_ff,
     up_2021_dedupe_suff,
     key1 = "eng_key_2015",
     key2 = "eng_key_2021",
     match_column1 = "district_name_2015",
     match_column2 = "district_name_2021",
     method = "jw",
     distance_col = "up_05_10_15_21_dist"
)

up_05_10_15_21_ff <- process_matched_dataframe(
     up_05_10_15_21_f,
     distance_threshold = 0.1,
     distance_col = "up_05_10_15_21_dist",
     group_by_cols_x = c("eng_key_2015"),
     group_by_cols_y = c("eng_key_2021")
)

write_parquet(up_05_10_ff, sink = here("data/up/up_05_10_fuzzy.parquet"))
write_parquet(up_05_10_15_ff, sink = here("data/up/up_05_10_15_fuzzy.parquet"))
write_parquet(up_05_10_15_21_ff, sink = here("data/up/up_all_fuzzy.parquet"))

## Compliance seems low in UP
summary(lm(winner_female ~ reservation_female, data = jeff))
summary(lm(I(cand_sex_fin_2005 == "महिला") ~ female_res_2005, data = up_2005_dedupe_suff))
summary(lm(I(cand_sex_fin_2010 == "महिला") ~ female_res_2010, data = up_2010_dedupe_suff))
summary(lm(I(sex_2015 == "महिला") ~ female_res_2015, data = up_2015_dedupe_suff))
summary(lm(I(sex_2021== "महिला") ~ female_res_2021, data = up_2021_dedupe_suff))

# Merge with Jeff's data

##     select(election, 
##            district_code_ele15, districtname_lgd21, districtcode_lgd21, 
##            block_code_ele15, blockcode_lgd21, 
##            gp_code_jj11, gp_code_ele15, gp_code_ele20, gp_code_lgd21, 
##            gp_name_jj11, gp_name_ele15, gp_name_ele15_t, gp_name_lgd21,
##     )

