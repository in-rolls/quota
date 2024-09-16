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

coalesce_columns <- function(df) {
     # Get unique column names
     unique_colnames <- unique(colnames(df))
     
     # Coalesce columns with the same name
     df_coalesced <- map_dfc(unique_colnames, function(colname) {
          if (sum(colnames(df) == colname) > 1) {
               coalesced_col <- coalesce(!!!df[colnames(df) == colname])
               tibble(!!colname := coalesced_col)
          } else {
               df %>% select(!!colname)
          }
     })
     
     return(df_coalesced)
}

# Fix Col. Names
replace_column_names <- function(df) {
     colnames(df) <- gsub("sankhya_vyaya_lakhom_mem", "in_lakhs", colnames(df))
     colnames(df) <- gsub("nos_expenditure", "", colnames(df))
     colnames(df) <- gsub("purna", "completed", colnames(df))
     colnames(df) <- gsub("_comp$", "_completed", colnames(df))
     colnames(df) <- gsub("cala_rahe_nilambita", "ongoing_suspended", colnames(df))
     colnames(df) <- gsub("svikrta_karya", "approved_not_in_progress", colnames(df))
     
     colnames(df) <- gsub("gramina_sanyojakata", "rural_connectivity", colnames(df))
     colnames(df) <- gsub("sukharodhana", "drought_proofing", colnames(df))
     colnames(df) <- gsub("bhumi_vikasa", "land_development", colnames(df))
     colnames(df) <- gsub("jala_sanraksana_aura_jala_sangrahana", "water_conservation_and_water_harvesting", colnames(df))
     colnames(df) <- gsub("kula", "total", colnames(df))
     colnames(df) <- gsub("an_ya_karya", "other_works", colnames(df))
     colnames(df) <- gsub("paramparagata_jala_nikaya_ka_navikarana", "renovation_of_traditional_water_bodies", colnames(df))
     colnames(df) <- gsub("esasi_esati_a_i_eva_i_ela_ara_labhagrahi_ko_sinca_i_suvidha_e", "provision_of_irrigation_facility_to_land_owned_by_sc_st_lr_or_iay_beneficiaries_small_or_marginal_farmers", colnames(df))
     colnames(df) <- gsub("irrigation_facilities_to_sc_st_iay_lr", "provision_of_irrigation_facility_to_land_owned_by_sc_st_lr_or_iay_beneficiaries_small_or_marginal_farmers", colnames(df))
     colnames(df) <- gsub("suksma_sinca_i_karya", "micro_irrigation_works", colnames(df))
     
     colnames(df) <- gsub("barha_niyantrana_aura_sanraksana", "flood_control_and_protection", colnames(df))
     colnames(df) <- gsub("flood_control_in", "flood_control_and_protection_in", colnames(df))
     
     colnames(df) <- gsub("bharata_nirmana", "bharat_nirman", colnames(df))
     colnames(df) <- gsub("rajeev_gandhi_sewa_kendra", "seva_kendra", colnames(df))
     colnames(df) <- gsub("seva", "sewa", colnames(df))
     
     colnames(df) <- gsub("krama_sankhya", "s_no", colnames(df))
     
     colnames(df) <- gsub("__", "_", colnames(df))
     
     return(df)
}

# Filter, add year, clean names
process_r6_files <- function(file_path, year) {
     
     year <- as.numeric(sub(".*/r6-all-(\\d{4})\\.csv\\.gz", "\\1", file_path))
     
     df <- read_csv(file_path) %>%
          filter(state %in% c("UTTAR PRADESH", "RAJASTHAN")) %>%
          clean_names()
     
     if ("pancayata" %in% names(df)) {
          df <- df %>%
               mutate(panchayat = coalesce(panchayat, pancayata))
     }

     df <- df %>%
          mutate(year = year,
                 key = normalize_string(paste(district, block, panchayat)),
                 state_key = normalize_string(paste(state, district, block, panchayat))) %>%
          drop_na(district, block, panchayat) %>%
          filter(!(duplicated(state_key) | duplicated(state_key, fromLast = TRUE))) %>%
          remove_na_columns() %>%
          replace_column_names() %>%
          coalesce_columns() %>%
          remove_na_columns()
     
     return(df)
}

# Directory containing the R6 files
file_directory <- here("data/mnrega/r6")
files <- list.files(path = file_directory, 
                    pattern = "\\.csv\\.gz$", 
                    full.names = TRUE)
data_list <- Map(process_r6_files, files)
# map_int(data_list, count_duplicate_colnames)

# Combine
cdf <- bind_rows(data_list, .id = "id") %>%
     mutate(total_in_lakhs_completed  = coalesce(total_in_lakhs_completed, nan_in_lakhs_completed))

missing_by_year <- function(df) {
     df %>%
          group_by(year) %>%
          summarise(
               total_observations = n(),
               across(everything(), list(missing = ~ sum(is.na(.))), .names = "{.fn}_{.col}")
          )
}

write_csv(missing_by_year(cdf), file = here("data/mnrega/mnrega_r6_missing_by_year.csv"))

# 2017 needs ifelse

## Custom function to parse the info in the columns - number of projects and expenditure are both mashed together into one observation.
cleanup_function <- function(df, to_clean_name, new_name, type) {
     if (!to_clean_name %in% colnames(df)) {
          stop(paste("Column", to_clean_name, "does not exist in the dataframe."))
     }
     
     temp <- df %>%
          select(any_of(to_clean_name)) %>%
          mutate(across(everything(), ~ gsub("  ", " ", .))) %>%
          as.data.frame()
     
     print(to_clean_name)
     split_cols <- str_split_fixed(temp[[1]], " ", 2)
     
     if (ncol(split_cols) != 2) {
          stop("The split operation did not produce exactly two columns.")
     }
     
     split_df <- as.data.frame(split_cols)
     colnames(split_df) <- c(paste(new_name, type, "project", sep = "_"), paste(new_name, type, "expenditure", sep = "_"))
     
     split_df <- split_df %>%
          mutate(across(everything(), as.numeric))
     
     return(split_df)
}

# Define the columns, new names, and types
columns_to_clean <- list(
     c("anganwadi_other_rural_infrastructure_in_lakhs_completed", "childcare", "comp"),
     c("anganwadi_other_rural_infrastructure_in_lakhs_ongoing_suspended", "childcare", "ongoing"),
     c("renovation_of_traditional_water_bodies_in_lakhs_completed", "water_trad", "comp"),
     c("renovation_of_traditional_water_bodies_in_lakhs_ongoing_suspended", "water_trad", "ongoing"),
     c("rural_connectivity_in_lakhs_completed", "connectivity", "comp"),
     c("rural_connectivity_in_lakhs_ongoing_suspended", "connectivity", "ongoing"),
     c("rural_drinking_water_in_lakhs_completed", "drinking_water", "comp"),
     c("rural_drinking_water_in_lakhs_ongoing_suspended", "drinking_water", "ongoing"),
     c("rural_sanitation_in_lakhs_completed", "sanitation", "comp"),
     c("rural_sanitation_in_lakhs_ongoing_suspended", "sanitation", "ongoing"),
     c("water_conservation_and_water_harvesting_in_lakhs_completed", "water_conserve", "comp"),
     c("water_conservation_and_water_harvesting_in_lakhs_ongoing_suspended", "water_conserve", "ongoing"),
     c("total_in_lakhs_completed", "total", "comp"),
     c("total_in_lakhs_ongoing_suspended", "total", "ongoing"),
     c("drought_proofing_in_lakhs_completed", "drought", "comp"),
     c("drought_proofing_in_lakhs_ongoing_suspended", "drought", "ongoing"),
     c("works_on_individuals_land_category_iv_in_lakhs_completed", "land_dev", "comp"),
     c("works_on_individuals_land_category_iv_in_lakhs_ongoing_suspended", "land_dev", "ongoing"),
     c("micro_irrigation_works_in_lakhs_completed", "micro_irrig", "comp"),
     c("micro_irrigation_works_in_lakhs_ongoing_suspended", "micro_irrig", "ongoing"),
     c("flood_control_and_protection_in_lakhs_completed", "flood", "comp"),
     c("flood_control_and_protection_in_lakhs_ongoing_suspended", "flood", "ongoing")
)

# Apply the cleanup function and bind the columns
for (col_info in columns_to_clean) {
     to_clean_name <- col_info[1]
     new_name <- col_info[2]
     type <- col_info[3]
     
     # Check if the column exists in the dataframe
     if (to_clean_name %in% colnames(cdf)) {
          cleaned_df <- cleanup_function(cdf, to_clean_name, new_name, type)
          cdf <- bind_cols(cdf, cleaned_df)
     } else {
          print(paste("Column", to_clean_name, "does not exist in the dataframe."))
     }
}

# mnrega_r6_wide$micro_irrigation_works_in_lakhs_completed_2011

## To wide
mnrega_r6_wide <- cdf %>%
     group_by(state_key) %>%
     filter(n() == length(unique(year))) %>%  # Keep only ids with all years present
     ungroup() %>%
     pivot_wider(id_cols = c(state_key, state, district, block, panchayat),
                 names_from = year, 
                 values_from = c(anganwadi_other_rural_infrastructure_in_lakhs_completed:total_in_lakhs_approved_not_in_progress, 
                                 works_on_individuals_land_category_iv_in_lakhs_completed:flood_ongoing_expenditure),
                 names_sep = "_")

write_parquet(mnrega_r6_wide, here("data/mnrega/mnrega_r6.parquet"))
