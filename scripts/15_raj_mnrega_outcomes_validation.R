# Load libs
library(arrow)
library(broom)
library(dplyr)
library(here)
library(purrr)
library(readr)
library(stargazer)
library(tidyr)
library(ggplot2)

# Source utils
source(here("scripts/00_utils.R"))

# Big File
mnrega_elex_raj_05_20 <- read_parquet(here("data/raj/mnrega_elex_raj_05_20.parquet"))

# Areas
# "childcare", "infrastructure", 
bases <- c("total", "connectivity", "sanitation", "water_conserve", "water_trad", "drinking_water")
components <- c("comp_project", "comp_expenditure", "ongoing_project", "ongoing_expenditure")
column_groups <- as.vector(outer(bases, components, paste, sep = "_"))


mnrega_elex_raj_05_20 <- mnrega_elex_raj_05_20 %>%
     mutate(
          map_dfc(set_names(column_groups, paste0(column_groups, "_tot_11_23")), 
                  ~ {
                       col_pattern <- paste0(.x, "_20(1[1-9]|2[0-3])$")
                       new_col <- rowSums(select(mnrega_elex_raj_05_20, matches(col_pattern)), na.rm = TRUE)
                       return(new_col)
                  }),
          
          map_dfc(set_names(column_groups, paste0(column_groups, "_tot_11_13")), 
                  ~ {
                       col_pattern_11_13 <- paste0(.x, "_201[1-3]$")
                       new_col_11_13 <- rowSums(select(mnrega_elex_raj_05_20, matches(col_pattern_11_13)), na.rm = TRUE)
                       return(new_col_11_13)
                  }),
          
          map_dfc(set_names(column_groups, paste0(column_groups, "_tot_16_18")), 
                  ~ {
                       col_pattern_16_18 <- paste0(.x, "_201[6-8]$")
                       new_col_16_18 <- rowSums(select(mnrega_elex_raj_05_20, matches(col_pattern_16_18)), na.rm = TRUE)
                       return(new_col_16_18)
                  }),
          
          map_dfc(set_names(column_groups, paste0(column_groups, "_tot_21_23")), 
                  ~ {
                       col_pattern_21_23 <- paste0(.x, "_202[1-3]$")
                       new_col_21_23 <- rowSums(select(mnrega_elex_raj_05_20, matches(col_pattern_21_23)), na.rm = TRUE)
                       return(new_col_21_23)
                  })
     )

## Corr.

# List of time periods
time_periods <- c("11_13", "16_18", "21_23")

# Filter columns for the same area and calculate correlations
correlation_results <- bases %>%
     map_df(~ {
          # Select columns matching the base name across time periods
          columns_to_select <- paste0(.x, "_ongoing_project_tot_", time_periods)
          
          # Filter the relevant columns
          selected_data <- mnrega_elex_raj_05_20 %>%
               select(all_of(columns_to_select)) %>%
               drop_na()  # Remove rows with NAs
          
          # Remove constant columns (zero standard deviation)
          selected_data <- selected_data %>%
               select(where(~ sd(.x, na.rm = TRUE) > 0))
          
          # If fewer than 2 columns remain, skip correlation calculation
          if (ncol(selected_data) < 2) {
               return(data.frame(Var1 = character(), Var2 = character(), Freq = numeric(), base = .x))
          }
          
          # Compute correlation matrix
          cor_matrix <- cor(selected_data, use = "complete.obs")
          
          # Get the upper triangle of the correlation matrix
          cor_upper <- cor_matrix
          cor_upper[lower.tri(cor_upper, diag = TRUE)] <- NA
          
          # Convert the upper triangle into a tidy format
          cor_tidy <- as.data.frame(as.table(cor_upper)) %>%
               filter(!is.na(Freq)) %>%
               mutate(base = .x)
          
          return(cor_tidy)
     })

# Print correlation results
print(correlation_results)




# List of time periods and components
time_periods <- c("11_13", "16_18", "21_23")
components <- c("ongoing_project_tot", "comp_expenditure_tot", "comp_project_tot")

# Filter columns for the same area and calculate correlations
correlation_results <- bases %>%
     map_df(~ {
          # Generate all combinations of components and time periods
          columns_to_select <- as.vector(outer(components, time_periods, paste, sep = "_"))
          columns_to_select <- paste0(.x, "_", columns_to_select) # Prepend the base name
          
          # Filter the relevant columns
          selected_data <- mnrega_elex_raj_05_20 %>%
               select(all_of(columns_to_select)) %>%
               drop_na()  # Remove rows with NAs
          
          # Remove constant columns (zero standard deviation)
          selected_data <- selected_data %>%
               select(where(~ sd(.x, na.rm = TRUE) > 0))
          
          # If fewer than 2 columns remain, skip correlation calculation
          if (ncol(selected_data) < 2) {
               return(data.frame(Var1 = character(), Var2 = character(), Freq = numeric(), base = .x))
          }
          
          # Compute correlation matrix
          cor_matrix <- cor(selected_data, use = "complete.obs")
          
          # Get the upper triangle of the correlation matrix
          cor_upper <- cor_matrix
          cor_upper[lower.tri(cor_upper, diag = TRUE)] <- NA
          
          # Convert the upper triangle into a tidy format
          cor_tidy <- as.data.frame(as.table(cor_upper)) %>%
               filter(!is.na(Freq)) %>%
               mutate(base = .x)
          
          return(cor_tidy)
     })

# Print correlation results
print(correlation_results)

## Social Audit Validation
## Let's take a small random sample of GPs from recent time period for which MNREGA social audit reports are available 
## and manually compare to the data online
set.seed(31415)
sampled_df <- mnrega_elex_raj_05_20[sample(nrow(mnrega_elex_raj_05_20), size = 100), ]
cols_2022 <- colnames(mnrega_elex_raj_05_20)[grepl("2022", colnames(mnrega_elex_raj_05_20))]
write.csv(sampled_df[, c("district_2020", "ps_2020", "gp_2020", cols_2022)], file = "data/mnrega/audit_mnrega_2022.csv")
