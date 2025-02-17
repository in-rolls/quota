# Load libs
library(arrow)
library(broom)
library(dplyr)
library(here)
library(purrr)
library(readr)
library(stargazer)

# Source utils
source(here("scripts/00_utils.R"))

# Year wise (2005, 2010, 2015, 2020) --- plausible future
mnrega_elex_raj_10_strict <- read_parquet(here("data/raj/mnrega_elex_raj_10_strict.parquet"))
mnrega_elex_raj_15_strict <- read_parquet(here("data/raj/mnrega_elex_raj_15_strict.parquet"))
mnrega_elex_raj_20_strict <- read_parquet(here("data/raj/mnrega_elex_raj_20_strict.parquet"))

# Two Big Files
mnrega_elex_raj_05_10 <- read_parquet(here("data/raj/mnrega_elex_raj_05_10.parquet"))
mnrega_elex_raj_05_20 <- read_parquet(here("data/raj/mnrega_elex_raj_05_20.parquet"))

mnrega_elex_raj_05_10_strict <- read_parquet(here("data/raj/mnrega_elex_raj_05_10_strict.parquet"))
mnrega_elex_raj_05_20_strict <- read_parquet(here("data/raj/mnrega_elex_raj_05_20_strict.parquet"))

# Areas
# "childcare", "infrastructure", 
bases <- c("total", "connectivity", "sanitation", "water_conserve", "water_trad", "drinking_water")
components <- c("comp_project", "comp_expenditure", "ongoing_project", "ongoing_expenditure")
column_groups <- as.vector(outer(bases, components, paste, sep = "_"))

mnrega_elex_raj_05_10 <- mnrega_elex_raj_05_10 %>%
     mutate(
          # Create columns for the 2011 to 2014 range
          map_dfc(set_names(column_groups, paste0(column_groups, "_tot_11_14")), 
                  ~ {
                       col_pattern_11_14 <- paste0(.x, "_201[1-4]$")
                       new_col_11_14 <- rowSums(select(mnrega_elex_raj_05_10, matches(col_pattern_11_14)), na.rm = TRUE)
                       return(new_col_11_14)
                  }),
          
          map_dfc(set_names(column_groups, paste0(column_groups, "_tot_11_13")), 
                  ~ {
                       col_pattern_11_13 <- paste0(.x, "_201[1-3]$")
                       new_col_11_13 <- rowSums(select(mnrega_elex_raj_05_10, matches(col_pattern_11_13)), na.rm = TRUE)
                       return(new_col_11_13)
                  })
     )

mnrega_elex_raj_05_10_strict <- mnrega_elex_raj_05_10_strict %>%
     mutate(
          map_dfc(set_names(column_groups, paste0(column_groups, "_tot_11_14")), 
                  ~ {
                       col_pattern <- paste0(.x, "_201[1-4]$")
                       new_col <- rowSums(select(mnrega_elex_raj_05_10_strict, matches(col_pattern)), na.rm = TRUE)
                       return(new_col)
                  })
     )

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

# Year wise
mnrega_elex_raj_10_strict <- mnrega_elex_raj_10_strict  %>%
     mutate(
          map_dfc(set_names(column_groups, paste0(column_groups, "_tot_11_13")), 
                  ~ {
                       col_pattern_11_13 <- paste0(.x, "_201[1-3]$")
                       new_col_11_13 <- rowSums(select(mnrega_elex_raj_10_strict, matches(col_pattern_11_13)), na.rm = TRUE)
                       return(new_col_11_13)
                  })
     )

mnrega_elex_raj_15_strict <- mnrega_elex_raj_15_strict  %>%
     mutate(
          map_dfc(set_names(column_groups, paste0(column_groups, "_tot_16_18")), 
                  ~ {
                       col_pattern_16_18 <- paste0(.x, "_201[6-8]$")
                       new_col_16_18 <- rowSums(select(mnrega_elex_raj_15_strict, matches(col_pattern_16_18)), na.rm = TRUE)
                       return(new_col_16_18)
                  })
     )

mnrega_elex_raj_20_strict <- mnrega_elex_raj_20_strict  %>%
     mutate(
          map_dfc(set_names(column_groups, paste0(column_groups, "_tot_21_23")), 
                  ~ {
                       col_pattern_21_23 <- paste0(.x, "_202[1-3]$")
                       new_col_21_23 <- rowSums(select(mnrega_elex_raj_20_strict, matches(col_pattern_21_23)), na.rm = TRUE)
                       return(new_col_21_23)
                  })
     )

# Main
main_outcome_caption <- "They are: 
(i) All: The total number of completed projects, including areas not listed here like Fisheries, Drought Proofing, etc.;
(ii) Rural Roads: The number of projects to improve connectivity and roads;
(iii) Sanitation: The number of projects to improve sanitation facilities;
(iv) Water Conservation: The number of projects to improve water conservation;
(v) Trad. Water: The number of projects to maintain traditional water bodies."

# Model Names
model_names <- paste0("lm_", column_groups)
mod_cols <- paste0(column_groups, "_tot_11_14")

# Just a dist/sam fe model to see how much variance it explains
#dist_sam_models <- set_names(mod_cols, mod_cols) %>% 
#     map(~ lm(as.formula(paste(.x, "~ dist_sam")), data = mnrega_elex_raj_05_10))

# Actual 
main_models <- set_names(mod_cols, mod_cols) %>% 
     map(~ lm(as.formula(paste(.x, "~ caste_res_2005 + caste_res_2010")), data = mnrega_elex_raj_05_10))

# Print
map(main_models, ~ tidy(.x) %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
map(main_models, glance)
proportion_significant_pvalues(main_models)

# Number of completed projects
n_comp_proj <- paste0(
     c("total", "connectivity", "childcare", "sanitation", "water_conserve", "water_trad"), 
     "_comp_project_tot_11_14"
)

n_comp_main_models <- main_models[names(main_models) %in% n_comp_proj]

label_mapping <- c(
     "caste_res_2005OBC" = "2005 OBC",
     "caste_res_2005SC" = "2005 SC",
     "caste_res_2005ST" = "2005 ST",
     "caste_res_2010OBC" = "2010 OBC",
     "caste_res_2010SC" = "2010 SC",
     "caste_res_2010ST" = "2010 ST"
)

coef_names <- names(coef(main_models[[1]]))
covariate_names <- coef_names[-1]
cov_labels <- label_mapping[covariate_names]

custom_stargazer(n_comp_main_models,
                 title = "Effects of Caste Reservations on the Number of Completed MNREGA Projects, 2011--2014",
                 covariate.labels = cov_labels,
                 column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Trad. Water"),
                 add.lines = list(c("Covariates", rep("No", 5))),
                 label = "main_caste_mnrega",
                 notes = paste(cons_term, 
                               "The outcomes are from MNREGA administrative data for years 2011--2014.", 
                               main_outcome_caption),
                 out = here("tabs/mnrega_caste_raj_05_10_main.tex"))