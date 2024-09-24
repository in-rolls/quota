# Load libs
library(estimatr)
library(stargazer)
library(dplyr)
library(readr)
library(broom)
library(arrow)
library(ggthemes)
library(here)
library(purrr)

# Source utils
source(here("scripts/00_utils.R"))

# Load dat
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
          map_dfc(set_names(column_groups, paste0(column_groups, "_tot_11_14")), 
                  ~ {
                       col_pattern <- paste0(.x, "_201[1-4]$")
                       new_col <- rowSums(select(mnrega_elex_raj_05_10, matches(col_pattern)), na.rm = TRUE)
                       return(new_col)
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
                  })
     )

# Main
# Model Names
model_names <- paste0("lm_", column_groups)
mod_cols <- paste0(column_groups, "_tot_11_14")

main_models <- set_names(mod_cols, mod_cols) %>% 
     map(~ lm(as.formula(paste(.x, "~ female_res_2005 + female_res_2010")), data = mnrega_elex_raj_05_10))

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

custom_stargazer(n_comp_main_models,
          title = "Effects of Reservations on the Number of Completed MNREGA Projects, 2011-2014",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Traditional Water"),
          add.lines = list(c("Covariates", rep("No", 5))),
          label = "main_mnrega",
          notes = c("All - Total completed or ongoing MNREGA projects (2011--2014);", 
                    "Rural Roads: Number of projects to improve connectivity and roads (2011--2014);",
                    "Sanitation:  Number of projects to improve sanitation facilities  (2011--2014);",
                    "Water Conservation: Number of projects to improve water conservation (2011--2014);",
                    "Trad. Water: Number of projects to maintain traditional water bodies (2011--2014)."),
          out = here("tabs/mnrega_raj_05_10_main.tex"))

## Ongoing Proj
n_ongoing_proj <- paste0(
                       c("total", "connectivity", "childcare", "sanitation", "water_conserve", "water_trad"), 
                       "_ongoing_project_tot_11_14"
                       )

n_ongoing_models <- main_models[names(main_models) %in% n_ongoing_proj]

custom_stargazer(n_ongoing_models,
          title = "Effects of Reservations on the Number of Ongoing MNREGA Projects, 2011-2014",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Traditional Water"),
          add.lines = list(c("Covariates", rep("No", 5))),
          label = "main_mnrega_ongoing",
          notes = c("All - Total ongoing MNREGA projects (2011--2014);", 
                    "Rural Roads: Number of projects to improve connectivity and roads (2011--2014);",
                    "Sanitation:  Number of projects to improve sanitation facilities  (2011--2014);",
                    "Water Conservation: Number of projects to improve water conservation (2011--2014);",
                    "Trad. Water: Number of projects to maintain traditional water bodies (2011--2014)."),
          out = here("tabs/mnrega_raj_05_10_main_ongoing.tex"))

## Interaction
int_models <- set_names(mod_cols, mod_cols)  %>% 
     map(~ lm(as.formula(paste(.x, "~ female_res_2005*female_res_2010")), data = mnrega_elex_raj_05_10))

# Tidy and Glance
map(int_models, tidy)
map(int_models, glance)
proportion_significant_pvalues(int_models)

n_comp_int_models <- int_models[names(int_models) %in% n_comp_proj]

custom_stargazer(n_comp_int_models,
          title = "Effects of Reservations on the Number of Completed MNREGA Projects, 2011-2014. (With Interaction Term).",
          covariate.labels = c("2005", "2010", "2005 * 2010", "Constant"),
          column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Traditional Water"),
          add.lines = list(c("Covariates", rep("No", 5))),
          label = "main_mnrega_interaction",
          notes = c("All - Total completed or ongoing MNREGA projects (2011--2014);", 
                    "Rural Roads: Number of projects to improve connectivity and roads (2011--2014);",
                    "Sanitation:  Number of projects to improve sanitation facilities  (2011--2014);",
                    "Water Conservation: Number of projects to improve water conservation (2011--2014);",
                    "Trad. Water: Number of projects to maintain traditional water bodies (2011--2014)."),
          out = here("tabs/mnrega_raj_05_10_main_interaction.tex"))

## Limit to Phase 1 and 2
raj_p12 <- mnrega_elex_raj_05_10 %>%
     filter(phase_1 == 1 | phase_2 == 1)

p12_models <- set_names(mod_cols, mod_cols) %>% 
     map(~ lm(as.formula(paste(.x, "~ female_res_2005 + female_res_2010")), data = raj_p12))

# Tidy and Glance
map(p12_models, tidy)
map(p12_models, glance)
proportion_significant_pvalues(p12_models)

# Filter to comp.
n_comp_p12_mods <- p12_models[names(p12_models) %in% n_comp_proj]

custom_stargazer(n_comp_p12_mods,
          title = "Effects of Reservations on the Number of Completed MNREGA Projects, 2011-2014. (Phase 1 and 2 Only.)",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Traditional Water"),
          add.lines = list(c("Covariates", rep("No", 5))),
          label = "main_mnrega_phase_1_2",
          notes = c("All - Total completed or ongoing MNREGA projects (2011--2014);", 
                    "Rural Roads: Number of projects to improve connectivity and roads (2011--2014);",
                    "Sanitation:  Number of projects to improve sanitation facilities  (2011--2014);",
                    "Water Conservation: Number of projects to improve water conservation (2011--2014);",
                    "Trad. Water: Number of projects to maintain traditional water bodies (2011--2014)."),
          out = here("tabs/mnrega_raj_05_10_main_phase_1_2.tex"))

### Limit to ST
raj_st = mnrega_elex_raj_05_10 %>%
     filter(reservation_2005 %in% c("ST", "STW") | reservation_2010 %in% c("ST", "STW"))

# Apply the model fitting function across all specified column groups
st_models <- set_names(mod_cols, mod_cols) %>% 
     map(~ lm(as.formula(paste(.x, "~ female_res_2005 + female_res_2010")), data = raj_st))

# Tidy and Glance
map(st_models, tidy)
map(st_models, glance)
n_comp_st_models <- st_models[names(st_models) %in% n_comp_proj]

custom_stargazer(n_comp_st_models,
          title = "Effects of Reservations on the Number of Completed MNREGA Projects, 2011-2014. (ST Only.)",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Traditional Water"),
          add.lines = list(c("Covariates", rep("No", 5))),
          label = "main_mnrega_phase_st",
          notes = c("All - Total completed or ongoing MNREGA projects (2011--2014);", 
                    "Rural Roads: Number of projects to improve connectivity and roads (2011--2014);",
                    "Sanitation:  Number of projects to improve sanitation facilities  (2011--2014);",
                    "Water Conservation: Number of projects to improve water conservation (2011--2014);",
                    "Trad. Water: Number of projects to maintain traditional water bodies (2011--2014)."),
          out = here("tabs/mnrega_raj_05_10_main_st.tex"))

## Limit to Strict
strict_models <- set_names(mod_cols, mod_cols) %>% 
     map(~ lm(as.formula(paste(.x, "~ female_res_2005 + female_res_2010")), data = mnrega_elex_raj_05_10_strict))

# Tidy and Glance
map(strict_models, tidy)
map(strict_models, glance)
n_comp_strict_models <- strict_models[names(strict_models) %in% n_comp_proj]

custom_stargazer(n_comp_strict_models,
          title = "Effects of Reservations on the Number of Completed MNREGA Projects, 2011-2014. (Strict Matching.)",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Traditional Water"),
          add.lines = list(c("Covariates", rep("No", 5))),
          label = "main_mnrega_strict",
          notes = c("All - Total completed or ongoing MNREGA projects (2011--2014);", 
                    "Rural Roads: Number of projects to improve connectivity and roads (2011--2014);",
                    "Sanitation:  Number of projects to improve sanitation facilities  (2011--2014);",
                    "Water Conservation: Number of projects to improve water conservation (2011--2014);",
                    "Trad. Water: Number of projects to maintain traditional water bodies (2011--2014)."),
          out = here("tabs/mnrega_raj_05_10_main_strict.tex"))

### Extreme
raj_extreme <- mnrega_elex_raj_05_20 %>%
     filter(case == '1_1_1_1' | case == '0_0_0_0')

# Model Names
model_names <- paste0("lm_", column_groups)
mod_cols <- paste0(column_groups, "_tot_11_23")

# Apply the model fitting function across all specified column groups
extreme_models <- set_names(mod_cols, mod_cols) %>% 
     map(~ lm(as.formula(paste(.x, "~ case")), data = raj_extreme))

# Tidy and Glance
map(extreme_models, tidy)
map(extreme_models, glance)

# Number of projects: All Rural Roads Sanitation Water Conservation Traditional Water
n_comp_proj_11_23 <- paste0(
                    c("total", "connectivity", "childcare", "sanitation", "water_conserve", "water_trad"), 
                    "_comp_project_tot_11_23"
                    )

n_comp_extreme_models <- extreme_models[names(extreme_models) %in% n_comp_proj_11_23]

custom_stargazer(n_comp_extreme_models,
          title = "Effects of Reservations on the Number of Completed MNREGA Projects, 2011-2023.",
          covariate.labels = c("T-T-T-T", "Constant"),
          column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Traditional Water"),
          add.lines = list(c("Covariates", rep("No", 5))),
          label = "main_mnrega_2011_2023",
          notes = c("All - Total completed or ongoing MNREGA projects (2011--2023);", 
                    "Rural Roads: Number of projects to improve connectivity and roads (2011--2023);",
                    "Sanitation:  Number of projects to improve sanitation facilities  (2011--2023);",
                    "Water Conservation: Number of projects to improve water conservation (2011--2023);",
                    "Trad. Water: Number of projects to maintain traditional water bodies (2011--2023)."),
          out = here("tabs/mnrega_raj_05_20_main_extreme.tex"))

### All
model_names <- paste0("lm_", column_groups)
mod_cols <- paste0(column_groups, "_tot_11_23")

all_models <- set_names(mod_cols, mod_cols)  %>% 
     map(~ lm(as.formula(paste(.x, "~ female_res_2005*female_res_2010*female_res_2015*female_res_2020")), data = mnrega_elex_raj_05_20))

# Tidy and Glance
map(all_models, tidy)
map(all_models, glance)

n_comp_all_models <- all_models[names(all_models) %in% n_comp_proj_11_23]

custom_stargazer(n_comp_all_models,
          title = "Effects of Reservations on the Number of Completed MNREGA Projects, 2011-2023.",
          covariate.labels = c("2005", "2010", "2015", "2020", 
                               "2005*2010", "2005*2015", "2010*2015", 
                               "2005*2020", "2010*2020", "2015*2020", "2005*2010*2015",
                               "2005*2010*2020", "2005*2015*2020", "2010*2015*2020", "2005*2010*2015*2020",
                               "Constant"),
          column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Traditional Water"),
          add.lines = list(c("Covariates", rep("No", 5))),
          label = "main_mnrega_2011_2023_all",
          notes = c("All - Total completed or ongoing MNREGA projects (2011--2023);", 
                    "Rural Roads: Number of projects to improve connectivity and roads (2011--2023);",
                    "Sanitation:  Number of projects to improve sanitation facilities  (2011--2023);",
                    "Water Conservation: Number of projects to improve water conservation (2011--2023);",
                    "Trad. Water: Number of projects to maintain traditional water bodies (2011--2023)."),
          out = here("tabs/mnrega_raj_05_20_main_all.tex"))

### Dosage
mnrega_elex_raj_05_20 <- mnrega_elex_raj_05_20 %>%
     mutate(female_res_sum = rowSums(select(., female_res_2005, female_res_2010, female_res_2015, female_res_2020)),
            female_res_sum_factor = factor(female_res_sum, levels = 0:max(female_res_sum)))

# Model Names
model_names <- paste0("lm_", column_groups)
mod_cols <- paste0(column_groups, "_tot_11_23")

dosage_models <- set_names(mod_cols, mod_cols)  %>% 
     map(~ lm(as.formula(paste(.x, "~ female_res_sum_factor")), data = mnrega_elex_raj_05_20))

# Tidy and Glance
map(dosage_models, tidy)
map(dosage_models, glance)

n_comp_dosage_models <- dosage_models[names(dosage_models) %in% n_comp_proj_11_23]

custom_stargazer(n_comp_dosage_models,
          title = "Effects of Reservations on the Number of Completed MNREGA Projects, 2011-2023.",
          covariate.labels = c("Reserved Once", "Reserved Twice", "Reserved Thrice", "Reserved Four Times", "Constant"),
          column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Traditional Water"),
          add.lines = list(c("Covariates", rep("No", 5))),
          label = "main_mnrega_2011_2023_dosage",
          notes = c("All - Total completed or ongoing MNREGA projects (2011--2023);", 
                    "Rural Roads: Number of projects to improve connectivity and roads (2011--2023);",
                    "Sanitation:  Number of projects to improve sanitation facilities  (2011--2023);",
                    "Water Conservation: Number of projects to improve water conservation (2011--2023);",
                    "Trad. Water: Number of projects to maintain traditional water bodies (2011--2023)."),
          out = here("tabs/mnrega_raj_05_20_main_dosage.tex"))
