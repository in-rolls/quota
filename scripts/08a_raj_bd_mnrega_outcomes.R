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

# Load dat
mnrega_elex_up_05_10 <- read_parquet(here("data/up/mnrega_elex_up_05_10.parquet"))

mnrega_elex_up_05_10 <- mnrega_elex_up_05_10 %>% 
     mutate(female_res_2005 = grepl("Female", gp_res_status_fin_eng_2005),
            female_res_2010 = grepl("Female", gp_res_status_fin_eng_2010)
     )

# ## Line 32 - there cannot be any missing values - where do these come from? Investigate?
# Areas
# "childcare", "infrastructure", 
bases <- c("total", "connectivity", "sanitation", "water_conserve", "water_trad", "drinking_water")
components <- c("comp_project", "comp_expenditure", "ongoing_project", "ongoing_expenditure")
column_groups <- as.vector(outer(bases, components, paste, sep = "_"))

mnrega_elex_up_05_10 <- mnrega_elex_up_05_10 %>%
     mutate(
          map_dfc(set_names(column_groups, paste0(column_groups, "_tot_11_14")), 
                  ~ {
                       col_pattern <- paste0(.x, "_201[1-4]$")
                       new_col <- rowSums(select(mnrega_elex_up_05_10, matches(col_pattern)), na.rm = TRUE)
                       return(new_col)
                  })
     )

# Model Names
model_names <- paste0("lm_", column_groups)
mod_cols <- paste0(column_groups, "_tot_11_14")

models <- set_names(mod_cols, mod_cols) %>% 
     map(~ lm(as.formula(paste(.x, "~ female_res_2005 + female_res_2010")), data = mnrega_elex_up_05_10))

# Tidy and Glance
model_tidies <- map(models, tidy)
model_glances <- map(models, glance)

# Number of projects
# All Rural Roads Sanitation Water Conservation Traditional Water
selected_model_names <- paste0(c("total_comp_project",
                                 "connectivity_comp_project",
                                 "childcare_comp_project",
                                 "sanitation_comp_project",
                                 "water_conserve_comp_project",
                                 "water_trad_comp_project"), "_tot_11_14")

# Filter clean_models to only include the specified models
selected_models <- models[names(models) %in% selected_model_names]

custom_stargazer(selected_models,
          title = "Effects of Reservations on the Number of Completed MNREGA Projects, 2011-2014",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Traditional Water"),
          add.lines = list(c("Covariates", rep("No", 5))),
          notes = c("All - Total completed or ongoing MNREGA projects (2011--2014);", 
                    "Rural Roads: Number of projects to improve connectivity and roads (2011--2014);",
                    "Sanitation:  Number of projects to improve sanitation facilities  (2011--2014);",
                    "Water Conservation: Number of projects to improve water conservation (2011--2014);",
                    "Trad. Water: Number of projects to maintain traditional water bodies (2011--2014)."),
          out = here("tabs/mnrega_up_05_10_main.tex"))

# Bose and Das Districts
raj_bd = mnrega_elex_up_05_10 %>%
     filter(phase_1_bose_2005 == 1 | phase_2_bose_2005 == 1)

# Apply the model fitting function across all specified column groups
models <- set_names(mod_cols, mod_cols) %>% 
     map(~ lm(as.formula(paste(.x, "~ female_res_2005 + female_res_2010")), data = raj_bd))

# Tidy and Glance
model_tidies <- map(models, tidy)
model_glances <- map(models, glance)

# Number of projects
# All Rural Roads Sanitation Water Conservation Traditional Water
selected_model_names <- paste0(c("total_comp_project",
                                 "connectivity_comp_project",
                                 "childcare_comp_project",
                                 "sanitation_comp_project",
                                 "water_conserve_comp_project",
                                 "water_trad_comp_project"), "_tot_11_14")

# Filter clean_models to only include the specified models
selected_models <- models[names(models) %in% selected_model_names]

custom_stargazer(selected_models,
          title = "Effects of Reservations on the Number of Completed MNREGA Projects, 2011-2014 (Bose and Das Districts)",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Traditional Water"),
          add.lines = list(c("Covariates", rep("No", 5))),
          notes = c("All - Total completed or ongoing MNREGA projects (2011--2014);", 
                    "Rural Roads: Number of projects to improve connectivity and roads (2011--2014);",
                    "Sanitation:  Number of projects to improve sanitation facilities  (2011--2014);",
                    "Water Conservation: Number of projects to improve water conservation (2011--2014);",
                    "Trad. Water: Number of projects to maintain traditional water bodies (2011--2014)."),
          out = here("tabs/mnrega_up_05_10_main_bd_districts.tex"))
