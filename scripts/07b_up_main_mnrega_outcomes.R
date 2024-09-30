# Load libs
library(here)
library(purrr)
library(arrow)
library(readr)
library(estimatr)
library(stargazer)
library(dplyr)
library(broom)
library(ggthemes)

# Source utils
source(here("scripts/00_utils.R"))

# Load dat
mnrega_elex_up_05_10 <- read_parquet(here("data/up/mnrega_elex_up_05_10.parquet"))

mnrega_elex_up_05_10 <- mnrega_elex_up_05_10 %>% 
     mutate(female_res_2005 = grepl("Female", gp_res_status_fin_eng_2005),
            female_res_2010 = grepl("Female", gp_res_status_fin_eng_2010)
     )

# Outcomes
bases <- c("total", "connectivity", "sanitation", "water_conserve", "water_trad", "drinking_water")
components <- c("comp_project", "comp_expenditure", "ongoing_project", "ongoing_expenditure")
column_groups <- as.vector(outer(bases, components, paste, sep = "_"))
column_groups_append <- c(column_groups, c("number_of_jobcards_issued", 
                                           "registered_workers_total_workers",
                                           "registered_workers_women"))

mnrega_elex_up_05_10 <- mnrega_elex_up_05_10 %>%
     mutate(
          map_dfc(set_names(column_groups_append, paste0(column_groups_append, "_tot_11_14")), 
                  ~ {
                       col_pattern <- paste0(.x, "_201[1-4]$")
                       new_col <- rowSums(select(mnrega_elex_up_05_10, matches(col_pattern)), na.rm = TRUE)
                       return(new_col)
                  })
     )

# Model Names
model_names <- paste0("lm_", column_groups_append)
mod_cols <- paste0(column_groups_append, "_tot_11_14")

# Apply the model fitting function across all specified column groups
models <- set_names(mod_cols, mod_cols) %>% 
     map(~ lm(as.formula(paste(.x, "~ female_res_2005 + female_res_2010")), data = mnrega_elex_up_05_10))

# Tidy and Glance
map(models, tidy)
map(models, glance)

# Number of projects
# All Rural Roads Sanitation Water Conservation Traditional Water
selected_model_names <- paste0(c("number_of_jobcards_issued", 
                                 "registered_workers_total_workers",
                                 "registered_workers_women"), "_tot_11_14")

# Filter clean_models to only include the specified models
selected_models <- models[names(models) %in% selected_model_names]

# Get the logged DV
models <- set_names(mod_cols, mod_cols) %>% 
     map(~ {
          # Fit the linear model with the log-transformed dependent variable
          lm(as.formula(paste("log(", .x, "+ 1) ~ female_res_2005 + female_res_2010")), 
             data = mnrega_elex_up_05_10)
     })

# Tidy and Glance/Editor view
model_tidies  <- map(models, tidy)
model_glances <- map(models, glance)

# B/D Outcomes
selected_log_model_names <- paste0(c("number_of_jobcards_issued", 
                                     "registered_workers_total_workers",
                                     "registered_workers_women"), "_tot_11_14")

selected_log_models <- models[names(models) %in% selected_log_model_names]

custom_stargazer(selected_models, selected_log_models,
          title = "Effects of Reservations on Demand for Work and Women Employment via MNREGA, 2011-2014",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = c("Tot. Jobcards", "Reg. Workers", "Reg. Women",
                            "Log Tot. Jobcards", "Log Reg. Workers", "Log Reg. Women"),
          add.lines = list(c("Covariates", rep("No", 6))),
          label = "mnrega_main_up",
          notes = c("Total Jobcards (2011--2014);", 
                    "Registered Workers (2011--2014);",
                    "Registered Workers Women  (2011--2014);"),
          out = "tabs/mnrega_up_05_10_main_bose.tex")

# Limit to BD Phase 1 districts
p1_bd <- mnrega_elex_up_05_10 %>%
          filter(phase_1_bose_2005 == 1)
p2_bd <- mnrega_elex_up_05_10 %>%
     filter(phase_2_bose_2005 == 1)

p1_models <- set_names(mod_cols, mod_cols) %>% 
     map(~ {
          # Fit the linear model with the log-transformed dependent variable
          lm(as.formula(paste("log(", .x, "+ 1) ~ female_res_2005 + female_res_2010")), 
             data = p1_bd)
     })

map(p1_models, tidy)
map(p1_models, glance)

selected_p1_models <- p1_models[names(p1_models) %in% selected_log_model_names]

p2_models <-  set_names(mod_cols, mod_cols) %>% 
     map(~ {
          # Fit the linear model with the log-transformed dependent variable
          lm(as.formula(paste("log(", .x, "+ 1) ~ female_res_2005 + female_res_2010")), 
             data = p2_bd)
     })

selected_p2_models <- p2_models[names(p2_models) %in% selected_log_model_names]

custom_stargazer(selected_p1_models, selected_p2_models,
          title = "Effects of Reservations on Demand for Work and Women Employment via MNREGA, 2011-2014 (Phase 1 and 2, Bose and Das Sample)",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = c("Ph. 1 Jobcards", "Ph. 1 Workers", "Ph. 1 Women",
                            "Ph. 2 Jobcards", "Ph. 2 Workers", "Ph. 2 Women"),
          add.lines = list(c("Covariates", rep("No", 6))),
          label = "mnrega_main_up_bd",
          notes = c("Total Jobcards (2011--2014);", 
                    "Registered Workers (2011--2014);",
                    "Registered Workers Women  (2011--2014);"),
          out = "tabs/mnrega_up_05_10_main_bose_phase_1_2_bd.tex")
