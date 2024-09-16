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

# Load dat
mnrega_elex_up_05_10 <- read_parquet(here("data/up/mnrega_elex_up_05_10.parquet"))

mnrega_elex_up_05_10 <- mnrega_elex_up_05_10 %>% 
     mutate(female_res_2005 = grepl("Female", gp_res_status_fin_eng_2005),
            female_res_2010 = grepl("Female", gp_res_status_fin_eng_2010)
     )

# Log of job cards issued 
# Log of NREGA demand
# Log of person days
# ## Line 32 - there cannot be any missing values - where do these come from? Investigate?
# Areas
# "childcare", "infrastructure", 
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

# Function to fit 
fit_model_for_group <- function(column_name, data) {
     sufficient_data <- complete.cases(data[[column_name]], data$female_res_2005, data$female_res_2010)
     if (sum(sufficient_data) > 0) {
          lm(as.formula(paste(column_name, "~ female_res_2005 + female_res_2010")), data = data)
     } else {
          NULL  # Indicate failure to fit the model due to insufficient data
     }
}

# Apply the model fitting function across all specified column groups
models <- set_names(mod_cols, mod_cols) %>% 
     map(~ fit_model_for_group(.x, mnrega_elex_up_05_10))

# Tidy and Glance
model_tidies <- map(models, tidy)
model_glances <- map(models, glance)

# Number of projects
# All Rural Roads Sanitation Water Conservation Traditional Water
selected_model_names <- paste0(c("number_of_jobcards_issued", 
                                 "registered_workers_total_workers",
                                 "registered_workers_women"), "_tot_11_14")

# Filter clean_models to only include the specified models
selected_models <- models[names(models) %in% selected_model_names]

# Get the logged DV
fit_model_for_group <- function(column_name, data) {
     # Ensure that the data has complete cases for the necessary variables
     sufficient_data <- complete.cases(data[[column_name]], data$female_res_2005, data$female_res_2010)
     
     if (sum(sufficient_data) > 0) {
          # Log-transform the dependent variable, adding 1 to avoid log(0)
          data[[column_name]] <- log(data[[column_name]] + 1)
          
          # Fit the linear model with the log-transformed dependent variable
          lm(as.formula(paste(column_name, "~ female_res_2005 + female_res_2010")), data = data)
     } else {
          NULL  # Return NULL if there's insufficient data to fit the model
     }
}

models <- set_names(mod_cols, mod_cols) %>% 
     map(~ fit_model_for_group(.x, mnrega_elex_up_05_10))

# Tidy and Glance/Editor view
model_tidies  <- map(models, tidy)
model_glances <- map(models, glance)

# Number of projects
# All Rural Roads Sanitation Water Conservation Traditional Water
selected_log_model_names <- paste0(c("number_of_jobcards_issued", 
                                     "registered_workers_total_workers",
                                     "registered_workers_women"), "_tot_11_14")

# Filter clean_models to only include the specified models
selected_log_models <- models[names(models) %in% selected_log_model_names]

stargazer(selected_models, selected_log_models,
          type = "latex", 
          title = "Effects of Reservations on Demand for Work and Women Employment via MNREGA, 2011-2014",
          model.names = FALSE,
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = c("Tot. Jobcards", "Reg. Workers", "Reg. Women",
                            "Log Tot. Jobcards", "Log Reg. Workers", "Log Reg. Women"),
          add.lines = list(c("Covariates", rep("No", 5))),
          omit.stat = c("rsq", "ser", "f"),
          single.row = FALSE,
          digits = 2,
          dep.var.labels.include = FALSE,
          star.cutoffs = NULL,
          dep.var.caption = "",
          label = "mnrega_main_up",
          font.size = "scriptsize",
          report = "vcs",
          notes = c("Total Jobcards (2011--2014);", 
                    "Registered Workers (2011--2014);",
                    "Registered Workers Women  (2011--2014);"),
          notes.align = "l",
          out = "tabs/mnrega_up_05_10_main_bose.tex")

# Limit to BD Phase 1 districts
models <- set_names(mod_cols, mod_cols) %>% 
     map(~ fit_model_for_group(.x, mnrega_elex_up_05_10[mnrega_elex_up_05_10$phase_1_bose_2005, ]))

model_tidies  <- map(models, tidy)
model_glances <- map(models, glance)

selected_phase_1_models <- models[names(models) %in% selected_log_model_names]

models <- set_names(mod_cols, mod_cols) %>% 
     map(~ fit_model_for_group(.x, mnrega_elex_up_05_10[mnrega_elex_up_05_10$phase_2_bose_2005, ]))

selected_phase_2_models <- models[names(models) %in% selected_log_model_names]

stargazer(selected_phase_1_models, selected_phase_2_models,
          type = "latex", 
          title = "Effects of Reservations on Demand for Work and Women Employment via MNREGA, 2011-2014 (Phase 1 and 2, Bose and Das Sample)",
          model.names = FALSE,
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = c("Ph. 1 Jobcards", "Ph. 1 Workers", "Ph. 1 Women",
                            "Ph. 2 Jobcards", "Ph. 2 Workers", "Ph. 2 Women"),
          add.lines = list(c("Covariates", rep("No", 5))),
          omit.stat = c("rsq", "ser", "f"),
          single.row = FALSE,
          digits = 2,
          dep.var.labels.include = FALSE,
          star.cutoffs = NULL,
          dep.var.caption = "",
          label = "mnrega_main_up",
          font.size = "scriptsize",
          report = "vcs",
          notes = c("Total Jobcards (2011--2014);", 
                    "Registered Workers (2011--2014);",
                    "Registered Workers Women  (2011--2014);"),
          notes.align = "l",
          out = "tabs/mnrega_up_05_10_main_bose_phase_1_2_bd.tex")
