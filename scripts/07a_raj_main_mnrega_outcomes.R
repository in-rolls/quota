# Load libs
library(estimatr)
library(stargazer)
library(dplyr)
library(readr)
library(broom)
library(ggthemes)
library(here)

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

# Model Names
model_names <- paste0("lm_", column_groups)
mod_cols <- paste0(column_groups, "_tot_11_14")

fit_model_for_group <- function(column_name, data) {
     sufficient_data <- complete.cases(data[[column_name]], data$female_res_2005, data$female_res_2010)
     if (sum(sufficient_data) > 0) {
          lm(as.formula(paste(column_name, "~ female_res_2005 + female_res_2010")), data = data)
     } else {
          NULL  # If a model fails ...
     }
}

# Apply the model
models <- set_names(mod_cols, mod_cols) %>% 
     map(~ fit_model_for_group(.x, mnrega_elex_raj_05_10))

# Tidy and Glance
model_tidies  <- map(models, tidy)
model_glances <- map(models, glance)

# Number of projects: All Rural Roads Sanitation Water Conservation Traditional Water
selected_model_names <- paste0(c("total_comp_project",
                                 "connectivity_comp_project",
                                 "childcare_comp_project",
                                 "sanitation_comp_project",
                                 "water_conserve_comp_project",
                                 "water_trad_comp_project"), "_tot_11_14")

# Filter clean_models to only include the specified models
selected_models <- models[names(models) %in% selected_model_names]

stargazer(selected_models,
          type = "latex", 
          title = "Effects of Reservations on the Number of Completed MNREGA Projects, 2011-2014",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Traditional Water"),
          add.lines = list(c("Covariates", rep("No", 5))),
          omit.stat = c("rsq", "ser", "f"),
          single.row = FALSE,
          digits = 2,
          label = "main_mnrega",
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          no.space = TRUE,
          star.cutoffs = NULL,
          report = "vcs",
          notes = c("All - Total completed or ongoing MNREGA projects (2011--2014);", 
                    "Rural Roads: Number of projects to improve connectivity and roads (2011--2014);",
                    "Sanitation:  Number of projects to improve sanitation facilities  (2011--2014);",
                    "Water Conservation: Number of projects to improve water conservation (2011--2014);",
                    "Trad. Water: Number of projects to maintain traditional water bodies (2011--2014)."),
          notes.align = "l",
          out = here("tabs/mnrega_raj_05_10_main.tex"))

## Ongoing
# Number of projects: All Rural Roads Sanitation Water Conservation Traditional Water
ongoing_proj <- paste0(c("total_ongoing_project",
                                 "connectivity_ongoing_project",
                                 "childcare_ongoing_project",
                                 "sanitation_ongoing_project",
                                 "water_conserve_ongoing_project",
                                 "water_trad_ongoing_project"), "_tot_11_14")

# Filter clean_models to only include the specified models
selected_models <- models[names(models) %in% ongoing_proj]

stargazer(selected_models,
          type = "latex", 
          title = "Effects of Reservations on the Number of Ongoing MNREGA Projects, 2011-2014",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Traditional Water"),
          add.lines = list(c("Covariates", rep("No", 5))),
          omit.stat = c("rsq", "ser", "f"),
          single.row = FALSE,
          label = "main_mnrega_ongoing",
          digits = 2,
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          no.space = TRUE,
          star.cutoffs = NULL,
          report = "vcs",
          notes = c("All - Total ongoing MNREGA projects (2011--2014);", 
                    "Rural Roads: Number of projects to improve connectivity and roads (2011--2014);",
                    "Sanitation:  Number of projects to improve sanitation facilities  (2011--2014);",
                    "Water Conservation: Number of projects to improve water conservation (2011--2014);",
                    "Trad. Water: Number of projects to maintain traditional water bodies (2011--2014)."),
          notes.align = "l",
          out = here("tabs/mnrega_raj_05_10_main_ongoing.tex"))

## Interaction

# Function to fit 
fit_model_for_group <- function(column_name, data) {
     sufficient_data <- complete.cases(data[[column_name]], data$female_res_2005, data$female_res_2010)
     if (sum(sufficient_data) > 0) {
          lm(as.formula(paste(column_name, "~ female_res_2005*female_res_2010")), data = data)
     } else {
          NULL  # Indicate failure to fit the model due to insufficient data
     }
}

# Apply the model fitting function across all specified column groups
models <- set_names(mod_cols, mod_cols) %>% 
     map(~ fit_model_for_group(.x, mnrega_elex_raj_05_10))

# Tidy and Glance
model_tidies <- map(models, tidy)
model_glances <- map(models, glance)

# Filter clean_models to only include the specified models
selected_models <- models[names(models) %in% selected_model_names]

stargazer(selected_models,
          type = "latex", 
          title = "Effects of Reservations on the Number of Completed MNREGA Projects, 2011-2014. (With Interaction Term).",
          covariate.labels = c("2005", "2010", "2005 * 2010", "Constant"),
          column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Traditional Water"),
          add.lines = list(c("Covariates", rep("No", 5))),
          omit.stat = c("rsq", "ser", "f"),
          single.row = FALSE,
          digits = 2,
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          label = "main_mnrega_interaction",
          no.space = TRUE,
          star.cutoffs = NULL,
          report = "vcs",
          notes = c("All - Total completed or ongoing MNREGA projects (2011--2014);", 
                    "Rural Roads: Number of projects to improve connectivity and roads (2011--2014);",
                    "Sanitation:  Number of projects to improve sanitation facilities  (2011--2014);",
                    "Water Conservation: Number of projects to improve water conservation (2011--2014);",
                    "Trad. Water: Number of projects to maintain traditional water bodies (2011--2014)."),
          notes.align = "l",
          out = here("tabs/mnrega_raj_05_10_main_interaction.tex"))

## Limit to Phase 1 and 2

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
     map(~ fit_model_for_group(.x, mnrega_elex_raj_05_10[mnrega_elex_raj_05_10$phase_1 == 1 | mnrega_elex_raj_05_10$phase_2 == 1, ]))

# Tidy and Glance
model_tidies <- map(models, tidy)
model_glances <- map(models, glance)

# Filter clean_models to only include the specified models
selected_models <- models[names(models) %in% selected_model_names]

stargazer(selected_models,
          type = "latex", 
          title = "Effects of Reservations on the Number of Completed MNREGA Projects, 2011-2014. (Phase 1 and 2 Only.)",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Traditional Water"),
          add.lines = list(c("Covariates", rep("No", 5))),
          omit.stat = c("rsq", "ser", "f"),
          single.row = FALSE,
          digits = 2,
          header = TRUE,
          label = "main_mnrega_phase_1_2",
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          no.space = TRUE,
          star.cutoffs = NULL,
          report = "vcs",
          notes = c("All - Total completed or ongoing MNREGA projects (2011--2014);", 
                    "Rural Roads: Number of projects to improve connectivity and roads (2011--2014);",
                    "Sanitation:  Number of projects to improve sanitation facilities  (2011--2014);",
                    "Water Conservation: Number of projects to improve water conservation (2011--2014);",
                    "Trad. Water: Number of projects to maintain traditional water bodies (2011--2014)."),
          notes.align = "l",
          out = here("tabs/mnrega_raj_05_10_main_phase_1_2.tex"))

### Limit to ST

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
     map(~ fit_model_for_group(.x, mnrega_elex_raj_05_10[mnrega_elex_raj_05_10$reservation_2005 %in% c("ST", "STW") | 
                                                              mnrega_elex_raj_05_10$reservation_2010 %in% c("ST", "STW"), ]))

# Tidy and Glance
model_tidies <- map(models, tidy)
model_glances <- map(models, glance)

# Filter clean_models to only include the specified models
selected_models <- models[names(models) %in% selected_model_names]

stargazer(selected_models,
          type = "latex", 
          title = "Effects of Reservations on the Number of Completed MNREGA Projects, 2011-2014. (ST Only.)",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Traditional Water"),
          add.lines = list(c("Covariates", rep("No", 5))),
          omit.stat = c("rsq", "ser", "f"),
          single.row = FALSE,
          digits = 2,
          header = TRUE,
          label = "main_mnrega_phase_st",
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          no.space = TRUE,
          star.cutoffs = NULL,
          report = "vcs",
          notes = c("All - Total completed or ongoing MNREGA projects (2011--2014);", 
                    "Rural Roads: Number of projects to improve connectivity and roads (2011--2014);",
                    "Sanitation:  Number of projects to improve sanitation facilities  (2011--2014);",
                    "Water Conservation: Number of projects to improve water conservation (2011--2014);",
                    "Trad. Water: Number of projects to maintain traditional water bodies (2011--2014)."),
          notes.align = "l",
          out = here("tabs/mnrega_raj_05_10_main_st.tex"))

## Limit to Strict

# Apply the model fitting function across all specified column groups
models <- set_names(mod_cols, mod_cols) %>% 
     map(~ fit_model_for_group(.x, mnrega_elex_raj_05_10_strict))

# Tidy and Glance
model_tidies <- map(models, tidy)
model_glances <- map(models, glance)

# Filter clean_models to only include the specified models
selected_models <- models[names(models) %in% selected_model_names]

stargazer(selected_models,
          type = "latex", 
          title = "Effects of Reservations on the Number of Completed MNREGA Projects, 2011-2014. (Strict Matching.)",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Traditional Water"),
          add.lines = list(c("Covariates", rep("No", 5))),
          omit.stat = c("rsq", "ser", "f"),
          single.row = FALSE,
          digits = 2,
          header = TRUE,
          label = "main_mnrega_strict",
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          no.space = TRUE,
          star.cutoffs = NULL,
          report = "vcs",
          notes = c("All - Total completed or ongoing MNREGA projects (2011--2014);", 
                    "Rural Roads: Number of projects to improve connectivity and roads (2011--2014);",
                    "Sanitation:  Number of projects to improve sanitation facilities  (2011--2014);",
                    "Water Conservation: Number of projects to improve water conservation (2011--2014);",
                    "Trad. Water: Number of projects to maintain traditional water bodies (2011--2014)."),
          notes.align = "l",
          out = here("tabs/mnrega_raj_05_10_main_strict.tex"))

### Extreme

# Model Names
model_names <- paste0("lm_", column_groups)
mod_cols <- paste0(column_groups, "_tot_11_23")

fit_model_for_group <- function(column_name, data) {
     sufficient_data <- complete.cases(data[[column_name]], data$female_res_2005, data$female_res_2010)
     if (sum(sufficient_data) > 0) {
          lm(as.formula(paste(column_name, "~ case")), data = data)
     } else {
          NULL  # Indicate failure to fit the model due to insufficient data
     }
}

# Apply the model fitting function across all specified column groups
models <- set_names(mod_cols, mod_cols) %>% 
     map(~ fit_model_for_group(.x, mnrega_elex_raj_05_20[mnrega_elex_raj_05_20$case == '1_1_1_1' | 
                                                              mnrega_elex_raj_05_20$case == '0_0_0_0', ]))

# Tidy and Glance
model_tidies <- map(models, tidy)
model_glances <- map(models, glance)

# Number of projects: All Rural Roads Sanitation Water Conservation Traditional Water
selected_model_names <- paste0(c("total_comp_project",
                                 "connectivity_comp_project",
                                 "childcare_comp_project",
                                 "sanitation_comp_project",
                                 "water_conserve_comp_project",
                                 "water_trad_comp_project"), "_tot_11_23")

# Filter clean_models to only include the specified models
selected_models <- models[names(models) %in% selected_model_names]

stargazer(selected_models,
          type = "latex", 
          title = "Effects of Reservations on the Number of Completed MNREGA Projects, 2011-2023.",
          covariate.labels = c("T-T-T-T", "Constant"),
          column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Traditional Water"),
          add.lines = list(c("Covariates", rep("No", 5))),
          omit.stat = c("rsq", "ser", "f"),
          single.row = FALSE,
          digits = 2,
          header = TRUE,
          label = "main_mnrega_2011_2023",
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          no.space = TRUE,
          star.cutoffs = NULL,
          report = "vcs",
          notes = c("All - Total completed or ongoing MNREGA projects (2011--2023);", 
                    "Rural Roads: Number of projects to improve connectivity and roads (2011--2023);",
                    "Sanitation:  Number of projects to improve sanitation facilities  (2011--2023);",
                    "Water Conservation: Number of projects to improve water conservation (2011--2023);",
                    "Trad. Water: Number of projects to maintain traditional water bodies (2011--2023)."),
          notes.align = "l",
          out = here("tabs/mnrega_raj_05_20_main_extreme.tex"))


randomized_inference <- function(fit_model_fn, column_name, data, num_simulations = 1000) {
     # Fit the original model
     original_model <- fit_model_fn(column_name, data)
     if (is.null(original_model)) return(NA)
     
     original_coeff <- coef(summary(original_model))["case1_1_1_1", "Estimate"]
     
     # Initialize a vector to store the simulated coefficients
     simulated_coeffs <- numeric(num_simulations)
     
     # Perform randomization inference
     for (i in 1:num_simulations) {
          # Shuffle the treatment assignment
          data$case <- sample(data$case)
          
          # Fit the model on the randomized data
          randomized_model <- fit_model_fn(column_name, data)
          if (!is.null(randomized_model)) {
               simulated_coeffs[i] <- coef(summary(randomized_model))["case1_1_1_1", "Estimate"]
          }
     }
     
     # Calculate the p-value
     p_value <- mean(abs(simulated_coeffs) >= abs(original_coeff))
     
     return(p_value)
}

p_values <- set_names(mod_cols, mod_cols) %>%
     map(~ randomized_inference(fit_model_for_group, .x, 
                                mnrega_elex_raj_05_20[
                                     mnrega_elex_raj_05_20$case == '1_1_1_1' | 
                                          mnrega_elex_raj_05_20$case == '0_0_0_0', ]))

