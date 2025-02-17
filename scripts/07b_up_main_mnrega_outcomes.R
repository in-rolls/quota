# Load libs
library(arrow)
library(purrr)
library(estimatr)
library(stargazer)
library(dplyr)
library(readr)
library(broom)
library(ggthemes)
library(here)

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

main_outcome_caption <- "They are: 
(i) All: The total number of completed projects, including areas not listed here like Fisheries, Drought Proofing, etc.;
(ii) Rural Roads: The number of projects to improve connectivity and roads;
(iii) Sanitation: The number of projects to improve sanitation facilities;
(iv) Water Conservation: The number of projects to improve water conservation;
(v) Trad. Water: The number of projects to maintain traditional water bodies."

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
          title = "Effects of Reservations on the Number of Completed MNREGA Projects, 2011--2014 (UP)",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Trad. Water"),
          add.lines = list(c("Covariates", rep("No", 5))),
          label = "main_mnrega_up_2005_2010",
          notes = paste(cons_term, 
                        "The outcomes are from MNREGA administrative data for years 2011--2014.", 
                        main_outcome_caption),
          out = "tabs/mnrega_up_05_10_main.tex")

# Bose and Das Districts

# Apply the model fitting function across all specified column groups
models <- set_names(mod_cols, mod_cols) %>% 
     map(~ fit_model_for_group(.x, mnrega_elex_up_05_10[mnrega_elex_up_05_10$phase_1_bose_2005 == 1 | mnrega_elex_up_05_10$phase_2_bose_2005 == 1, ]))

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
          title = "Effects of Reservations on the Number of Completed MNREGA Projects, 2011--2014 (Bose and Das Districts; UP)",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = c("All", "Rural Roads", "Sanitation", "Water Conservation", "Trad. Water"),
          add.lines = list(c("Covariates", rep("No", 5))),
          label = "mnrega_up_bd_districts_05_10",
          notes = paste(cons_term,
                        "The outcomes are from MNREGA administrative data for years 2011--2014.", 
                        main_outcome_caption),
          out = here("tabs/mnrega_up_05_10_main_bd_districts.tex"))
