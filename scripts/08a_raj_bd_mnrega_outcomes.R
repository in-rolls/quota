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

mnrega_elex_raj_05_10 <- read_parquet(here("data/raj/mnrega_elex_raj_05_10.parquet"))

# Outcomes
column_groups <- c("number_of_jobcards_issued", 
                   "registered_workers_total_workers",
                   "registered_workers_women")

mnrega_elex_raj_05_10 <- mnrega_elex_raj_05_10 %>%
     mutate(
          map_dfc(set_names(column_groups, paste0(column_groups, "_tot_11_14")), 
                  ~ {
                       col_pattern <- paste0(.x, "_201[1-4]$")
                       new_col <- rowSums(select(mnrega_elex_raj_05_10, matches(col_pattern)), na.rm = TRUE)
                       return(new_col)
                  })
     )

# Model Names
model_names <- paste0("lm_", column_groups)
mod_cols <- paste0(column_groups, "_tot_11_14")

models <- set_names(mod_cols, mod_cols) %>% 
     map(~ lm(as.formula(paste(.x, "~ female_res_2005 + female_res_2010")), data = mnrega_elex_raj_05_10))

# Tidy and Glance/Editor view
model_tidies  <- map(models, tidy)
model_glances <- map(models, glance)

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
             data = mnrega_elex_raj_05_10)
     })

# Tidy and Glance/Editor view
model_tidies  <- map(models, tidy)
model_glances <- map(models, glance)

# B/D Outcomes
selected_log_model_names <- paste0(c("number_of_jobcards_issued", 
                                     "registered_workers_total_workers",
                                     "registered_workers_women"), "_tot_11_14")

selected_log_models <- models[names(models) %in% selected_log_model_names]
# Tidy and Glance
map(models, tidy)
map(models, glance)

custom_stargazer(list(selected_models, selected_log_models),
                 title = "Effects of Reservations on Demand for Work and Women Employment via MNREGA, 2011--2014",
                 covariate.labels = c("2005", "2010", "Constant"),
                 column.labels = c("Tot. Jobcards", "Reg. Workers", "Reg. Women",
                                   "Log Tot. Jobcards", "Log Reg. Workers", "Log Reg. Women"),
                 add.lines = list(c("Covariates", rep("No", 6))),
                 label = "mnrega_main_raj_bose",
                 notes = c("The outcomes are from administrative data for MNREGA for years 2011--2014. 
                           The outcomes are: 
                     (i) The number of Jobcards; 
                     (ii) The number of registered workers;
                     (iii) The number of registered workers who are women."),
                 out = "tabs/mnrega_raj_05_10_main_bose.tex",
                 float.env = "sidewaystable")
