# Load libs
library(here)
library(arrow)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(fixest)
library(estimatr)
library(kableExtra)
library(broom)
library(stargazer)

# Source utils
source(here("scripts/00_utils.R"))

# Load dat
raj_elex_shrug <- read_parquet(here("data/raj/shrug_lgd_raj_elex_05_10.parquet"))
ay <- read_csv(here("data/shrug/shrug-antyodaya-csv/antyodaya_shrid.csv"))

raj_elex_ay <- raj_elex_shrug %>% 
     inner_join(ay, by = "shrid2")

## Antyodya
# Underweight, Truant, , Registered, SC/ST enrolled
# interactions

# SHRUG: Underweight Truant Immunized Registered SC/ST enrolled
# SHRUG: Pri.School Mid.School Students School Meals
# SHRUG: Pri.Health Comm.Health Maternal Health Anemic Pregnant Non-stunted child

school_var <- list(
     "availability_of_primary_school" = "Primary School",
     "availability_of_middle_school" = "Middle School",
     "total_primary_school_students" = "Tot. Pri. School Students",
     "availability_of_mid_day_meal_sch" = "Mid-day Meal"
     )

# Let's create a GP Level dataset
raj_elex_ay_total <- raj_elex_ay %>%
     group_by(key) %>%
     summarise(female_res_2005 = first(female_res_2005),  # Keep shrid2-level variable
               female_res_2010 = first(female_res_2010), 
               across(
                    .cols = all_of(names(school_var)),
                    .fns = sum,
                    .names = "{.col}"
               )
          )

# Model Names
model_names <- paste0("lm_", names(school_var))

fit_model_for_group <- function(column_name, data) {
     sufficient_data <- complete.cases(data[[column_name]], data$female_res_2005, data$female_res_2010)
     if (sum(sufficient_data) > 0) {
          lm(as.formula(paste(column_name, "~ female_res_2005 + female_res_2010")), data = data)
     } else {
          NULL  # Indicate failure to fit the model due to insufficient data
     }
}

# Apply the model fitting function across all specified column groups
models <- set_names(names(school_var), names(school_var)) %>% 
     map(~ fit_model_for_group(.x, raj_elex_ay_total))

# Tidy and Glance
model_tidies <- map(models, tidy)
model_glances <- map(models, glance)

custom_stargazer(models,
          title = "Effects of Reservations on Long-term Outcomes Concerning Schooling",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = unlist(unname(school_var)),
          add.lines = list(c("Covariates", rep("No", length(school_var)))),
          label = "raj_shrug_schooling_05_10",
          notes = c("Is there a primary school or not;",
                    "Is there is a middle school available;",
                    "The number of students enrolled in primary school per 1000 people;",
                    "Is there is a mid-day meal facility for students in school."),
          out = here("tabs/shrug_raj_05_10_schooling.tex"))
