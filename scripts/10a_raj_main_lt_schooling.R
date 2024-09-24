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
raj_elex_shrug <- read_parquet(here("data/raj/shrug_lgd_raj_elex_05_10.parquet"))
ay <- read_csv(here("data/shrug/shrug-antyodaya-csv/antyodaya_shrid.csv"))

raj_elex_ay <- raj_elex_shrug %>% 
     inner_join(ay, by = "shrid2")

## Antyodya
# Underweight, Truant, , Registered, SC/ST enrolled
# interactions

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
models <- set_names(names(school_var), names(school_var)) %>% 
     map(~ lm(as.formula(paste(.x, "~ female_res_2005 + female_res_2010")), data = raj_elex_ay_total))

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
