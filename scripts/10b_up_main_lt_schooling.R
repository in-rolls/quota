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
up_elex_shrug <- read_parquet(here("data/up/shrug_lgd_up_elex_05_10.parquet"))
ay <- read_csv(here("data/shrug/shrug-antyodaya-csv/antyodaya_shrid.csv"))

up_elex_ay <- up_elex_shrug %>% 
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
up_elex_ay_total <- up_elex_ay %>%
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
     map(~ lm(as.formula(paste(.x, "~ female_res_2005 + female_res_2010")), data = up_elex_ay_total))

# Tidy and Glance
model_tidies <- map(models, tidy)
model_glances <- map(models, glance)

custom_stargazer(models,
          title = "Effects of Reservations on Long-term Outcomes Concerning Schooling (UP)",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = unlist(unname(school_var)),
          add.lines = list(c("Covariates", rep("No", length(school_var)))),
          label = "up_shrug_schooling_05_10",
          notes = paste(cons_term, "The outcomes are from the Mission Antyodya Facilities Survey from 2019. 
                     The village level outcomes have been aggregated to a GP level. 
                     The outcomes are:
                         (i) Primary School: Is there a primary school?
                         (ii) Middle School: Is there a middle school?
                         (iii) Tot. Pri. School Students: The number of students enrolled in primary schools per 1000 people;
                         (iv) Mid-day Meal: Is there is a mid-day meal facility for students in school?"),
          out = here("tabs/shrug_up_05_10_schooling.tex"))
