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

# SHRUG: Pri.Health Comm.Health Maternal Health Anemic Pregnant Non-stunted child

health_var <- list(
     "phc" = "Primary Health Ctr.",
     "chc" = "Community Health Ctr.",
     "availability_of_mother_child_hea" = "Maternal Health Ctr.",
     "total_anemic_pregnant_women" = "Anemic Preg. Women",
     "total_childs_categorized_non_stu" = "Non-Stunted Children"
     )

# Let's create a GP Level dataset
up_elex_ay_total <- up_elex_ay %>%
     group_by(key) %>%
     summarise(female_res_2005 = first(female_res_2005),
               female_res_2010 = first(female_res_2010), 
               across(
                    .cols = all_of(names(health_var)),
                    .fns = sum,
                    .names = "{.col}"
               )
          )

# Model Names
model_names <- paste0("lm_", names(health_var))
models <- set_names(names(health_var), names(health_var)) %>% 
     map(~ lm(as.formula(paste(.x, "~ female_res_2005 + female_res_2010")), data = up_elex_ay_total))

# Tidy and Glance
model_tidies <- map(models, tidy)
model_glances <- map(models, glance)

custom_stargazer(models,
          title = "Effects of Reservations on Long-term Outcomes Concerning Health",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = unlist(unname(health_var)),
          add.lines = list(c("Covariates", rep("No", length(health_var)))),
          label = "up_shrug_health_05_10",
          notes = c("Is there a primary school available?",
                    "Is there a middle school available?",
                    "The number of students enrolled in primary school per 1000 people;",
                    "Binary variable to indicate if there is a mid-day meal facility for students in school."),
          out = here("tabs/shrug_up_05_10_health.tex"))
