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
children_var <- list(
     "total_childs_aged_0_to_3_years_i" = "Immunized",
     "total_underweight_child_age_unde" = "Underweight",
     "no_of_children_not_attending_sch" = "Truant",
     "total_childs_aged_0_to_3_years_r" = "Anganwadi",
     "total_minority_children_getting_" = "SC/ST"
)

# Let's create a GP Level dataset
raj_elex_ay_total <- raj_elex_ay %>%
     group_by(key) %>%
     summarise(female_res_2005 = first(female_res_2005),  # Keep shrid2-level variable
               female_res_2010 = first(female_res_2010), 
               across(
                    .cols = all_of(names(children_var)),
                    .fns = sum,
                    .names = "{.col}"
               )
     )

# Model Names
model_names <- paste0("lm_", names(children_var))

fit_model_for_group <- function(column_name, data) {
     sufficient_data <- complete.cases(data[[column_name]], data$female_res_2005, data$female_res_2010)
     if (sum(sufficient_data) > 0) {
          lm(as.formula(paste(column_name, "~ female_res_2005 + female_res_2010")), data = data)
     } else {
          NULL  # Indicate failure to fit the model due to insufficient data
     }
}

# Apply the model fitting function across all specified column groups
models <- set_names(names(children_var), names(children_var)) %>% 
     map(~ lm(as.formula(paste(.x, "~ female_res_2005 + female_res_2010")), data = raj_elex_ay_total))

# Tidy and Glance
model_tidies <- map(models, tidy)
model_glances <- map(models, glance)

custom_stargazer(models,
          title = "Effects of Reservations on Long-term Outcomes Concerning Children",
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = unlist(unname(children_var)),
          add.lines = list(c("Covariates", rep("No", length(children_var)))),
          label = "raj_shrug_children_05_10",
          notes = "The outcomes are from the Mission Antyodya Survey from 2019. 
                   The village level outcomes have been aggregated to a GP level. The outcomes are:
                    (i) Immunized: The number of 0--3 year old children who are immunized;
                    (ii) Underweight: The number of underweight children under 6;
                    (iii) Truant: The number of children not attending school;
                    (iv) Anganwadi: The number of children under 6 registered in child care centers;
                    (v) SC/ST: The number of SC/ST/OBC/minority children receiving school subsidies.",
          out = here("tabs/shrug_raj_05_10_children.tex"))