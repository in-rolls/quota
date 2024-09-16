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

children_var <- list(
     "total_childs_aged_0_to_3_years_i" = "Immunized",
     "total_underweight_child_age_unde" = "Underweight",
     "no_of_children_not_attending_sch" = "Truant",
     "total_childs_aged_0_to_3_years_r" = "Registered",
     "total_minority_children_getting_" = "SC/ST enrolled"
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
     map(~ fit_model_for_group(.x, raj_elex_ay_total))

# Tidy and Glance
model_tidies <- map(models, tidy)
model_glances <- map(models, glance)

stargazer(models,
          type = "latex", 
          title = "Effects of Reservations on Long-term Outcomes Concerning Children",
          model.names = FALSE,
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = unlist(unname(children_var)),
          add.lines = list(c("Covariates", rep("No", length(children_var)))),
          omit.stat = c("rsq", "ser", "f"),
          digits = 2,
          dep.var.caption = "",
          star.cutoffs = NULL,
          report = "vcs",
          single.row = FALSE,
          label = "raj_shrug_children_05_10",
          dep.var.labels.include = FALSE,
          notes.align = "l",
          notes = c("The number of children under the age of 6 years who are underweight per hundred total children;",
                    "Number of children not attending school per hundred total children;",
                    "Number of children aged 0-3 years immunized per hundred total children;",
                    "Number of children aged 0-6 years registered in Aanganwadis per hundred total children;",
                    "Number of SC/ST/OBC/minoritychildren attending school."),
          out = here("tabs/shrug_raj_05_10_children.tex"))