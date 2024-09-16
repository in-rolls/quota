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

# SHRUG: Pri.Health Comm.Health Maternal Health Anemic Pregnant Non-stunted child

health_var <- list(
     "phc" = "Primary Health Ctr.",
     "chc" = "Community Health Ctr.",
     "availability_of_mother_child_hea" = "Maternal Health Ctr.",
     "total_anemic_pregnant_women" = "Anemic Preg. Women",
     "total_childs_categorized_non_stu" = "Non-Stunted Children"
     )

# Let's create a GP Level dataset
raj_elex_ay_total <- raj_elex_ay %>%
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

fit_model_for_group <- function(column_name, data) {
     sufficient_data <- complete.cases(data[[column_name]], data$female_res_2005, data$female_res_2010)
     if (sum(sufficient_data) > 0) {
          lm(as.formula(paste(column_name, "~ female_res_2005 + female_res_2010")), data = data)
     } else {
          NULL  # Indicate failure to fit the model due to insufficient data
     }
}

# Apply the model fitting function across all specified column groups
models <- set_names(names(health_var), names(health_var)) %>% 
     map(~ fit_model_for_group(.x, raj_elex_ay_total))

# Tidy and Glance
model_tidies <- map(models, tidy)
model_glances <- map(models, glance)

stargazer(models,
          type = "latex", 
          title = "Effects of Reservations on Long-term Outcomes Concerning Health",
          model.names = FALSE,
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = unlist(unname(health_var)),
          add.lines = list(c("Covariates", rep("No", length(health_var)))),
          omit.stat = c("rsq", "ser", "f"),
          digits = 2,
          dep.var.caption = "",
          star.cutoffs = NULL,
          report = "vcs",
          single.row = FALSE,
          label = "raj_shrug_health_05_10",
          dep.var.labels.include = FALSE,
          notes.align = "l",
          font.size = "scriptsize",
          notes = c("Is there a primary school available?",
                    "Is there a middle school available?",
                    "The number of students enrolled in primary school per 1000 people;",
                    "Binary variable to indicate if there is a mid-day meal facility for students in school."),
          out = here("tabs/shrug_raj_05_10_health.tex"))
