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
ay   <- read_csv(here("data/shrug/shrug-antyodaya-csv/antyodaya_shrid.csv"))
secc <- read_csv(here("data/shrug/shrug-secc-mord-rural-csv/secc_rural_shrid.csv"))

raj_elex_ay <- raj_elex_shrug %>% 
     inner_join(ay, by = "shrid2") %>%
     inner_join(secc, by = "shrid2")

# interactions

other_var <- list(
     "total_female_child_age_bw_0_6" = "Female Children",
     "total_hhd_having_bpl_cards" = "BPL Cards",
     "total_no_of_eligible_beneficiari" = "Maternity Benefit",
     "wall_mat_solid_share" = "Solid Wall",
     "roof_mat_solid_share" = "Solid Roof",
     "two_crop_acre_sum" = "Irrigation"
     )

# Let's create a GP Level dataset
raj_elex_ay_total <- raj_elex_ay %>%
     group_by(key) %>%
     summarise(female_res_2005 = first(female_res_2005),
               female_res_2010 = first(female_res_2010), 
               across(
                    .cols = all_of(names(other_var)),
                    .fns = sum,
                    .names = "{.col}"
               )
          )

# Model Names
model_names <- paste0("lm_", names(other_var))

fit_model_for_group <- function(column_name, data) {
     sufficient_data <- complete.cases(data[[column_name]], data$female_res_2005, data$female_res_2010)
     if (sum(sufficient_data) > 0) {
          lm(as.formula(paste(column_name, "~ female_res_2005 + female_res_2010")), data = data)
     } else {
          NULL  # Indicate failure to fit the model due to insufficient data
     }
}

# Apply the model fitting function across all specified column groups
models <- set_names(names(other_var), names(other_var)) %>% 
     map(~ fit_model_for_group(.x, raj_elex_ay_total))

# Tidy and Glance
model_tidies <- map(models, tidy)
model_glances <- map(models, glance)

stargazer(models,
          type = "latex", 
          title = "Effects of Reservations on Other Short- and Long-term Outcomes",
          model.names = FALSE,
          covariate.labels = c("2005", "2010", "Constant"),
          column.labels = unlist(unname(other_var)),
          add.lines = list(c("Covariates", rep("No", length(other_var)))),
          omit.stat = c("rsq", "ser", "f"),
          digits = 2,
          font.size = "scriptsize",
          dep.var.caption = "",
          star.cutoffs = NULL,
          report = "vcs",
          single.row = FALSE,
          notes.align = "l",
          label = "raj_shrug_other_05_10",
          dep.var.labels.include = FALSE,
          notes = c("Number of female children (0-6 years);",
                    "Number of Households having BPL ration cards;",
                    "Number of eligible beneficiaries under Pradhan Mantri Matru Vandana Yojana",
                    "Share of households with solid wall - wood/stone/sheets/burnt brick/concrete;",
                    "Share of households with solid roof - stone/slate sheets/concrete burned brick;",
                    "Total land with assured irrigation for two crops"),
          out = "tabs/shrug_raj_05_10_other.tex")
