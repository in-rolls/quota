# Load libs
library(arrow)
library(broom)
library(dplyr)
library(here)
library(purrr)
library(readr)
library(stargazer)
library(tidyr)
library(ggplot2)
library(fixest)
library(modelsummary)
library(pbapply)
library(janitor)

# Source utils
source(here("scripts/00_utils.R"))

# Read dat
mnrega_elex_raj_05_20 <- read_parquet(here("data/raj/mnrega_elex_raj_05_20.parquet")) %>% 
     dplyr::rename(match_name = match_name.x)
mnrega_raj_sa <- read_csv(here("data/mnrega/final_audit_results_translit.csv")) %>%
     filter(state == 'RAJASTHAN')

# Create match and fix
mnrega_raj_sa <- mnrega_raj_sa %>%
     mutate(match_name = normalize_string(gsub(" ", "", paste0(district, block, plain_transliteration)))) %>%
     filter(!is.na(panchayat))

mnrega_raj_sa_elex <- mnrega_elex_raj_05_20 %>% 
     select(female_res_2005, female_res_2010, female_res_2015, female_res_2020, match_name) %>%
     inner_join(mnrega_raj_sa, by = c("match_name" = "match_name"))

# Recode
mnrega_raj_sa_elex <- mnrega_raj_sa_elex %>%
     clean_names() %>%
     mutate(
          diff_works = total_works - works_verified > 0,
          diff_households = total_households - households_verified > 0,
          job_cards_with_people_numeric = case_when(
               job_cards_with_people == "Less than 25%"      ~ 0,
               job_cards_with_people == "Between 25% and 50%"  ~ .33,
               job_cards_with_people == "Between 50% and 75%"  ~ .66,
               job_cards_with_people == "Greater than 75%"     ~ 1,
               TRUE                                          ~ NA_real_
          ),
          adequate_manpower = case_when(
               gpc_personnel_training_is_there_adequate_manpower_to_implement_mgnrega_at_panchayat_level == "Less than 25%"      ~ 0,
               gpc_personnel_training_is_there_adequate_manpower_to_implement_mgnrega_at_panchayat_level == "Between 25% and 50%"  ~ .33,
               gpc_personnel_training_is_there_adequate_manpower_to_implement_mgnrega_at_panchayat_level == "Between 50% and 75%"  ~ .66,
               gpc_personnel_training_is_there_adequate_manpower_to_implement_mgnrega_at_panchayat_level == "Greater than 75%"     ~ 1,
               TRUE                                          ~ NA_real_
          ),
          rozgar_diwas = case_when(
               gpc_mgnregs_administration_is_rozgar_diwas_was_held_every_month == "Yes" ~ 1,
               gpc_mgnregs_administration_is_rozgar_diwas_was_held_every_month == "No" ~ 0,
               TRUE ~ NA_real_
          )
     )

# Run mods
dependent_vars <- c("diff_works", "diff_households", "job_cards_with_people_numeric",
                    "adequate_manpower", "rozgar_diwas")

models <- map(dependent_vars, function(dv) {
     feols(as.formula(paste0(dv, " ~ female_res_2005 + female_res_2010 + female_res_2015 + female_res_2020")),
           data = mnrega_raj_sa_elex,
           cluster = ~ match_name)
})
names(models) <- c("Works", "Households", "Job Cards", "Adq. Manpower", "Rozgar Diwas")


# Covar. label mapper
label_mapping <- c(
     "female_res_2005" = "2005",
     "female_res_2010" = "2010",
     "female_res_2015" = "2015",
     "female_res_2020" = "2020"
     
)

coef_names <- names(coef(models[[1]]))
cov_names <- coef_names[coef_names != "(Intercept)"]
cov_labels <- label_mapping[cov_names]

# Write
modelsummary(
     models,
     coef_map = label_mapping,
     title = "Effects of Reservations on the Audit Performance.",
     label = "main_mnrega_audit",
     gof_omit  = "AIC|BIC|RMSE|^R2$",
     notes = "The outcomes are from MNREGA social audits from 2020--2024.",
     notes_append  = TRUE,
     notes_align   = "l",
     latex_env = "talltblr",
     keepxp = FALSE,
     add_rows = NULL,
     stars = TRUE,
     output = here("tabs/mnrega_raj_05_20_audit.tex")
)
